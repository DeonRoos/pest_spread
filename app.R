library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(shinyjs)
library(pool)
library(DBI)
library(RPostgres)
library(httr)
library(jsonlite)

# load app-local environment
readRenviron(".Renviron")

# ---- field config ----
FIELD_POLY <- st_sfc(
  st_polygon(list(rbind(
    c(-2.1721658537901867, 57.294896013118304),
    c(-2.1654184492378130, 57.295713458154240),
    c(-2.1649325477643195, 57.295462856497170),
    c(-2.1658491346347730, 57.294227723390640),
    c(-2.1729609652922670, 57.293410245342640),
    c(-2.1721658537901867, 57.294896013118304)
  ))), crs = 4326
)

GRID_SIZE    <- 50
MAX_SURVEYS  <- 3
USERS        <- c("Deon Roos")
ACC_THRESHOLD <- 10

# ---- TEST MODE -------------------------------------------------
# TEST_MODE=1 to test data entry, 0 if not
SURVEY_TABLE <- if (Sys.getenv("TEST_MODE") == "1") "surveys_test" else "surveys"

DB_CONFIG <- list(
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
WEATHER_API_KEY <- Sys.getenv("OPENWEATHER_API")

# grid
create_grid <- function(poly, size_m) {
  cent <- st_coordinates(st_centroid(poly))
  utm_zone <- floor((cent[1] + 180) / 6) + 1
  crs_utm  <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
  poly_utm <- st_transform(poly, crs_utm)
  grid     <- st_make_grid(poly_utm, cellsize = size_m, square = TRUE) |> st_as_sf()
  grid     <- grid[st_intersects(grid, poly_utm, sparse = FALSE), ]
  st_transform(grid, 4326) |> mutate(site_id = sprintf("S%04d", row_number()))
}

grid_sf <- create_grid(FIELD_POLY, GRID_SIZE)

# database
pool <- dbPool(
  drv      = Postgres(),
  dbname   = DB_CONFIG$dbname,
  host     = DB_CONFIG$host,
  port     = DB_CONFIG$port,
  user     = DB_CONFIG$user,
  password = DB_CONFIG$password,
  sslmode  = "require"
)
onStop(function() poolClose(pool))

# create table
schema_sql <- sprintf(
  "CREATE TABLE IF NOT EXISTS %s (
     id            SERIAL PRIMARY KEY,
     site_id       TEXT,
     datetime      TIMESTAMPTZ,
     temperature   NUMERIC,
     rain          NUMERIC,
     survey_number INTEGER,
     detected      INTEGER,
     observer      TEXT,
     lat           DOUBLE PRECISION,
     lon           DOUBLE PRECISION,
     accuracy      NUMERIC
   );", SURVEY_TABLE)
dbExecute(pool, schema_sql)

# make sure required columns exist
for (col in c("detected", "accuracy")) {
  dtype <- ifelse(col == "accuracy", "NUMERIC", "INTEGER")
  alter <- sprintf("ALTER TABLE %s ADD COLUMN IF NOT EXISTS %s %s;", SURVEY_TABLE, col, dtype)
  dbExecute(pool, alter)
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Multi-season Occupancy – Field Data Entry"),
  
  # JavaScript helper: pick best fix within 4s
  tags$head(tags$script(HTML(sprintf("(function(){
      let watchID=null;
      function bestFix(thr,cb){
        let best=null;
        if(!navigator.geolocation){cb(null);return;}
        watchID=navigator.geolocation.watchPosition(function(p){
          if(!best||p.coords.accuracy<best.coords.accuracy) best=p;
          if(best.coords.accuracy<=thr) finish();
        },function(){finish();},{enableHighAccuracy:true,maximumAge:0});
        setTimeout(finish,4000);
        function finish(){
          if(watchID!==null){navigator.geolocation.clearWatch(watchID);watchID=null;}
          cb(best);
        }
      }
      $(document).on('click','#getloc',function(){
        bestFix(%d,function(pos){
          if(pos){
            Shiny.setInputValue('geolocation',{
              lat:pos.coords.latitude,
              lon:pos.coords.longitude,
              accuracy:pos.coords.accuracy,
              stamp:Date.now()
            });
          } else alert('Unable to obtain GPS position');
        });
      });
    })();", ACC_THRESHOLD)))),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("observer","Observer",choices = USERS),
      selectInput("site","Site (50×50 m)", choices = NULL),
      numericInput("survey_number","Survey #", value = NA, min = 1, max = MAX_SURVEYS),
      radioButtons("detected","Species detected?", choices = c("No" = 0, "Yes" = 1), inline = TRUE),
      numericInput("temperature","Temperature (°C)", value = NA),
      numericInput("rain","Rain (mm, past 1 h)", value = NA),
      actionButton("getloc","Get location", class = "btn-info"),
      htmlOutput("accInfo"),
      br(),
      actionButton("submit","Submit", class = "btn-success"),
      br(),
      verbatimTextOutput("status")
    ),
    mainPanel(leafletOutput("map", height = 650))
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  observe(updateSelectInput(session, "site", choices = grid_sf$site_id))
  
  # initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = FIELD_POLY, color = "red", weight = 2, fillOpacity = 0) %>%
      addPolygons(data = grid_sf, layerId = ~site_id, color = "blue", weight = 1, fillOpacity = 0) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # update on geolocation
  observeEvent(input$geolocation, {
    req(input$geolocation$lat, input$geolocation$lon)
    lat <- input$geolocation$lat; lon <- input$geolocation$lon
    
    # weather autofill (not implemented yet)
    if (WEATHER_API_KEY != "") {
      wx_url <- sprintf("https://api.openweathermap.org/data/2.5/weather?lat=%s&lon=%s&units=metric&appid=%s", lat, lon, WEATHER_API_KEY)
      res <- try(GET(wx_url), silent = TRUE)
      if (!inherits(res,"try-error") && status_code(res)==200) {
        wx <- fromJSON(content(res,"text",encoding="UTF-8"))
        updateNumericInput(session,"temperature", value = wx$main$temp)
        updateNumericInput(session,"rain", value = ifelse(is.null(wx$rain$`1h`),0,wx$rain$`1h`))
      }
    }
    
    # nearest site
    idx <- st_nearest_feature(st_sfc(st_point(c(lon,lat)), crs = 4326), grid_sf)
    updateSelectInput(session, "site", selected = grid_sf$site_id[idx])
    
    leafletProxy("map") %>% clearGroup("loc") %>% addCircles(lng = lon, lat = lat, radius = 2, group = "loc", label = "You")
    
    # accuracy read‑out
    acc <- input$geolocation$accuracy
    col <- ifelse(acc <= ACC_THRESHOLD, "#28a745", "#fd7e14")
    output$accInfo <- renderUI(span(style = sprintf("color:%s;", col), sprintf("GPS accuracy: ±%.1f m", acc)))
  })
  
  # move marker when site picked manually
  observeEvent(input$site, {
    req(input$site)
    ctr <- st_coordinates(st_centroid(grid_sf[grid_sf$site_id == input$site,]))
    leafletProxy("map") %>% clearGroup("loc") %>% addCircles(lng = ctr[1], lat = ctr[2], radius = 2, group = "loc", label = "Selected site")
  })
  
  # validation + auto survey number
  observe({
    done <- if (!is.null(input$site) && input$site != "")
      dbGetQuery(pool, sprintf("SELECT COUNT(*) FROM %s WHERE site_id=$1", SURVEY_TABLE), params = list(input$site))[[1]] else 0
    if (!is.null(input$site) && input$site != "" && done < MAX_SURVEYS)
      updateNumericInput(session, "survey_number", value = done + 1)
    else updateNumericInput(session, "survey_number", value = NA)
    
    filled <- all(!is.null(input$observer), input$observer != "",
                  !is.null(input$site), input$site != "",
                  !is.na(input$survey_number), !is.na(input$detected),
                  !is.na(input$temperature), !is.na(input$rain))
    
    if (done >= MAX_SURVEYS || !filled)
      shinyjs::disable("submit")
    else
      shinyjs::enable("submit")
  })
  
  # highlight sites surveyed today
  observe({
    today <- Sys.Date()
    done_today <- dbGetQuery(pool, sprintf("SELECT site_id FROM %s WHERE date(datetime)=$1", SURVEY_TABLE), params = list(today))$site_id
    g <- grid_sf %>% mutate(surveyed = site_id %in% done_today)
    pal <- colorFactor(c("transparent","green"), domain = c(FALSE, TRUE))
    leafletProxy("map") %>% clearGroup("surveyed") %>% addPolygons(data = g, fillColor = ~pal(surveyed), fillOpacity = 0.3, stroke = FALSE, group = "surveyed")
  })
  
  # map click selects site
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$layerId))
      updateSelectInput(session, "site", selected = click$layerId)
  })
  
  # submit
  observeEvent(input$submit, {
    req(input$observer, input$site, input$survey_number, input$detected)
    loc <- input$geolocation
    dbExecute(pool,
              sprintf("INSERT INTO %s (site_id, datetime, temperature, rain, survey_number, detected, observer, lat, lon, accuracy) VALUES ($1, NOW(), $2, $3, $4, $5, $6, $7, $8, $9)", SURVEY_TABLE),
              params = list(input$site, input$temperature, input$rain, as.integer(input$survey_number), as.integer(input$detected), input$observer, loc$lat, loc$lon, loc$accuracy))
    output$status <- renderText(sprintf("✓ Saved %s at %s", input$site, format(Sys.time(), "%H:%M:%S")))
  })
}

# ---- RUN ----
shinyApp(ui, server)

