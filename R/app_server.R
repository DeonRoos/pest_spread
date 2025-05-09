# R/app_server.R --------------------------------------------------------------
app_server <- function(input, output, session) {
  
  ## 1 ─ Connections & shared objects ────────────────────────────────────────
  con <- db_pool()                             # from utils_db.R
  iv  <- shinyvalidate::InputValidator$new()
  
  ## 2 ─ Input‑validation rules ──────────────────────────────────────────────
  iv$add_rule("date",         sv_required())
  iv$add_rule("survey_round", sv_required())
  iv$add_rule("time",         sv_required())
  iv$add_rule("crop",         sv_required())
  iv$add_rule("detection",    sv_required())
  iv$enable()
  
  ## 3 ─ Reactive helpers ────────────────────────────────────────────────────
  coords_ready <- reactive({
    !is.null(input$lat) && !is.null(input$lon)
  })
  
  ## Poll DB every 2 s so all sessions stay in sync
  all_surveys <- reactivePoll(
    2000, session,
    checkFunc = function() DBI::dbGetQuery(con, "SELECT MAX(survey_id) FROM surveys")[1,1],
    valueFunc = function() DBI::dbGetQuery(
      con, "SELECT s.survey_id, st.site_id, st.lat, st.lon,
                   s.survey_round, s.survey_ts, s.detection, s.crop
            FROM surveys s JOIN sites st USING (site_id)
            ORDER BY s.survey_ts DESC
            LIMIT 100")
  )
  
  ## 4 ─ UX helpers ──────────────────────────────────────────────────────────
  ## 4a ‑ Enable / disable the Submit button automatically
  observe({
    shinyjs::toggleState(
      id        = "submit",
      condition = iv$is_valid() && coords_ready()
    )
  })
  
  ## 4b ‑ When the user grabs their location, default Time to “now”
  observeEvent(input$getloc, {
    shinyTime::updateTimeInput(
      session, "time",
      value = lubridate::round_date(Sys.time(), "minute")
    )
  })
  
  ## 5 ─ Handle form submission ─────────────────────────────────────────────
  observeEvent(input$submit, {
    req(iv$is_valid(), coords_ready())
    
    ## 5a ‑ Build timestamp
    ts <- as.POSIXct(
      paste(input$date, format(input$time, "%H:%M")),
      tz = Sys.timezone()
    )
    
    ## 5b ‑ Insert row inside tryCatch for feedback
    tryCatch({
      site_id <- get_site_id(input$lat, input$lon, con)
      
      DBI::dbExecute(
        con,
        "INSERT INTO surveys
           (site_id, survey_round, survey_ts, detection, crop)
         VALUES ($1, $2, $3, $4, $5)",
        params = list(
          site_id,
          as.integer(input$survey_round),
          ts,
          input$detection,
          input$crop
        )
      )
      
      showNotification("✅ Observation saved", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message),
                       type = "error", duration = 8)
    })
    
    ## 5c ‑ Reset form for next entry
    updateDateInput(session,   "date",  value = Sys.Date())
    updateSelectInput(session, "survey_round", selected = 1)
    shinyTime::updateTimeInput(session, "time",
                               value = lubridate::round_date(Sys.time(), "minute"))
    updateSelectInput(session, "crop",  selected = "wheat")
    updateCheckboxInput(session,"detection", value = FALSE)
    shinyjs::reset("file")
  })
  
  ## 6 ─ Outputs ────────────────────────────────────────────────────────────
  output$tbl <- renderDT({
    DT::datatable(all_surveys(),
                  options = list(pageLength = 10,
                                 caption = htmltools::tags$caption(
                                   style = "caption-side: bottom; text-align: left;",
                                   "*Updates every ~2 seconds*")),
                  rownames = FALSE)
  })
  
  output$map <- leaflet::renderLeaflet({
    df <- all_surveys()
    leaflet::leaflet(df) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addCircleMarkers(~lon, ~lat,
                                layerId = ~site_id,
                                popup = ~paste0("Site ", site_id),
                                radius = 6)
  })
  
  ## 7 ─ Session‑end clean‑up ───────────────────────────────────────────────
  session$onSessionEnded(function() pool::poolReturn(con))
  
  ## 8 ─ Warn if geolocation denied ─────────────────────────────────────────
  observe({
    if (isFALSE(input$geo_allowed)) {
      showNotification(
        "Location blocked. Open the app in Chrome/Firefox and click “Allow” when prompted.",
        type = "error", duration = NULL
      )
    }
  })
}
