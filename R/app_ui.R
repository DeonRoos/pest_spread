# R/app_ui.R
app_ui <- function() {
  shiny::fluidPage(
    shinyjs::useShinyjs(),                  # ← loads shinyjs JS helpers
    tags$script(src = "js/geolocation.js"), # ← your JS file
    sidebarLayout(
      sidebarPanel(
        actionButton("getloc", "Get my location", class = "btn-info"),
        dateInput("date", "Date", value = Sys.Date()),
        shinyTime::timeInput(               # default = “now”, user can edit
          "time", "Time",
          value   = lubridate::round_date(Sys.time(), "minute"),
          seconds = FALSE
        ),
        selectInput("survey_round", "Survey",
                    choices = setNames(1:3, paste("Round", 1:3))),
        selectInput("crop", "Crop type",
                    choices = c("wheat", "barley", "fallow", "pasture")),
        checkboxInput("detection", "Detection made?"),
        actionButton("submit", "Submit", class = "btn-primary")
      ),
      mainPanel(
        leaflet::leafletOutput("map", height = 400),
        DT::DTOutput("tbl")
      )
    )
  )
}
