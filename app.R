source("R/globals.R", local = FALSE)
source("R/app_ui.R", local = FALSE)
source("R/app_server.R", local = FALSE)
shiny::shinyApp(ui = app_ui(), server = app_server)
