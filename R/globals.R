library(shiny)
library(shinyTime)
library(shinyvalidate)
library(shinyjs)
library(DT)
library(leaflet)
library(DBI)
library(pool)
library(RSQLite)
library(shinyjs)
library(lubridate)
library(geosphere)

source("R/utils_db.R")

db_pool <- local({
  .pool <- NULL 
  
  function() {
    if (is.null(.pool)) {
      cfg <- config::get(file = "config/config.yml")
      .pool <<- pool::dbPool(
        drv      = do.call(cfg$db$drv, list()),
        dbname   = cfg$db$dbname,
        host     = cfg$db$host,
        user     = cfg$db$user,
        password = cfg$db$password,
        minSize  = cfg$pool_min %||% 0,
        maxSize  = cfg$pool_max %||% 5
      )
    }
    .pool
  }
})
