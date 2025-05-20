all_files <- list.files(".", recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
rsconnect::deployApp(
  appDir   = ".",
  appName  = "pest_spread",
  account  = "deonroos",
  appFiles = c(all_files, ".Renviron")
)

