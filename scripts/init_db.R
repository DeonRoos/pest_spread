# scripts/init_db.R -----------------------------------------------------------
# initialise an (empty) SQLite database for the Shiny app
# run with:  Rscript scripts/init_db.R

# 0. load packages ------------------------------------------------------------
if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE)) {
  stop("Please install the DBI and RSQLite packages first.")
}

library(DBI)
library(RSQLite)

# 1. ensure data/db/ exists ---------------------------------------------------
db_dir  <- file.path("data", "db")
dir.create(db_dir, recursive = TRUE, showWarnings = FALSE)

db_path <- file.path(db_dir, "pest.sqlite")

# 2. connect (this creates the file if it doesn't exist) ----------------------
con <- dbConnect(SQLite(), dbname = db_path)

# 3. OPTIONAL: create schema --------------------------------------------------
# You can comment these out if you truly want a *blank* database,
# but the app code expects the tables to exist.
dbExecute(con, "
  CREATE TABLE IF NOT EXISTS sites (
    site_id INTEGER PRIMARY KEY,
    lat     REAL,
    lon     REAL
  );
")

dbExecute(con, "
  CREATE TABLE IF NOT EXISTS surveys (
    survey_id     INTEGER PRIMARY KEY,
    site_id       INTEGER REFERENCES sites(site_id),
    survey_round  INTEGER,
    survey_ts     TEXT,
    detection     INTEGER,            -- 0 = FALSE, 1 = TRUE
    crop          TEXT
  );
")

# 4. clean up -----------------------------------------------------------------
dbDisconnect(con)

message("SQLite DB initialised at ", db_path)
