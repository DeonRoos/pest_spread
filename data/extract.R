library(DBI)
library(RPostgres)

SURVEY_TABLE <- if (Sys.getenv("TEST_MODE") == "1") "surveys_test" else "surveys"

con <- dbConnect(
  Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = Sys.getenv("DB_PORT"),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD"),
  sslmode  = "require"
)

df <- dbReadTable(con, SURVEY_TABLE)

dbDisconnect(con)

head(df)
