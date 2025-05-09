# R/utils_db.R ---------------------------------------------------------------
# Helper functions shared by server code
library(DBI)
library(sf)          # for fast geodesic distance
# If you prefer geosphere: library(geosphere)

# --- FUNCTION: get_site_id ---------------------------------------------------
# • Checks if (lat, lon) is within SAME_THRESH metres of an existing site.
# • If yes, returns that site's ID.
# • If not, inserts a row into 'sites' and returns the new ID.
#
# Works with SQLite. For PostGIS you can rewrite the query to
# use ST_Distance for better performance.
get_site_id <- function(lat, lon, con,
                        same_thresh = 5,    # metres
                        new_thresh  = 50) { # kept for future logic
  
  # 1. Load existing sites -----------------------------------------------
  sites <- DBI::dbReadTable(con, "sites")   # cols: site_id, lat, lon
  if (nrow(sites) == 0) {
    # first site ever – insert and return 1
    DBI::dbExecute(con, "
      INSERT INTO sites (lat, lon) VALUES (?, ?)", params = list(lat, lon))
    return(DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1,1])
  }
  
  # 2. Compute distance to nearest site (in R, fine for <10 k sites) ------
  # Use sf for convenience
  pts_existing <- sf::st_as_sf(sites, coords = c("lon", "lat"), crs = 4326)
  pt_new       <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  dists <- sf::st_distance(pts_existing, pt_new)        # units in metres
  mindist <- min(as.numeric(dists))
  
  if (mindist < same_thresh) {
    return(sites$site_id[which.min(dists)])
  }
  
  # 3. Insert new site -----------------------------------------------------
  DBI::dbExecute(con,"
    INSERT INTO sites (lat, lon) VALUES (?, ?)",
    params = list(lat, lon))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1,1]
}
