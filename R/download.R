# Retrieve data for Bioko work
library(curl)


#' Reads configuration file on where to download data.
#' If you want to make the config file, then create
#' a file called `$HOME/.config/MASH/data.ini` and put
#' the following in it:
#' ```
#' [Default]
#' SCPHOST = computer-name.ihme.uw.edu
#' SCPHOSTBASE = /path/to/data/directory
#' ```
data_configuration <- function() {
  home <- list(
    xdg = Sys.getenv("XDG_CONFIG_HOME"),
    env = fs::path(Sys.getenv("HOME"), ".config"),
    default = fs::path("", "home", Sys.info()[["effective_user"]])
  )

  for (directory in home) {
    ini_path <- fs::path(directory, "MASH", "data.ini")
    if (file.exists(ini_path)) {
      cfg <- configr::read.config(ini_path)
      if (is.list(cfg)) {
        data_configuration <<- cfg$Default
        return(data_configuration)
      }
    }
  }
  FALSE
}


#' Unzip a 7zip file. There is a Github project called archive
#' that would do this, too. This command is on Ubuntu but may
#' not be elsewhere.
#'
#' @param archive Path to the file in 7z format.
#' @param where Directory into which to unzip the archive.
#'     This command changes the working directory to `where`
#'     in order to unzip.
un7zip <- function(archive, where) {
  archive <- normalizePath(archive)
  current_path <- setwd(where)
  system(paste("7zr x", archive, sep = " "))
  setwd(current_path)
}


#' Use scp to retrieve data, if you have ssh credentials set up.
#' This is used for data that isn't yet public.
#' Equivalent to: ssh ihme.uw.edu:/path/to/file local_file.dat
#'
#' You have to call `data_configuration()` before you call this.
get_from_ihme <- function(filename, local_directory = "inst/extdata") {
  base_directory <- fs::path(data_configuration$SCPHOSTBASE, "equatorial_guinea")
  host <- data_configuration$SCPHOST
  target <- fs::path(base_directory, filename)
  local_file <- fs::path(local_directory, filename)
  system(paste("scp", paste(host, target, sep = ":"), local_file, sep = " "))
}


#' Worldpop returns several GeoTIFFs in WGS 84.
#' GNQ = Equatorial Guinea
#' 10 or 15 is 2010 or 2015 data.
#' adjv2 or v2 is whether it was adjusted to match WHO.
#' So use GNQ15v2.tif.
download_worldpop <- function(local_directory = "inst/extdata", overwrite = FALSE) {
  local_file <- fs::path(local_directory, "Equatorial_Guinea_100m_Population.7z")
  worldpop_directory <- fs::path_ext_remove(local_file)
  remote_url <- "ftp://ftp.worldpop.org.uk/GIS/Population/Individual_countries/GNQ/Equatorial_Guinea_100m_Population.7z"

  if (!file.exists(local_file)) {
    curl::curl_download(remote_url, local_file, mode = "wb")
  }
  if (!dir.exists(worldpop_directory)) {
    dir.create(worldpop_directory)
    un7zip(local_file, worldpop_directory)
  }
  dir(worldpop_directory)
}


#' Bioko grids are two shapefiles in UTM zone 32N projection.
#' The 100m grids are secs.shp and the 1km are mapareas.shp.
#' The two grids align in this projection.
download_bioko_grids <- function(local_directory = "inst/extdata") {
  filename <- "Bioko_grids.zip"
  local_path <- fs::path(local_directory, filename)
  destination_directory <- fs::path_ext_remove(local_path)
  if (!file.exists(local_path)) {
    get_from_ihme(filename)
  }
  if (!dir.exists(destination_directory)) {
    dir.create(destination_directory)
  }
  unzip(local_path, exdir = destination_directory)
}


read_bioko_grids <- function(local_directory = "inst/extdata") {
  grid_dir <- fs::path(local_directory, "Bioko_grids")
  fine_grid <- sf::st_read(dsn = fs::path(grid_dir, "secs.shp"), layer = "secs")
  coarse_grid <- sf::st_read(dsn = fs::path(grid_dir, "mapareas_grid.shp"), layer = "mapareas_grid")
  list(fine = fine_grid, coarse = coarse_grid)
}


#' HRSL is a geotiff in WGS 84 for all of Equatorial Guinea (GNQ).
download_hrsl_points <- function(local_directory = "inst/extdata") {
  filename <- "population_gnq_2018-10-01.zip"
  local_path <- fs::path(local_directory, filename)
  if (!file.exists(local_path)) {
    get_from_ihme(filename)
  }
  unzip(local_path, exdir = local_directory)
}


read_hrsl <- function(local_directory = "inst/extdata") {
  raster::raster(fs::path(local_directory, "population_gnq_2018-10-01.tif"))
}
