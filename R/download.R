# Retrieve data for Bioko work
library(curl)


#' Reads configuration file on where to download data.
#'
#' It stores that configuration in `data_config`
#' at the global level.
#'
#' If you want to make the config file, then create
#' a file called `$HOME/.config/MASH/data.ini` and put
#' the following in it:
#'
#' @return A list of configuration parameters.
#'
#' @examples
#' \dontrun{
#' [Default]
#' SCPHOST = computer-name.ihme.uw.edu
#' SCPHOSTBASE = /path/to/data/directory
#' }
#'
#' @export
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
        data_config <<- cfg$Default
        return(data_config)
      }
    }
  }
  FALSE
}


#' Unzip a 7zip file.
#'
#' There is a Github project called archive
#' that would do this, too. This command is on Ubuntu but may
#' not be elsewhere.
#'
#' @param archive Path to the file in 7z format.
#' @param where Directory into which to unzip the archive.
#'     This command changes the working directory to `where`
#'     in order to unzip.
#' @export
un7zip <- function(archive, where) {
  archive <- normalizePath(archive)
  current_path <- setwd(where)
  system(paste("7zr x", archive, sep = " "))
  setwd(current_path)
}


#' Use scp to retrieve data.
#'
#' Only if you have ssh credentials set up.
#' This is used for data that isn't yet public.
#' Equivalent to: ssh ihme.uw.edu:/path/to/file local_file.dat
#'
#' You have to call `data_configuration()` before you call this.
#'
#' @param filename The path of the file within the repository.
#' @param local_directory Where to put that file on the local machine.
#' @export
get_from_ihme <- function(filename, local_directory = "inst/extdata") {
  base_directory <- fs::path(data_config$SCPHOSTBASE, "equatorial_guinea")
  host <- data_config$SCPHOST
  target <- fs::path(base_directory, filename)
  local_file <- fs::path(local_directory, filename)
  system(paste("scp", paste(host, target, sep = ":"), local_file, sep = " "))
}


#' Retrieve worldpop data.
#'
#' Worldpop returns several GeoTIFFs in WGS 84.
#' GNQ = Equatorial Guinea
#' 10 or 15 is 2010 or 2015 data.
#' adjv2 or v2 is whether it was adjusted to match WHO.
#' So use GNQ15v2.tif.
#'
#' This dataset is from 2018, even though it doesn't say that in the URL. The ftp shows a last
#' modification date of 11/23/18. We could also use:
#' ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/2018/GNQ/gnq_ppp_2018.tif
#'
#' @param local directory Where to put that file on the local machine.
#' @param overwrite Whether to overwrite an existing file by the same name.
#' @export
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



#' The gridded population of the world
#' @export
download_gridded_population_world <- function() {
  url <- "https://sedac.ciesin.columbia.edu/downloads/data/gpw-v4/gpw-v4-population-count-rev11/gpw-v4-population-count-rev11_2020_30_sec_tif.zip"
  url
}


#' Retrieve Bioko grid data.
#'
#' Bioko grids are two shapefiles in UTM zone 32N projection.
#' The 100m grids are secs.shp and the 1km are mapareas.shp.
#' The two grids align in this projection.
#' @param local directory Where to put that file on the local machine.
#' @export
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


#' Read the Bioko grid information as a shapefile.
#'
#' @param local directory Where to put that file on the local machine.
#' @return a list with the `fine` and `coarse` shapefiles.
#' @export
read_bioko_grids <- function(local_directory = "inst/extdata") {
  grid_dir <- fs::path(local_directory, "Bioko_grids")
  fine_grid <- sf::st_read(dsn = fs::path(grid_dir, "secs.shp"), layer = "secs")
  coarse_grid <- sf::st_read(dsn = fs::path(grid_dir, "mapareas_grid.shp"), layer = "mapareas_grid")
  list(fine = fine_grid, coarse = coarse_grid)
}


#' Download HRSL from the repository.
#'
#' HRSL is a geotiff in WGS 84 for all of Equatorial Guinea (GNQ).
#' @param local directory Where to put that file on the local machine.
#' @export
download_hrsl_points <- function(local_directory = "inst/extdata") {
  filename <- "population_gnq_2018-10-01.zip"
  local_path <- fs::path(local_directory, filename)
  if (!file.exists(local_path)) {
    get_from_ihme(filename)
  }
  unzip(local_path, exdir = local_directory)
}

#' Read the HRSL as a raster file.
#'
#' @param local directory Where to put that file on the local machine.
#' @return a `raster::raster` file.
#' @export
read_hrsl <- function(local_directory = "inst/extdata") {
  raster::raster(fs::path(local_directory, "population_gnq_2018-10-01.tif"))
}


#' Load LandScan raster file.

#' @param local directory Where to put that file on the local machine.
#' @return a `raster::raster` file.
#' @export
read_landscan <- function(local_directory = "inst/extdata") {
  raster::raster(fs::path(local_directory, "LandScan Global 2018", "lspop2018"))
}


#' Load WorldPop raster file.

#' @param local directory Where to put that file on the local machine.
#' @return a `raster::raster` file.
#' @export
read_worldpop <- function(local_directory = "inst/extdata") {
  wp_options <- list(raw = "gnq_ppp_2018.tif", adjusted = "gnq_ppp_2018_UNadj.tif")
  raster::raster(fs::path(
    local_directory, "Equatorial_Guinea_100m_Population", wp_options[["adjusted"]]))
}


#' Write a raster layer to a file.
#'
#' Our intermediate datasets go into a directory with a given format.
#' We write these files into an `aligned` subdirectory of the local directory.
#' The name is (grid, resolution, source), in that order. That way
#' they are near each other when you `dir()`.
#' This also ensures the name of the layer matches the source name.
#'
#' @param layer is the RasterLayer to write.
#' @param layer_id is a list describing the layer with
#'   (source = (HRSL, BIMEP, LandScan, WorldPop), grid = (HRSL, LandScan, Worldpop))
#'   and resolution = (100, 1000).
#' @param local_directory is where we put the data. This data
#'   will be written to a subdirectory of that.
#' @export
write_aligned_raster <- function(layer, layer_id, local_directory = "inst/extdata") {
  aligned <- fs::path(local_directory, "aligned")
  if (!dir.exists(aligned)) {
    fs::dir_create(aligned, recurse = TRUE)
  }
  resolution <- ifelse(layer_id$resolution < 800, "fine", "coarse")
  filename <- paste(
    layer_id$grid,
    resolution,
    layer_id$source,
    sep = "_"
  )
  layer_path <- fs::path(aligned, filename, ext = "tif")
  if (file.exists(layer_path)) {
    unlink(layer_path)
  }
  names(layer) <- layer_id$source
  raster::writeRaster(
    layer,
    filename = layer_path,
    format = "GTiff"
  )
}


#' Given filenames, decode them into traits of the file.
#'
#' @param filenames A character vector of filenames
#' @return A dataframe with parameters of the files.
#' @export
filenames_to_description <- function(filenames) {
  description_df <- data.frame(
    filename = filenames,
    name = filenames,
    resolution = rep(100L, length(filenames)),
    source = filenames,
    grid = filenames,
    stringsAsFactors = FALSE
  )
  for (file_idx in 1:length(filenames)) {
    filename <- filenames[file_idx]
    splitted <- strsplit(filename, "[_.]")[[1]]
    grid <- splitted[1]
    resolution <- splitted[2]
    source <- splitted[3]
    full_name <- paste(source, "on", grid, resolution)
    description_df[file_idx, ] <- list(filename, full_name, resolution, source, grid)
  }
  rownames(description_df) <- description_df$name
  description_df
}
