#' popbioko: Computes Bioko island population metrics.
#'
#' @section Retrieve data:
#'
#' Gets data from the web or from cluster storage.
#'
#' \itemize{
#'   \item \link{data_configuration} - Reads configuration file on where to download data.
#'   \item \link{un7zip} - Unzip a 7zip file.
#'   \item \link{get_from_ihme} - Use scp to retrieve data.
#'   \item \link{download_worldpop} - Retrieve worldpop data.
#'   \item \link{download_bioko_grids} - Retrieve Bioko grid data.
#'   \item \link{download_hrsl_points} - Download HRSL from the repository.
#' }
#'
#'
#' @section Read and write data:
#'
#' Reads the data into R or writes it to a file or finds it on disk.
#'
#' \itemize{
#'   \item \link{read_bioko_grids} - Read the Bioko grid information as a shapefile.
#'   \item \link{read_hrsl} - Read the HRSL as a raster file.
#'   \item \link{read_landscan} - Load LandScan raster file.
#'   \item \link{write_aligned_raster} - Write a raster layer to a file.
#'   \item \link{filenames_to_description} - Given filenames, decode them into traits of the file.
#'   \item \link{read_bimep_point_data} - Read BIMEP point data
#'   \item \link{bimep_population_as_points} - Turns each population entry into a lat-long coordinate.
#' }
#'
#'
#' @section Conversions:
#'
#' \itemize{
#'   \item \link{im_to_raster} - Convert a spatstat im to a raster image
#'   \item \link{parameters_of_km_grid} - Given a 1km grid for Bioko, figure out how you would remake it.
#'   \item \link{make_fresh_grid} - Creates a regular grid of rectangular polygons.
#'   \item \link{bimep_named_vector} - Associate BIMEP index with its map ID.
#'   \item \link{bimep_on_grid} - Aligns long-lat per-house data to a given raster grid.
#'   \item \link{bimep_population_as_raster} - Read BIMEP population as a raster.
#'   \item \link{hrsl_points} - Given an HRSL raster, convert it into points in space.
#'   \item \link{assign_to_grid} - Given spatial points of population, assign them to shapes.
#' }
#'
#'
#' @section Metrics:
#'
#' \itemize{
#'   \item \link{population_count_estimator} - Estimate density of population using density estimator on points.
#'   \item \link{population_density_estimator} - Estimate density of population using density estimator on the image.
#'   \item \link{density_from_disc} - Find the average density within a 1 square km disc around each pixel
#'   \item \link{square_meters_per_pixel.im} - Calculate square meters per pixel for a spatstat im
#'   \item \link{square_meters_per_pixel.raster} - Calculate square meters per pixel for a raster::raster
#'   \item \link{pareto_fraction} - Compute pareto fraction of a vector.
#'   \item \link{power_bandwidth} - Estimate bandwidth for a density function kernel with Bowman and Azzalini.
#'   \item \link{density_estimated} - Create a map with density estimation
#'   \item \link{urban_fraction} - Urban fraction from a density raster
#'   \item \link{urban_fraction_by_density_estimator} - Calculate urban fraction using a density estimator
#'   \item \link{summary_statistics} - Compute basic summary statistics.
#' }
#'
#'
#'
#' @docType package
#' @name gisdemog
NULL
