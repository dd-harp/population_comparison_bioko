#' Save an image to the images directory as both pdf and png.
#'
#' This saves the last ggplot image to a directory
#' as both pdf and png.
#'
#' @param name The base name to use for the file.
#' @return The filenames to which it saved.
#' @export
save_image <- function(name) {
  image_directory <- rprojroot::find_package_root_file("images")
  if (!dir.exists(image_directory)) {
    dir.create(image_directory, showWarnings = FALSE, recursive = TRUE)
  }
  formats <- c("pdf", "png")
  filename <- vector(mode = "character", length = length(formats))
  for (f_idx in seq(along = formats)) {
    filename[f_idx] <- fs::path(image_directory, name, ext = formats[f_idx])
    ggplot2::ggsave(filename[f_idx])
  }
  invisible(filename)
}


#' Save a plot by base::plot to a file.
#'
#' This saves the plot to a directory
#' as both pdf and png.
#'
#' @param plot_function The function to run in order to plot
#' @param name The base name to use for the file.
#' @return The filenames to which it saved.
#' @export
save_plot <- function(plot_function, name) {
  image_directory <- rprojroot::find_package_root_file("images")
  if (!dir.exists(image_directory)) {
    dir.create(image_directory, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(image_directory)) {
    dir.create(image_directory, showWarnings = FALSE, recursive = TRUE)
  }
  formats <- list(
    list(fun = grDevices::pdf, name = "pdf"),
    list(fun = grDevices::png, name = "png")
  )
  filename <- vector(mode = "character", length = length(formats))
  for (f_idx in 1:length(formats)) {
    filename[f_idx] <- fs::path(image_directory, name, ext = formats[[f_idx]][["name"]])

    formats[[f_idx]][["fun"]](filename[f_idx])
    plot_function()
    dev.off()
  }
  invisible(filename)
}
