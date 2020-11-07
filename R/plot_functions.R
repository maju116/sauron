#' Transforms pixel `array` into `data.frame` with raster data.
#' @description Transforms pixel `array` into `data.frame` with raster data.
#' @param xy_axis `x` and `y` image grid.
#' @param sample_image Pixel `array`.
#' @param grayscale Should images be plotted in grayscale.
#' @return  `data.frame` with raster data.
create_plot_data <- function(xy_axis, sample_image, grayscale) {
  if (grayscale) {
    cbind(xy_axis,
          gray = as.vector(t(sample_image[, , 1])) / max(sample_image[, , 1]))
  } else {
    cbind(xy_axis,
          r = as.vector(t(sample_image[, , 1])) / max(sample_image[, , 1]),
          g = as.vector(t(sample_image[, , 2])) / max(sample_image[, , 2]),
          b = as.vector(t(sample_image[, , 3])) / max(sample_image[, , 3]))
  }
}

#' Generates raster image.
#' @description Generates raster image.
#' @import ggplot2
#' @importFrom grDevices rgb gray
#' @param plot_data `data.frame` with `x`, `y` coordinates and color values.
#' @param grayscale Should images be plotted in grayscale.
#' @return  Raster image.
plot_raster <- function(plot_data, grayscale) {
  base_plot <- if (grayscale) {
    ggplot(plot_data, aes(x, y, fill = gray(gray)))
  } else {
    ggplot(plot_data, aes(x, y, fill = rgb(r, g, b)))
  }
  base_plot + guides(fill = FALSE) + scale_fill_identity() +
    theme_void() + geom_raster(hjust = 0, vjust = 0)
}

#' Generates raster image.
#' @description Generates raster image.
#' @import ggplot2
#' @importFrom dplyr rename
#' @importFrom purrr set_names
#' @param explanations Explanations.
#' @param combine_plots Should images be combined.
#' @return Raster image(s).
#' @export
plot_explanations <- function(explanations, combine_plots = TRUE) {
  imgs_dim <- dim(explanations$input_imgs)
  n_imgs <- imgs_dim[1]
  h <- imgs_dim[2]
  w <- imgs_dim[3]
  xy_axis <- expand.grid(1:w, h:1) %>% rename(x = Var1, y = Var2)
  explanation_plots <- 1:n_imgs %>% map(~ {
    idx <- .x
    names(explanations) %>% map(~ {
      explanation_name <- .x
      sample_image <- explanations[[explanation_name]][idx, , , , drop = TRUE]
      sample_image <- sample_image / max(sample_image)
      grayscale <- dim(sample_image)[3] == 1
      plot_data <- create_plot_data(xy_axis, sample_image, grayscale)
      base_plot <- plot_raster(plot_data, grayscale)
      if (idx == 1 | !combine_plots) {
        base_plot + ggtitle(explanation_name) +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        base_plot
      }
    })
  }) %>% unlist(recursive = FALSE) %>%
    set_names(rep(names(explanations), n_imgs))
  if (combine_plots) {
    do.call("grid.arrange", c(explanation_plots, nrow = n_imgs))
  } else {
    explanation_plots
  }
}
