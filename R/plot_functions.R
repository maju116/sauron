#' Transforms pixel `array` into `data.frame` with raster data.
#' @description Transforms pixel `array` into `data.frame` with raster data.
#' @param xy_axis `x` and `y` image grid.
#' @param sample_image Pixel `array`.
#' @param grayscale Should images be plotted in grayscale.
#' @return  `data.frame` with raster data.
create_plot_data <- function(xy_axis, sample_image, grayscale) {
  if (is.na(grayscale)) {
    cbind(xy_axis,
          confidence = as.vector(t(sample_image)))
  } else if (grayscale) {
    cbind(xy_axis,
          gray = as.vector(t(sample_image)) / max(sample_image))
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
    theme_void() + geom_raster(hjust = 0, vjust = 0, interpolate = TRUE)
}

#' Generates heatmap image.
#' @description Generates heatmap image.
#' @import ggplot2
#' @importFrom grDevices rgb gray
#' @param plot_data `data.frame` with `x`, `y` coordinates and color values.
#' @return  Heatmap image.
plot_heatmap <- function(plot_data) {
  ggplot(plot_data, aes(x, y, fill = confidence)) +
    theme_void() + geom_raster(hjust = 0, vjust = 0, interpolate = TRUE) +
    scale_fill_gradient(low = "blue", high = "red")
}

#' Generates raster image(s) with explanations.
#' @description Generates raster image(s) with explanations.
#' @import ggplot2
#' @importFrom dplyr rename filter pull
#' @importFrom purrr imap
#' @param explanations Explanations.
#' @return Raster image(s) with explanations.
create_cnn_explanation_plots <- function(explanations) {
  imgs_dim <- dim(explanations$Input)
  n_imgs <- imgs_dim[1]
  h <- imgs_dim[2]
  w <- imgs_dim[3]
  xy_axis <- expand.grid(1:w, h:1) %>% rename(x = Var1, y = Var2)
  explanation_plots <- explanations %>% imap(~ {
    explanation_name <- .y
    current_explanation <- .x
    1:n_imgs %>% map(~ {
      idx <- .x
      sample_image <- current_explanation[idx, , , , drop = TRUE]
      grayscale <- if (explanation_name != "OCC") {
        length(dim(sample_image)) == 2
      } else {
        NA
      }
      plot_data <- create_plot_data(xy_axis, sample_image, grayscale)
      base_plot <- if (!is.na(grayscale)) {
        plot_raster(plot_data, grayscale)
      } else {
        plot_heatmap(plot_data)
      }
      base_plot
    })
  })
  explanation_plots
}

#' Plots raster image(s) with explanations.
#' @description Generates raster image(s) with explanations.
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom purrr iwalk
#' @param explanation_plots Explanation plots.
#' @param combine_plots Should images be combined.
#' @export
save_cnn_explanation_plots <- function(explanation_plots, combine_plots) {
  if (combine_plots) {
    ncol <- length(explanation_plots)
    grobs <- explanation_plots %>% imap(~ {
      explanation_name <- find_method_name(.y)
      arrangeGrob(grobs = .x, top = explanation_name)
    })
    grid.arrange(grobs = grobs, ncol = ncol)
  } else {
    explanation_plots %>% iwalk(~ {
      explanation_name <- find_method_name(.y)
      .x %>% walk(~ {
        base_plot <- .x
        plot(base_plot + ggtitle(explanation_name) +
               theme(plot.title = element_text(hjust = 0.5)))
      })
    })
  }
}
