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
  imgs_dim <- explanations$metadata$target_size
  n_imgs <- explanations$metadata$n_imgs
  h <- imgs_dim[1]
  w <- imgs_dim[2]
  xy_axis <- expand.grid(1:w, h:1) %>% rename(x = Var1, y = Var2)
  explanation_plots <- explanations$explanations %>% imap(~ {
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
  list(explanation_plots = explanation_plots,
       metadata = explanations$metadata)
}

#' Combines multimodel explanation plots.
#' @description Combines multimodel explanation plots.
#' @importFrom purrr pmap
#' @param explanation_plots Explanation plots.
#' @return Raster image(s) with explanations.
combine_cnn_multimodel_explanation_plots <- function(explanation_plots) {
  n_models <- length(explanation_plots)
  n_imgs <- explanation_plots[[1]]$metadata$n_imgs
  plots_order <- 1:n_imgs %>%
    map(~ seq(.x, n_models*n_imgs, by = n_imgs)) %>% unlist()
  pmap(explanation_plots %>% map(~ .$explanation_plots), c) %>%
    map(~ .x[plots_order])
}

#' Plots raster image(s) with explanations.
#' @description Generates raster image(s) with explanations.
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom purrr iwalk
#' @param explanation_plots Explanation plots.
#' @param output_path Where to save explanation plots.
#' @param model_name Model name.
#' @param single_img_height Single image height.
#' @param single_img_width Single image width.
#' @param plot Should explanation be plotted.
save_single_explanation_plots <- function(explanation_plots, output_path, model_name,
                                          single_img_height, single_img_width, plot) {
  pixel_to_cm <- 2.54 / 96
  explanation_plots$explanation_plots %>% iwalk(~ {
    explanation_name <- find_method_name(.y)
    .x %>% iwalk(~ {
      img_nr <- .y
      final_plot <- .x + ggtitle(explanation_name) +
        theme(plot.title = element_text(hjust = 0.5))
      if (plot) plot(final_plot)
      if (!is.null(output_path)) {
        ggsave(file.path(output_path,
                         paste0(model_name, "_", img_nr, "_",
                                explanation_name,  ".png")),
               plot = final_plot,
               height = single_img_height * pixel_to_cm,
               width = single_img_width * pixel_to_cm,
               limitsize = FALSE, units = "cm")
      }
    })
  })
}

#' Plots raster image(s) with explanations.
#' @description Plots raster image(s) with explanations.
#' @importFrom gridExtra grid.arrange arrangeGrob
#' @importFrom purrr map_dbl walk map_chr
#' @param explanation_plots Explanation plots.
#' @param combine_plots Should images be combined.
#' @param output_path Where to save explanation plots.
#' @param plot Should explanation be plotted.
#' @export
save_cnn_explanation_plots <- function(explanation_plots, combine_plots,
                                       output_path = NULL, plot = TRUE) {
  pixel_to_cm <- 2.54 / 96
  multimodel_explanations <- !("metadata" %in% names(explanation_plots))
  if (multimodel_explanations) {
    n_models <- length(explanation_plots)
    model_names <- explanation_plots %>% map_chr(~ .$metadata$id)
    single_img_height <- explanation_plots %>% map_dbl(~ .$metadata$target_size[1]) %>% max()
    single_img_width <- explanation_plots %>% map_dbl(~ .$metadata$target_size[2]) %>% max()
    n_imgs <- explanation_plots[[1]]$metadata$n_imgs
    if (combine_plots) {
      explanation_plots <- combine_cnn_multimodel_explanation_plots(explanation_plots)
      ncol <- length(explanation_plots)
      grobs <- explanation_plots %>% imap(~ {
        explanation_name <- find_method_name(.y)
        arrangeGrob(grobs = .x, top = explanation_name, ncol = 1)
      })
      final_plot <- arrangeGrob(grobs = grobs, ncol = ncol,
                                bottom = model_names %>%
                                  paste(collapse = ", ") %>%
                                  paste("Models order:", .))
      if (plot) plot(final_plot)
      if (!is.null(output_path)) {
        ggsave(file.path(output_path, paste0(model_names %>%
                                               paste(collapse = "_"),
                                             "_explanations.png")),
               plot = final_plot,
               height = 1.1 * n_models * n_imgs * single_img_height * pixel_to_cm,
               width = 1.1 * single_img_width * ncol * pixel_to_cm,
               limitsize = FALSE, units = "cm")
      }
    } else {
      explanation_plots %>% walk(~ {
        current_explanations <- .x
        current_model_name <- current_explanations$metadata$id
        current_single_img_height <- current_explanations$metadata$target_size[1]
        current_single_img_width <- current_explanations$metadata$target_size[2]
        current_explanations %>%
          save_single_explanation_plots(output_path, current_model_name,
                                        current_single_img_height,
                                        current_single_img_width, plot)
      })
    }
  } else {
    model_name <- explanation_plots$metadata$id
    single_img_height <- explanation_plots$metadata$target_size[1]
    single_img_width <- explanation_plots$metadata$target_size[2]
    n_imgs <- explanation_plots$metadata$n_imgs
    if (combine_plots) {
      ncol <- length(explanation_plots$metadata$methods) + 1
      grobs <- explanation_plots$explanation_plots %>% imap(~ {
        explanation_name <- find_method_name(.y)
        arrangeGrob(grobs = .x, top = explanation_name, ncol = 1)
      })
      final_plot <- arrangeGrob(grobs = grobs, ncol = ncol)
      if (plot) plot(final_plot)
      if (!is.null(output_path)) {
        ggsave(file.path(output_path, paste0(model_name, "_explanations.png")),
               plot = final_plot,
               height = 1.1 * n_imgs * single_img_height * pixel_to_cm,
               width = 1.1 * single_img_width * ncol * pixel_to_cm,
               limitsize = FALSE, units = "cm")
      }
    } else {
      save_single_explanation_plots(explanation_plots, output_path, model_name,
                                    single_img_height, single_img_width, plot)
    }
  }
}
