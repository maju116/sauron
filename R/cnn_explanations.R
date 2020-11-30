#' Creates `CNNexplanations` object.
#' @description Creates `CNNexplanations` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplanations` object.
#' @export
CNNexplanations <- R6::R6Class(
  classname = "CNNexplanations",
  public = list(
    #' @description Initializes `CNNexplanations` object.
    #' @param explanations Explanations.
    #' @param ids Explainers ids.
    #' @param target_sizes Target sizes list.
    #' @param methods Explanation methods.
    #' @param input_imgs_paths Input images paths.
    initialize = function(explanations, ids, target_sizes, methods, input_imgs_paths) {
      private$check_inputs(explanations, ids, target_sizes, methods, input_imgs_paths)
      private$explanations = explanations
      private$metadata = list(
        multimodel_explanations = length(ids) > 1,
        ids = ids,
        n_models = length(ids),
        target_sizes = target_sizes,
        methods = methods,
        input_imgs_paths = input_imgs_paths,
        n_imgs = length(input_imgs_paths)
      )
    },
    #' @description Prints basic information about object.
    print = function() {
      paste("CNNexplanations object contains explanations for",
            private$metadata$n_imgs, "images for",
            private$metadata$n_models,
            if (private$metadata$multimodel_explanations) "models." else "model.") %>%
        cat()
    },
    #' @description Collects explanations from `CNNexplanations` object.
    get_explanations = function() {
      private$explanations
    },
    #' @description Collects metadata from `CNNexplanations` object.
    get_metadata = function() {
      private$metadata
    },
    #' @description Combines `CNNexplanations` objects.
    #' @param ... `CNNexplanations` objects.
    combine = function(...) {
      if (list(...) %>% map_lgl(~ !("CNNexplanations" %in% class(.))) %>% any()) {
        stop("You can only pass objects of class 'CNNexplanations'.
             Please check your 'explanations' list.")
      }
      for (ex in list(...)) {
        current_explanations <- ex$get_explanations()
        # TODO: check if have same explanations and metadata
        private$explanations <- c(private$explanations, current_explanations)
        current_metadata <- ex$get_metadata()
        private$metadata$ids <- c(private$metadata$ids, current_metadata$ids)
        private$metadata$n_models <- length(private$metadata$ids)
        private$metadata$target_sizes <- c(private$metadata$target_sizes, current_metadata$target_sizes)
        private$metadata$multimodel_explanations <- TRUE
      }
      invisible(self)
    },
    #' @description Generates raster image(s) with explanations.
    #' @param combine_plots Should images be combined.
    #' @param output_path Where to save explanation plots.
    #' @param plot Should explanation be plotted.
    plot_and_save = function(combine_plots, output_path = NULL, plot = TRUE) {
      explanation_plots <- private$explanations %>% map(~ {
        create_cnn_explanation_plots(.)
      })
      pixel_to_cm <- 2.54 / 96
      multimodel_explanations <- private$metadata$multimodel_explanations
      n_models <- private$metadata$n_models
      if (combine_plots) {
        model_names <- private$metadata$ids
        single_img_height <- private$metadata$target_size %>% map_dbl(~ .[1]) %>% max()
        single_img_width <- private$metadata$target_size %>% map_dbl(~ .[2]) %>% max()
        n_imgs <- private$metadata$n_imgs
        explanation_plots <- if (multimodel_explanations) {
          combine_cnn_multimodel_explanation_plots(explanation_plots, n_models, n_imgs)
        } else {
          explanation_plots[[1]]
        }
        ncol <- length(private$metadata$methods) + 1
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
        for (idx in 1:n_models) {
          current_explanations <- explanation_plots[[idx]]
          current_model_name <- private$metadata$ids[[idx]]
          current_single_img_height <- private$metadata$target_sizes[[idx]][1]
          current_single_img_width <- private$metadata$target_sizes[[idx]][2]
          current_explanations %>%
            save_single_explanation_plots(output_path, current_model_name,
                                          basename(private$metadata$input_imgs_paths),
                                          current_single_img_height,
                                          current_single_img_width, plot)
        }
      }

    }
  ),
  private = list(
    explanations = NULL,
    metadata = NULL,
    check_inputs = function(explanations, ids, target_sizes, methods, input_imgs_paths) {
      # TODO
    }
  )
)

#' Generates explanations for images.
#' @description Generates explanations for images.
#' @import keras
#' @importFrom purrr map
#' @param model Tensorflow model.
#' @param input_imgs_paths Input images paths.
#' @param id Explainer id.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param methods Methods to be calculated.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @param steps Integration steps. Must be positive integer.
#' @param patch_size Patch size. 2-D `integer` vector.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @return Explanations for images.
generate_cnn_explanations <- function(model, input_imgs_paths, id,
                                      preprocessing_function = NULL,
                                      class_index = NULL,
                                      methods = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC"),
                                      num_samples = 5, noise_sd = 0.1,
                                      steps = 20, patch_size = c(50, 50),
                                      absolute_values = TRUE,
                                      grayscale = TRUE) {
  target_size <- unlist(model$input$get_shape()$as_list())
  # TODO: Add batch_size and reading directory
  input_imgs <- input_imgs_paths %>% map(~ {
    image_load(., target_size = target_size[1:2]) %>%
      image_to_array() %>% array_reshape(dim = c(1, target_size))
  }) %>% abind(along = 1)

  explanations <- list(Input = input_imgs)
  # TODO: Remove loop, optimize methods
  for (m in methods) {
    if (m == "V") {
      explanations[[m]] <- vanilla_gradient(model, input_imgs, preprocessing_function,
                                            class_index, absolute_values, grayscale, TRUE)
    } else if (m == "GI") {
      explanations[[m]] <- gradient_x_input(model, input_imgs, preprocessing_function,
                                            class_index, absolute_values, grayscale, TRUE)
    } else if (m == "SG") {
      explanations[[m]] <- smooth_grad(model, input_imgs, preprocessing_function,
                                       class_index, num_samples, noise_sd,
                                       absolute_values, grayscale, TRUE)
    } else if (m == "SGI") {
      explanations[[m]] <- smooth_grad(model, input_imgs, preprocessing_function,
                                       class_index, num_samples, noise_sd,
                                       absolute_values, grayscale, TRUE)
    } else if (m == "IG") {
      explanations[[m]] <- integrated_gradients(model, input_imgs, preprocessing_function,
                                                class_index, steps,
                                                absolute_values, grayscale, TRUE)
    } else if (m == "GB") {
      explanations[[m]] <- guided_backpropagation(model, input_imgs, preprocessing_function,
                                                  class_index, absolute_values, grayscale, TRUE)
    } else if (m == "OCC") {
      explanations[[m]] <- occlusion(model, input_imgs, preprocessing_function,
                                     class_index, patch_size)
    }
  }
  CNNexplanations$new(list(explanations), id, list(target_size), methods, input_imgs_paths)
}
