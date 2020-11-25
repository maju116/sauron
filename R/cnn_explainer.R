#' Creates `CNNexplainers` object.
#' @description Creates `CNNexplainers` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplainers` object.
#' @export
CNNexplainers <- R6::R6Class(
  classname = "CNNexplainers",
  public = list(
    #' @field explainers List of `CNNexplainer` objects.
    explainers = NULL,
    #' @description Initializes `CNNexplainers` object.
    #' @param ... `CNNexplainer` objects.
    initialize = function(...) {
      if (list(...) %>% map_lgl(~ !("CNNexplainer" %in% class(.))) %>% any()) {
        stop("You can only pass objects of class 'CNNexplainer'.
             Please check your 'explainers' list.")
      }
      self$explainers <- list(...)
    },
    #' @description Prints available explanation methods.
    show_available_methods = function() {
      private$available_methods
    },
    #' @description Generates explanations.
    #' @param input_imgs_paths Input images paths.
    #' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
    #' @param methods Methods to be calculated.
    #' @param num_samples Number of noised samples per one image.
    #' @param noise_sd Gaussian noise standard deviation.
    #' @param steps Integration steps. Must be positive integer.
    #' @param patch_size Patch size. 2-D `integer` vector.
    #' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
    #' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
    #' @return Explanations for images.
    explain = function(input_imgs_paths,
                       class_index,
                       methods,
                       num_samples, noise_sd,
                       steps, patch_size,
                       absolute_values,
                       grayscale) {
      self$explainers %>% map(~ {
        current_explainer <- .x
        current_explainer$explain(input_imgs_paths,
                                  class_index,
                                  methods,
                                  num_samples, noise_sd,
                                  steps, patch_size,
                                  absolute_values,
                                  grayscale)
      })
    },
    #' @description Generates raster image(s) with explanations.
    #' @param explanations Explanations.
    #' @param combine_plots Should images be combined.
    #' @param output_path Where to save explanation plots.
    #' @param plot Should explanation be plotted.
    save_explanation_plots = function(explanations, combine_plots,
                                      output_path = NULL, plot = TRUE) {
      explanations %>% map(~ {
        current_explanations <- .x
        create_cnn_explanation_plots(current_explanations)
      }) %>%
        save_cnn_explanation_plots(combine_plots, output_path, plot)
    }
  ),
  private = list(
    available_methods = filter_methods_by_network("CNN")
  )
)

#' Creates `CNNexplainer` object.
#' @description Creates `CNNexplainer` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplainer` object.
#' @export
CNNexplainer <- R6::R6Class(
  classname = "CNNexplainer",
  public = list(
    #' @field model Tensorflow model.
    model = NULL,
    #' @field preprocessing_function Image preprocessing function.
    preprocessing_function = NULL,
    #' @field id Explainer id.
    id = NULL,
    #' @description Initializes `CNNexplainer` object.
    #' @param model Tensorflow model.
    #' @param preprocessing_function Image preprocessing function.
    #' @param id Explainer id.
    initialize = function(model, preprocessing_function, id = NULL) {
      self$model <- model
      self$preprocessing_function <- preprocessing_function
      self$id <- if (is.null(id) || class(id) != "character" || id == "") {
        paste0(sample(letters, 10), collapse = "")
      } else {
        id
      }
    },
    #' @description Prints available explanation methods.
    show_available_methods = function() {
      private$available_methods
    },
    #' @description Generates explanations.
    #' @param input_imgs_paths Input images paths.
    #' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
    #' @param methods Methods to be calculated.
    #' @param num_samples Number of noised samples per one image.
    #' @param noise_sd Gaussian noise standard deviation.
    #' @param steps Integration steps. Must be positive integer.
    #' @param patch_size Patch size. 2-D `integer` vector.
    #' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
    #' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
    #' @return Explanations for images.
    explain = function(input_imgs_paths,
                       class_index,
                       methods,
                       num_samples, noise_sd,
                       steps, patch_size,
                       absolute_values,
                       grayscale) {
      generate_cnn_explanations(self$model, input_imgs_paths,
                                self$id, self$preprocessing_function,
                                class_index,
                                methods,
                                num_samples, noise_sd,
                                steps, patch_size,
                                absolute_values,
                                grayscale)
    },
    #' @description Generates raster image(s) with explanations.
    #' @param explanations Explanations.
    #' @param combine_plots Should images be combined.
    #' @param output_path Where to save explanation plots.
    #' @param plot Should explanation be plotted.
    save_explanation_plots = function(explanations, combine_plots,
                                      output_path = NULL, plot = TRUE) {
      save_cnn_explanation_plots(
        create_cnn_explanation_plots(explanations),
        combine_plots, output_path, plot)
    }
  ),
  private = list(
    available_methods = filter_methods_by_network("CNN")
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
  input_imgs <- input_imgs_paths %>% map(~ {
    image_load(., target_size = target_size[1:2]) %>%
      image_to_array() %>% array_reshape(dim = c(1, target_size))
  }) %>% abind(along = 1)

  explanations <- list(Input = input_imgs)
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
  list(
    explanations = explanations,
    metadata = list(
      id = id,
      target_size = target_size,
      methods = methods,
      n_imgs = length(input_imgs_paths)
    )
  )
}
