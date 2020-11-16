#' Acronyms and names for available explanation methods.
#' @description Acronyms and names for available explanation methods.
#' @import tibble
#' @export
sauron_available_methods <- tibble::tibble(
  method = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC"),
  name = c("Vanilla gradient",
           "Gradient x Input",
           "SmoothGrad",
           "SmoothGrad x Input",
           "Integrated Gradients",
           "Guided Backpropagation",
           "Occlusion Sensitivity"),
  model_type = "CNN"
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
    iput_data = NULL,
    model = NULL,
    preprocessing_function = NULL,
    initialize = function(iput_data, model, preprocessing_function) {
      self$iput_data <- iput_data
      self$model <- model
      self$preprocessing_function <- preprocessing_function
    },
    show_available_methods = function() {
      private$available_methods
    },
    explain = function(class_index,
                       methods,
                       num_samples, noise_sd,
                       steps, patch_size,
                       absolute_values,
                       grayscale) {
      generate_cnn_explanations(self$model, self$iput_data,
                                self$preprocessing_function,
                                class_index,
                                methods,
                                num_samples, noise_sd,
                                steps, patch_size,
                                absolute_values,
                                grayscale)
    }
  ),
  private = list(
    available_methods = sauron_available_methods %>%
      filter(model_type == "CNN") %>%
      dplyr::select(-model_type)
  )
)

#' Generates explanations for images.
#' @description Generates explanations for images.
#' @import keras
#' @importFrom purrr map
#' @param model Tensorflow model.
#' @param input_imgs_paths Input images paths.
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
#' @export
generate_cnn_explanations <- function(model, input_imgs_paths,
                                  preprocessing_function = NULL,
                                  class_index = NULL,
                                  methods = sauron_available_methods$method,
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
  explanations
}
