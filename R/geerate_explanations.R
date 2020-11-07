#' Acronyms for available explanation methods.
#' @description Acronyms for available explanation methods.
#' @export
available_methods <- c("V", "GI", "SG", "SGI", "IG")

#' Generates explanations for images.
#' @description Generates explanations for images.
#' @import keras
#' @importFrom purrr map
#' @param model Tensorflow model.
#' @param input_imgs_paths Input images paths.
#' @param target_size Input images target size.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param methods Methods to be calculated.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @param steps Integration steps. Must be positive integer.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return Explanations for images.
#' @export
generate_explanations <- function(model, input_imgs_paths, target_size = c(299, 299),
                              preprocessing_function = NULL,
                              class_index = NULL, methods = available_methods,
                              num_samples = 5, noise_sd = 0.1, steps = 20,
                              absolute_values = TRUE,
                              grayscale = TRUE, standardize = TRUE) {
  input_imgs <- input_imgs_paths %>% map(~ {
    image_load(., target_size = target_size) %>%
      image_to_array() %>% array_reshape(dim = c(1, target_size, 3))
  }) %>% abind(along = 1)

  explanations <- list()
  for (m in methods) {
    if (m == "V") {
      explanations[m] <- vanilla_gradient(model, input_imgs, preprocessing_function,
                                          class_index, absolute_values, grayscale, standardize)
    } else if (m == "GI") {
      explanations[m] <- gradient_x_input(model, input_imgs, preprocessing_function,
                                          class_index, absolute_values, grayscale, standardize)
    } else if (m == "SG") {
      explanations[m] <- smooth_grad(model, input_imgs, preprocessing_function,
                                     class_index, num_samples, noise_sd,
                                     absolute_values, grayscale, standardize)
    } else if (m == "SGI") {
      explanations[m] <- smooth_grad(model, input_imgs, preprocessing_function,
                                     class_index, num_samples, noise_sd,
                                     absolute_values, grayscale, standardize)
    } else if (m == "IG") {
      explanations[m] <- integrated_gradients(model, input_imgs, preprocessing_function,
                                              class_index, steps,
                                              absolute_values, grayscale, standardize)
    }
  }
  explanations
}
