#' Calculates smooth-grad for a CNN.
#' @description Calculates smooth-grad for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return smooth-grad for a CNN.
#' @export
smooth_grad <- function(model, input_imgs, preprocessing_function = NULL,
                        class_index = NULL, num_samples = 5, noise_sd = 0.1,
                        absolute_values = TRUE, grayscale = TRUE, standardize = TRUE) {
  smooth_gradients <- calculate_smoothed_gradients(model, input_imgs, preprocessing_function,
                                                   class_index, num_samples, noise_sd)
  if (absolute_values) smooth_gradients <- tf$abs(smooth_gradients)
  if (grayscale) smooth_gradients <- transform_rgb_images_to_grayscale(smooth_gradients)
  if (standardize) smooth_gradients <- per_image_standardization(smooth_gradients)
  smooth_gradients$numpy()
}
