#' Calculates vanilla gradient for a CNN.
#' @description Calculates vanilla gradient for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return Vanilla gradient for a CNN.
#' @export
vanilla_gradient <- function(model, input_imgs, preprocessing_function = NULL,
                             class_index = NULL, absolute_values = TRUE,
                             grayscale = TRUE, standardize = TRUE) {
  check_class_indexes(input_imgs, class_index)
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  gradients <- get_input_output_gradients(model, input_imgs, class_index)
  if (absolute_values) gradients <- tf$abs(gradients)
  if (grayscale) gradients <- transform_rgb_images_to_grayscale(gradients)
  if (standardize) gradients <- per_image_standardization(gradients)
  gradients$numpy()
}
