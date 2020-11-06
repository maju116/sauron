#' Calculates gradientXinput for a CNN.
#' @description Calculates gradientXinput for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return gradientXinput for a CNN.
#' @export
gradient_x_input <- function(model, input_imgs, preprocessing_function = NULL,
                             class_index = NULL, absolute_values = TRUE,
                             grayscale = TRUE, standardize = TRUE) {
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  gradients <- get_input_output_gradients(model, input_imgs, class_index)
  gradient_x_input <- tf$multiply(input_imgs, gradients)
  if (absolute_values) gradient_x_input <- tf$abs(gradient_x_input)
  if (grayscale) gradient_x_input <- transform_rgb_images_to_grayscale(gradient_x_input)
  if (standardize) gradient_x_input <- per_image_standardization(gradient_x_input)
  gradient_x_input$numpy()
}
