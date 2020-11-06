#' Calculates smooth-grad for a CNN.
#' @description Calculates smooth-grad for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param num_samples Number of noised samples per one image.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return smooth-grad for a CNN.
#' @export
smooth_grad <- function(model, input_imgs, preprocessing_function = NULL,
                             class_index = NULL, num_samples = 5, absolute_values = TRUE,
                             grayscale = TRUE, standardize = TRUE) {
  noised_imgs <- generate_noisy_images(input_imgs, num_samples)
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
    noised_imgs <- preprocessing_function(noised_imgs)
  }

  if (is.null(class_index)) {
    preds <- model(tf$cast(input_imgs, tf$float32))
    class_index <- tf$argmax(preds, axis = as.integer(1))$numpy() + 1
    class_index <- rep(class_index, each = num_samples)
  }

  gradients <- get_input_output_gradients(model, noised_imgs, class_index)
  if (absolute_values) gradient_x_input <- tf$abs(gradient_x_input)
  if (grayscale) gradient_x_input <- transform_rgb_images_to_grayscale(gradient_x_input)
  if (standardize) gradient_x_input <- per_image_standardization(gradient_x_input)
  gradient_x_input$numpy()
}

#' Generates noisy images.
#' @description Generates noisy images.
#' @import tensorflow
#' @param input_imgs Input images if for of 4-D tensor.
#' @param num_samples Number of noised samples per one image.
#' @return Noised images.
generate_noisy_images <- function(input_imgs, num_samples) {
  input_imgs <- tf$image$convert_image_dtype(test_imgs, dtype = tf$float32, saturate = FALSE)
  images_copy <- tf$keras$backend$repeat_elements(input_imgs, as.integer(num_samples), axis = as.integer(0))
  noise <- 255 * tf$random$normal(shape = tf$shape(images_copy), mean = 0.0, stddev = 0.1, dtype = tf$float32)
  tf$clip_by_value(tf$add(images_copy, noise), 0, 255)$numpy()
}
