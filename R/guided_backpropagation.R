#' Guided ReLU with custom gradient function.
#' @description Guided ReLU with custom gradient function.
#' @param x Layer input.
#' @return Guided ReLU with custom gradient function.
guidedRelu <- function(x) {
  grad <- function(dy) {
    tf$cast(dy > 0, tf$float32) * tf$cast(x > 0, tf$float32) * dy
  }
  list(tf$nn$relu(x), grad)
}

#' Calculates guided backpropagation for a CNN.
#' @description Calculates guided backpropagation for a CNN.
#' @import tensorflow
#' @importFrom purrr keep
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return Guided backpropagation for a CNN.
#' @export
guided_backpropagation <- function(model, input_imgs, preprocessing_function = NULL,
                             class_index = NULL, absolute_values = TRUE,
                             grayscale = TRUE, standardize = TRUE) {
  check_class_indexes(input_imgs, class_index)
  model_copy <- tf$keras$models$clone_model(model)
  set_weights(model_copy, get_weights(model))
  model_copy_layers <- model_copy$layers %>% keep(~ "activation" %in% names(.))
  for (l in model_copy_layers) {
    if (l$activation == tf$keras$activations$relu) {
      l$activation = tf$custom_gradient(guidedRelu)
    }
  }

  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  gradients <- get_input_output_gradients(model_copy, input_imgs, class_index)
  if (absolute_values) gradients <- tf$abs(gradients)
  if (grayscale) gradients <- transform_rgb_images_to_grayscale(gradients)
  if (standardize) gradients <- per_image_standardization(gradients)
  gradients$numpy()
}
