#' Calculates integrated gradients for a CNN.
#' @description Calculates integrated gradients for a CNN.
#' @import tensorflow
#' @importFrom purrr map2
#' @importFrom abind abind
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param steps Integration steps. Must be positive integer.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return integrated gradients for a CNN.
#' @export
integrated_gradients <- function(model, input_imgs, preprocessing_function = NULL,
                                 class_index = NULL, steps = 20, absolute_values = TRUE,
                                 grayscale = TRUE, standardize = TRUE) {
  check_class_indexes(input_imgs, class_index)
  n_imgs <- dim(input_imgs)[1]
  sample_index <- rep(1:n_imgs, each = steps + 1)
  steps_index <- rep(0:steps, n_imgs)
  baseline <- tf$zeros(c(1L, dim(input_imgs)[2:4]))$numpy()
  interpolated_imgs <- map2(sample_index, steps_index, ~ {
    img <- input_imgs[.x, , , , drop = FALSE]
    step <- .y
    baseline + step / steps * (img - baseline)
  }) %>% abind(along = 1)

  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
    interpolated_imgs <- preprocessing_function(interpolated_imgs)
  }

  if (length(class_index > 1)) {
    class_index <- rep(class_index, each = steps + 1)
  } else if (is.null(class_index)) {
    preds <- model(tf$cast(input_imgs, tf$float32))
    class_index <- tf$argmax(preds, axis = as.integer(1))$numpy()
    class_index <- rep(class_index, each = steps + 1)
  }

  gradients <- get_input_output_gradients(model, interpolated_imgs, class_index)
  gradients <- tf$reshape(gradients,
                          shape = as.integer(c(-1, steps + 1, dim(input_imgs)[2:4])))
  integrated_gradients <- (gradients[ , 1:steps, , , ] + gradients[ , 2:(steps + 1), , , ]) / 2
  integrated_gradients <- tf$reduce_mean(integrated_gradients, axis = as.integer(1))
  integrated_gradients <- integrated_gradients * tf$image$convert_image_dtype(input_imgs, dtype = tf$float32, saturate = FALSE)

  if (absolute_values) integrated_gradients <- tf$abs(integrated_gradients)
  if (grayscale) integrated_gradients <- transform_rgb_images_to_grayscale(integrated_gradients)
  if (standardize) integrated_gradients <- per_image_standardization(integrated_gradients)
  integrated_gradients$numpy()
}

