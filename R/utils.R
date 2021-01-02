#' Checks if `class_index` argument is correct.
#' @description Selects predictions form output based on indexes.
#' @param input_imgs Input images in form of 4-D tensor.
#' @param class_index Class index.
check_class_indexes <- function(input_imgs, class_index) {
  n_imgs <- dim(input_imgs)[1]
  if (
    !(is.null(class_index) |
      (is.numeric(class_index) & length(class_index) == 1) |
      (is.numeric(class_index) & length(class_index) == n_imgs))
  ) {
    stop("'class_index' must be `NULL` or integer vector of length 1 or N images!")
  }
}

#' Selects predictions form output based on indexes.
#' @description Selects predictions form output based on indexes.
#' @param preds Prediction tensor.
#' @param class_index Class index.
#' @return Predictions form output based on indexes
get_model_predictions_based_on_indexes <- function(preds, class_index) {
  if (is.null(class_index)) {
    top_class <- tf$math$reduce_max(preds, axis = as.integer(1))
  } else if (length(class_index) == 1) {
    top_class <- preds[ , class_index]
  } else {
    n_imgs <- preds$get_shape()$as_list()[1]
    class_index <- cbind(as.integer(0:(n_imgs - 1)), as.integer(class_index))
    top_class <- tf$gather_nd(preds, class_index)
  }
  top_class
}

#' Calculates gradient between input and output layers.
#' @description Calculates gradient between input and output layers.
#' @import keras
#' @import tensorflow
#' @importFrom magrittr %>%
#' @param model Tensorflow model.
#' @param input_imgs Input images in form of 4-D tensor.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @return Gradient between input and output layers.
#' @export
get_input_output_gradients <- function(model, input_imgs, class_index) {
  images <-  tf$cast(input_imgs, tf$float32)

  with(tf$GradientTape() %as% t, {
    t$watch(images)
    preds <- model(images)
    top_class <- get_model_predictions_based_on_indexes(preds, class_index)
  })
  t$gradient(top_class, images)
}

#' Transforms RGB images to grayscale.
#' @description Transforms RGB images to grayscale.
#' @import tensorflow
#' @param rgb_images RGB images.
#' @return Grayscale images.
#' @export
transform_rgb_images_to_grayscale <- function(rgb_images) {
  tf$expand_dims(tf$reduce_sum(rgb_images, axis = as.integer(-1)), axis = as.integer(-1))
}

#' Per image standardization.
#' @description Per image standardization.
#' @import tensorflow
#' @param images Images.
#' @return Standardized images.
#' @export
per_image_standardization <- function(images) {
  tf$cast(
    255 * tf$image$per_image_standardization(images), tf$uint8
  )
}

#' Transforms and standardizes gradients.
#' @description Transforms and standardizes gradients.
#' @param gradients Tensor with gradients.
#' @param absolute_values Boolean. If `TRUE` absolute values of gradients will be returned.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @param standardize Boolean. Should gradients be standardized.
#' @return Transformed and standardized gradients.
#' @export
transform_and_standarize_images <- function(gradients, absolute_values, grayscale, standardize) {
  if (absolute_values) gradients <- tf$abs(gradients)
  if (grayscale) gradients <- transform_rgb_images_to_grayscale(gradients)
  if (standardize) gradients <- per_image_standardization(gradients)
  gradients$numpy()
}

#' Generates noisy images.
#' @description Generates noisy images.
#' @import tensorflow
#' @param input_imgs Input images if for of 4-D tensor.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @return Noised images.
generate_noisy_images <- function(input_imgs, num_samples, noise_sd) {
  input_imgs <- tf$image$convert_image_dtype(input_imgs, dtype = tf$float32, saturate = FALSE)
  images_copy <- tf$keras$backend$repeat_elements(input_imgs, as.integer(num_samples), axis = as.integer(0))
  noise <- 255 * tf$random$normal(shape = tf$shape(images_copy), mean = 0.0, stddev = noise_sd, dtype = tf$float32)
  tf$clip_by_value(tf$add(images_copy, noise), 0, 255)$numpy()
}

#' Calculates smooth gradients for a CNN.
#' @description Calculates smooth gradients for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @return smooth gradients for a CNN.
calculate_smoothed_gradients <- function(model, input_imgs, preprocessing_function,
                                         class_index, num_samples, noise_sd) {
  noised_imgs <- generate_noisy_images(input_imgs, num_samples, noise_sd)
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
    noised_imgs <- preprocessing_function(noised_imgs)
  }

  if (length(class_index > 1)) {
    class_index <- rep(class_index, each = num_samples)
  } else if (is.null(class_index)) {
    preds <- model(tf$cast(input_imgs, tf$float32))
    class_index <- tf$argmax(preds, axis = as.integer(1))$numpy()
    class_index <- rep(class_index, each = num_samples)
  }

  gradients <- get_input_output_gradients(model, noised_imgs, class_index)
  gradients <- tf$reshape(gradients,
                          shape = as.integer(c(-1, num_samples, dim(input_imgs)[2:4])))
  smooth_gradients <- tf$reduce_mean(gradients, axis = as.integer(1))
  smooth_gradients
}

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

#' Finds last Conv2D layer in the network.
#' @description Finds last Conv2D layer in the network.
#' @param model Tensorflow model.
#' @return Last Conv2D layer in the network.
find_last_conv2d_layer <- function(model) {
  model$layers %>% keep(~ length(.$output_shape) == 4) %>%
    rev() %>% .[[1]]
}
