#' Calculates gradient between input and output layers.
#' @description Calculates gradient between input and output layers.
#' @import keras
#' @import tensorflow
#' @importFrom magrittr %>%
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @return Gradient between input and output layers.
#' @export
get_input_output_gradients <- function(model, input_imgs, class_index = NULL) {
  images <-  tf$cast(input_imgs, tf$float32)

  with(tf$GradientTape() %as% t, {
    t$watch(images)
    preds <- model(images)
    if (is.null(class_index)) {
      top_class <- tf$math$reduce_max(preds, axis = as.integer(1))
    } else {
      top_class <- preds[ , class_index]
    }
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
  tf$reduce_sum(rgb_images, axis = as.integer(-1))
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
