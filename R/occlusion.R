#' Calculates occlusion sensitivity for a CNN.
#' @description Calculates occlusion sensitivity for a CNN.
#' @import tensorflow
#' @importFrom purrr pwalk
#' @importFrom dplyr mutate rowwise
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param patch_size Patch size. 2-D `integer` vector.
#' @return Occlusion sensitivity for a CNN.
#' @export
occlusion <- function(model, input_imgs, preprocessing_function = NULL,
                      class_index = NULL, patch_size = c(50, 50)) {
  check_class_indexes(input_imgs, class_index)

  n_imgs <- dim(input_imgs)[1]
  image_size <- dim(input_imgs)[2:3]
  sensitivity_hw <- ceiling(image_size / patch_size)
  sensitivity <- array(0, dim = c(dim(input_imgs)[1:3], 1))

  patched_input_imgs_coords <- expand.grid(1:sensitivity_hw[1], 1:sensitivity_hw[2]) %>%
    rename(h = Var1, w = Var2) %>%
    rowwise() %>%
    mutate(h_start = (h - 1) * patch_size[1] + 1,
           h_end = min(h * patch_size[1], image_size[1]),
           w_start = (w - 1) * patch_size[2] + 1,
           w_end = min(w * patch_size[2], image_size[2]))

  patched_input_imgs_coords %>%
    pwalk(function(h, w, h_start, h_end, w_start, w_end) {
      img_copy <- input_imgs
      img_copy[ , h_start:h_end, w_start:w_end, ] <- 127.5
      if (!is.null(preprocessing_function)) {
        img_copy <- preprocessing_function(img_copy)
      }
      preds <- model(img_copy)
      preds_for_class <- get_model_predictions_based_on_indexes(preds, class_index)$numpy()
      sensitivity[ , h_start:h_end, w_start:w_end, 1] <<- preds_for_class
    })
  1 - sensitivity
}
