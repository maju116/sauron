#' Calculates vanilla gradient for a CNN.
#' @description Calculates vanilla gradient for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
vanilla_gradient <- function(model, input_imgs, preprocessing_function = NULL,
                             class_index = NULL) {
  check_class_indexes(input_imgs, class_index)
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  gradients <- get_input_output_gradients(model, input_imgs, class_index)
  gradients
}

#' Calculates gradientXinput for a CNN.
#' @description Calculates gradientXinput for a CNN.
#' @import tensorflow
#' @param vanilla_gradients Vanilla gradients.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @return gradientXinput for a CNN.
gradient_x_input <- function(vanilla_gradients, input_imgs, preprocessing_function) {
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  gradient_x_input <- tf$multiply(input_imgs, vanilla_gradients)
  gradient_x_input
}

#' Calculates smooth-grad for a CNN.
#' @description Calculates smooth-grad for a CNN.
#' @import tensorflow
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @return smooth-grad for a CNN.
smooth_grad <- function(model, input_imgs, preprocessing_function = NULL,
                        class_index = NULL, num_samples = 5, noise_sd = 0.1) {
  check_class_indexes(input_imgs, class_index)
  smooth_gradients <- calculate_smoothed_gradients(model, input_imgs, preprocessing_function,
                                                   class_index, num_samples, noise_sd)
  smooth_gradients
}

#' Calculates smooth-gradXInput for a CNN.
#' @description Calculates gradientXinput for a CNN.
#' @import tensorflow
#' @param smooth_gradients Smooth gradients.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @return smooth-gradXinput for a CNN.
smooth_grad_x_input <- function(smooth_gradients, input_imgs, preprocessing_function) {
  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  smooth_gradients_x_input <- tf$multiply(input_imgs, smooth_gradients)
  smooth_gradients_x_input$numpy()
}

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
#' @return integrated gradients for a CNN.
#' @export
integrated_gradients <- function(model, input_imgs, preprocessing_function = NULL,
                                 class_index = NULL, steps = 20) {
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
  integrated_gradients
}

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
  if (is.null(class_index)) {
    if (!is.null(preprocessing_function)) {
      input_imgs_temp <- preprocessing_function(input_imgs)
    }
    preds <- model(input_imgs_temp)
    class_index <- tf$math$argmax(preds, axis = as.integer(1))$numpy()
    if (length(class_index) == 1) class_index <- class_index + 1
  }

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

#' Calculates guided backpropagation for a CNN.
#' @description Calculates guided backpropagation for a CNN.
#' @import tensorflow
#' @importFrom purrr keep
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @return Guided backpropagation for a CNN.
#' @export
guided_backpropagation <- function(model, input_imgs, preprocessing_function = NULL,
                                   class_index = NULL) {
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
  gradients
}

#' Calculates Guided Grad-CAM for a CNN.
#' @description Calculates Guided Grad-CAM for a CNN.
#' @import tensorflow
#' @importFrom purrr keep
#' @param model Tensorflow model.
#' @param input_imgs Input images if for of 4-D tensor.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @return Guided Grad-CAM for a CNN.
#' @export
guided_grad_cam <- function(model, input_imgs, preprocessing_function = NULL,
                            class_index = NULL) {
  check_class_indexes(input_imgs, class_index)
  last_conv2d <- find_last_conv2d_layer(model)
  grad_model <- keras_model(
    inputs = model$input,
    outputs = list(last_conv2d$output, model$output)
  )

  if (!is.null(preprocessing_function)) {
    input_imgs <- preprocessing_function(input_imgs)
  }
  images <-  tf$cast(input_imgs, tf$float32)
  with(tf$GradientTape() %as% t, {
    t$watch(images)
    c_p <- grad_model(images)
    conv_outputs <- c_p[[1]]
    preds <- c_p[[2]]
    top_class <- get_model_predictions_based_on_indexes(preds, class_index)
  })
  grads <- t$gradient(top_class, conv_outputs)

  grads <- (
    tf$cast(conv_outputs > 0, tf$float32)
    * tf$cast(grads > 0, tf$float32)
    * grads
  )
  weights <- tf$reduce_mean(grads, axis = as.integer(c(1, 2))) %>%
    tf$expand_dims(axis = as.integer(1)) %>%
    tf$expand_dims(axis = as.integer(1))
  tf$reduce_sum(tf$multiply(weights, conv_outputs), axis = as.integer(-1)) %>%
    tf$expand_dims(axis = as.integer(-1))
}

#' Generates explanations for images.
#' @description Generates explanations for images.
#' @import keras
#' @importFrom purrr map
#' @param model Tensorflow model.
#' @param input_imgs_paths Input images paths.
#' @param id Explainer id.
#' @param preprocessing_function Preprocessing function. Default to `NULL`.
#' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
#' @param methods Methods to be calculated.
#' @param batch_size Batch size.
#' @param num_samples Number of noised samples per one image.
#' @param noise_sd Gaussian noise standard deviation.
#' @param steps Integration steps. Must be positive integer.
#' @param patch_size Patch size. 2-D `integer` vector.
#' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
#' @return Explanations for images.
generate_cnn_explanations <- function(model, input_imgs_paths, id,
                                      preprocessing_function = NULL,
                                      class_index = NULL,
                                      methods = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC"),
                                      batch_size = length(input_imgs_paths),
                                      num_samples = 5, noise_sd = 0.1,
                                      steps = 20, patch_size = c(50, 50),
                                      grayscale = TRUE) {
  target_size <- unlist(model$input$get_shape()$as_list())
  n_batches <- ceiling(length(input_imgs_paths) / batch_size)
  id_batches <- 1:n_batches %>% map(~ {
    ((.x - 1) * batch_size + 1):min((.x * batch_size), length(input_imgs_paths))
  })

  explanations <- id_batches %>% map(~ {
    input_imgs <- input_imgs_paths[.x] %>% map(~ {
      image_load(., target_size = target_size[1:2]) %>%
        image_to_array() %>% array_reshape(dim = c(1, target_size))
    }) %>% abind(along = 1)

    batch_explanations <- list(Input = input_imgs)
    if ("V" %in% methods || "GI" %in% methods) {
      vanilla_gradients <- vanilla_gradient(model, input_imgs, preprocessing_function, class_index)
      if ("V" %in% methods) {
        batch_explanations[["V"]] <- vanilla_gradients %>%
          transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
      }
      if ("GI" %in% methods) {
        batch_explanations[["GI"]] <- gradient_x_input(vanilla_gradients, input_imgs, preprocessing_function) %>%
          transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
      }
    }
    if ("SG" %in% methods || "SGI" %in% methods) {
      smooth_gradients <- smooth_grad(model, input_imgs, preprocessing_function,
                                      class_index, num_samples, noise_sd)
      if ("SG" %in% methods) {
        batch_explanations[["SG"]] <- smooth_gradients %>%
          transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
      }
      if ("SGI" %in% methods) {
        batch_explanations[["SGI"]] <- smooth_grad_x_input(smooth_gradients, input_imgs, preprocessing_function) %>%
          transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
      }
    }
    if ("IG" %in% methods) {
      batch_explanations[["IG"]] <- integrated_gradients(model, input_imgs, preprocessing_function,
                                                         class_index, steps) %>%
        transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
    }
    if ("GB" %in% methods) {
      batch_explanations[["GB"]] <- guided_backpropagation(model, input_imgs, preprocessing_function,
                                                           class_index) %>%
        transform_and_standarize_images(absolute_values = TRUE, grayscale = grayscale, standardize = TRUE)
    }
    if ("OCC" %in% methods) {
      batch_explanations[["OCC"]] <- occlusion(model, input_imgs, preprocessing_function,
                                               class_index, patch_size)
    }
    if ("GGC" %in% methods) {
      batch_explanations[["GGC"]] <- guided_grad_cam(model, input_imgs, preprocessing_function,
                                                     class_index) %>%
        sweep(., 1, apply(., 1, min), "-") %>%
        sweep(., 1, apply(., 1, max) - apply(., 1, min), "/")
      batch_explanations[["GGC"]] <- tf$image$resize(batch_explanations[["GGC"]], as.integer(dim(input_imgs)[2:3]))$numpy()
    }
    batch_explanations
  }) %>% pmap(., abind, along = 1) %>% list()
  names(explanations) <- id
  CNNexplanations$new(explanations, id, list(target_size), methods, input_imgs_paths)
}
