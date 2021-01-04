#' Creates `CNNexplainers` object.
#' @description Creates `CNNexplainers` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplainers` object.
#' @export
CNNexplainers <- R6::R6Class(
  classname = "CNNexplainers",
  public = list(
    #' @field explainers List of `CNNexplainer` objects.
    explainers = NULL,
    #' @description Initializes `CNNexplainers` object.
    #' @param ... `CNNexplainer` objects.
    initialize = function(...) {
      if (list(...) %>% map_lgl(~ !("CNNexplainer" %in% class(.))) %>% any()) {
        stop("You can only pass objects of class 'CNNexplainer'.
             Please check your 'explainers' list.")
      }
      self$explainers <- list(...)
    },
    #' @description Prints available explanation methods.
    show_available_methods = function() {
      private$available_methods
    },
    #' @description Generates explanations.
    #' @param input_imgs_paths Input images paths.
    #' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
    #' @param methods Methods to be calculated.
    #' @param batch_size Batch size.
    #' @param num_samples Number of noised samples per one image.
    #' @param noise_sd Gaussian noise standard deviation.
    #' @param steps Integration steps. Must be positive integer.
    #' @param patch_size Patch size. 2-D `integer` vector.
    #' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
    #' @return Explanations for images.
    explain = function(input_imgs_paths,
                       class_index = NULL,
                       methods = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC", "GGC"),
                       batch_size = length(input_imgs_paths),
                       num_samples = 5, noise_sd = 0.1,
                       steps = 20, patch_size = c(50, 50),
                       grayscale = TRUE) {
      multimodel_explainations <- self$explainers %>% map(~ {
        current_explainer <- .x
        current_explainer$explain(input_imgs_paths,
                                  class_index,
                                  methods,
                                  batch_size,
                                  num_samples, noise_sd,
                                  steps, patch_size,
                                  grayscale)
      })
      for (ex in 2:length(self$explainers)) {
        multimodel_explainations[[1]]$combine(multimodel_explainations[[ex]])
      }
      multimodel_explainations[[1]]
    }
  ),
  private = list(
    available_methods = filter_methods_by_network("CNN")
  )
)

#' Creates `CNNexplainer` object.
#' @description Creates `CNNexplainer` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplainer` object.
#' @export
CNNexplainer <- R6::R6Class(
  classname = "CNNexplainer",
  public = list(
    #' @field model Tensorflow model.
    model = NULL,
    #' @field preprocessing_function Image preprocessing function.
    preprocessing_function = NULL,
    #' @field id Explainer id.
    id = NULL,
    #' @description Initializes `CNNexplainer` object.
    #' @param model Tensorflow model.
    #' @param preprocessing_function Image preprocessing function.
    #' @param id Explainer id.
    initialize = function(model, preprocessing_function, id = NULL) {
      self$model <- model
      self$preprocessing_function <- preprocessing_function
      self$id <- if (is.null(id) || class(id) != "character" || id == "") {
        paste0(sample(letters, 10), collapse = "")
      } else {
        id
      }
    },
    #' @description Prints available explanation methods.
    show_available_methods = function() {
      private$available_methods
    },
    #' @description Generates explanations.
    #' @param input_imgs_paths Input images paths.
    #' @param class_index Class index. If set to `NULL` index with max predicted probability will be selected.
    #' @param methods Methods to be calculated.
    #' @param batch_size Batch size.
    #' @param num_samples Number of noised samples per one image.
    #' @param noise_sd Gaussian noise standard deviation.
    #' @param steps Integration steps. Must be positive integer.
    #' @param patch_size Patch size. 2-D `integer` vector.
    #' @param grayscale Boolean. Should gradients be converted from RGB to grayscale.
    #' @return Explanations for images.
    explain = function(input_imgs_paths,
                       class_index = NULL,
                       methods = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC", "GGC"),
                       batch_size = length(input_imgs_paths),
                       num_samples = 5, noise_sd = 0.1,
                       steps = 20, patch_size = c(50, 50),
                       grayscale = TRUE) {
      generate_cnn_explanations(self$model, input_imgs_paths,
                                self$id, self$preprocessing_function,
                                class_index,
                                methods,
                                batch_size,
                                num_samples, noise_sd,
                                steps, patch_size,
                                grayscale)
    }
  ),
  private = list(
    available_methods = filter_methods_by_network("CNN")
  )
)
