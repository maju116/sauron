#' Creates `CNNexplanations` object.
#' @description Creates `CNNexplanations` object.
#' @import R6
#' @importFrom dplyr select filter
#' @return `CNNexplanations` object.
#' @export
CNNexplanations <- R6::R6Class(
  classname = "CNNexplanations",
  public = list(
    #' @description Initializes `CNNexplanations` object.
    #' @param explanations Explanations.
    #' @param ids Explainers ids.
    #' @param target_sizes Target sizes list.
    #' @param methods Explanation methods.
    #' @param input_imgs_paths Input images paths.
    initialize = function(explanations, ids, target_sizes, methods, input_imgs_paths) {
      private$check_inputs(explanations, ids, target_sizes, methods, input_imgs_paths)
      private$explanations = explanations
      private$metadata = list(
        multimodel_explanations = length(ids) > 1,
        ids = ids,
        n_models = length(ids),
        target_sizes = target_sizes,
        methods = methods,
        input_imgs_paths = input_imgs_paths,
        n_imgs = length(input_imgs_paths)
      )
    },
    #' @description Prints basic information about object.
    print = function() {
      paste("CNNexplanations object contains explanations for",
            private$metadata$n_imgs, "images for",
            private$metadata$n_models,
            if (private$metadata$multimodel_explanations) "models." else "model.") %>%
        cat()
    },
    #' @description Collects explanations from `CNNexplanations` object.
    get_explanations = function() {
      private$explanations
    },
    #' @description Collects metadata from `CNNexplanations` object.
    get_metadata = function() {
      private$metadata
    },
    #' @description Combines `CNNexplanations` objects.
    #' @param ... `CNNexplanations` objects.
    combine = function(...) {
      if (list(...) %>% map_lgl(~ !("CNNexplanations" %in% class(.))) %>% any()) {
        stop("You can only pass objects of class 'CNNexplanations'.
             Please check your 'explanations' list.")
      }
      for (ex in list(...)) {
        current_explanations <- ex$get_explanations()
        # TODO: check if have same explanations and metadata
        private$explanations <- c(private$explanations, current_explanations)
        current_metadata <- ex$get_metadata()
        private$metadata$ids <- c(private$metadata$ids, current_metadata$ids)
        private$metadata$n_models <- length(private$metadata$ids)
        private$metadata$target_sizes <- c(private$metadata$target_sizes, current_metadata$target_sizes)
        private$metadata$multimodel_explanations <- TRUE
      }
      invisible(self)
    },
    #' @description Generates raster image(s) with explanations.
    #' @param combine_plots Should images be combined.
    #' @param output_path Where to save explanation plots.
    #' @param plot Should explanation be plotted.
    plot_and_save = function(combine_plots, output_path = NULL, plot = TRUE) {
      explanation_plots <- private$explanations %>% map(~ {
        create_cnn_explanation_plots(.)
      })
      pixel_to_cm <- 2.54 / 96
      multimodel_explanations <- private$metadata$multimodel_explanations
      n_models <- private$metadata$n_models
      if (combine_plots) {
        model_names <- private$metadata$ids
        single_img_height <- private$metadata$target_size %>% map_dbl(~ .[1]) %>% max()
        single_img_width <- private$metadata$target_size %>% map_dbl(~ .[2]) %>% max()
        n_imgs <- private$metadata$n_imgs
        explanation_plots <- if (multimodel_explanations) {
          combine_cnn_multimodel_explanation_plots(explanation_plots, n_models, n_imgs)
        } else {
          explanation_plots[[1]]
        }
        ncol <- length(private$metadata$methods) + 1
        grobs <- explanation_plots %>% imap(~ {
          explanation_name <- find_method_name(.y)
          arrangeGrob(grobs = .x, top = explanation_name, ncol = 1)
        })
        final_plot <- arrangeGrob(grobs = grobs, ncol = ncol,
                                  bottom = model_names %>%
                                    paste(collapse = ", ") %>%
                                    paste("Models order:", .))
        if (plot) plot(final_plot)
        if (!is.null(output_path)) {
          ggsave(file.path(output_path, paste0(model_names %>%
                                                 paste(collapse = "_"),
                                               "_explanations.png")),
                 plot = final_plot,
                 height = 1.1 * n_models * n_imgs * single_img_height * pixel_to_cm,
                 width = 1.1 * single_img_width * ncol * pixel_to_cm,
                 limitsize = FALSE, units = "cm")
        }
      } else {
        for (idx in 1:n_models) {
          current_explanations <- explanation_plots[[idx]]
          current_model_name <- private$metadata$ids[[idx]]
          current_single_img_height <- private$metadata$target_sizes[[idx]][1]
          current_single_img_width <- private$metadata$target_sizes[[idx]][2]
          current_explanations %>%
            save_single_explanation_plots(output_path, current_model_name,
                                          basename(private$metadata$input_imgs_paths),
                                          current_single_img_height,
                                          current_single_img_width, plot)
        }
      }

    }
  ),
  private = list(
    explanations = NULL,
    metadata = NULL,
    check_inputs = function(explanations, ids, target_sizes, methods, input_imgs_paths) {
      # TODO
    }
  )
)
