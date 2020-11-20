#' Acronyms and names for available explanation methods.
#' @description Acronyms and names for available explanation methods.
#' @import tibble
#' @export
sauron_available_methods <- tibble::tibble(
  method = c("V", "GI", "SG", "SGI", "IG", "GB", "OCC"),
  name = c("Vanilla gradient",
           "Gradient x Input",
           "SmoothGrad",
           "SmoothGrad x Input",
           "Integrated Gradients",
           "Guided Backpropagation",
           "Occlusion Sensitivity"),
  network_type = "CNN"
)

#' Filters available methods by type.
#' @description Filters available methods by type.
#' @return Filtered `tibble` with available method for a network type.
filter_methods_by_network <- function(network_type) {
  sauron_available_methods %>%
    filter(network_type == network_type) %>%
    dplyr::select(-network_type)
}

#' Finds explanation method name.
#' @description Finds explanation method name.
#' @param method_acc Method acronym.
#' @return Method full name.
find_method_name <- function(method_acc) {
  if (method_acc %in% sauron_available_methods$method) {
    sauron_available_methods %>%
      dplyr::filter(method == method_acc) %>%
      pull(name)
  } else {
    method_acc
  }
}
