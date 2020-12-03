#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "xai_app", package = "sauron")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `sauron`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
