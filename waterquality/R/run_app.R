#' Launch the waterquality Shiny app
#'
#' Opens the interactive explorer for the `yarra_wq` dataset.
#'
#' @return Invisible NULL (runs for side effects).
#' @export
#' @examples
#' \dontrun{ waterquality::run_app() }
run_app <- function() {
  app_dir <- system.file("app", package = "waterquality")
  if (app_dir == "") stop("App directory not found. Try reinstalling the package.", call. = FALSE)
  shiny::runApp(app_dir, display.mode = "normal")
}
