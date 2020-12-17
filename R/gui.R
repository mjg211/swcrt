#' Graphical user interface to package functionality
#'
#' \code{gui()} runs an R Shiny web browser based graphical user interface for
#' running functions within \code{\link{swcrt}}.
#'
#' @examples
#' # Launch the graphical user interface
#' \dontrun{gui()}
#' @seealso \code{\link{opt_sw_norm}}, \code{\link{plot.swcrt_opt_sw_norm}}.
#' @export
gui <- function() {
  app_dir <- system.file("shiny", "swcrt", package = "swcrt")
  if (app_dir == "") {
    stop("Could not find required directory for Shiny graphical user ",
         "interface. Try re-installing swcrt.")
  }
  shiny::runApp(app_dir, launch.browser = TRUE, display.mode = "normal")
}
