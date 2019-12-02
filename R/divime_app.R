#' interactive user interface for avutils
#'
#' @param launchinbrowser logical, should the app start in your default browser (by default \code{TRUE})
#'
#' @return launches a \code{shiny} app
#' @export
#' @author Christof Neumann
#'
#' @examples
#' \dontrun{
#' divime_app()
#' }
divime_app <- function(launchinbrowser = TRUE) {
  appDir <- system.file("divime_tools",
                        package = "avutils")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `avutils`.",
         call. = FALSE)
  }
  shiny::runApp(appDir,
                display.mode = "normal",
                launch.browser = launchinbrowser)
}
