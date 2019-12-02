#' set options with locations to ffmpeg and sox binaries
#'
#' @param pathtoffmpeg character, path to ffmpeg binary, by default looks for it in the path variable
#' @param pathtosox character, path to sox binary, by default looks for it in the path variable
#' @param printmessages logical, should messages be returned
#' @details this function sets some of R's global options.
#'
#' the settings are temporary, i.e. the settings are lost when R is restarted
#' @return a message
#' @export
#'

set_binaries <- function(pathtoffmpeg = Sys.which("ffmpeg"), pathtosox = Sys.which("sox"), printmessages = TRUE) {

  if (!is.null(pathtoffmpeg)) {
    if (pathtoffmpeg == "") {
      if (printmessages) message("ffmpeg not in the path variable")
    } else {
      options(avutils_ffmpeg = normalizePath(pathtoffmpeg, winslash = "/"))
      if (printmessages) message("ffmpeg set to this location: ",
                                 as.character(getOption("avutils_ffmpeg")))
    }
  }

  if (!is.null(pathtosox)) {
    if (pathtosox == "") {
      if (printmessages) message("sox not in the path variable")
    } else {
      options(avutils_sox = normalizePath(pathtosox, winslash = "/"))
      if (printmessages) message("sox set to this location: ",
                                 as.character(getOption("avutils_sox")))
    }
  }

}
