#' set options with locations to ffmpeg and sox binaries
#'
#' @param pathtoffmpeg character, path to ffmpeg binary, by default looks for it in the path variable
#' @param pathtosox character, path to sox binary, by default looks for it in the path variable
#' @details this function sets some of R's global options.
#'
#' these settings are presumably temporary, i.e. the settings are lost when R is restarted
#' @return a message
#' @export
#'

set_binaries <- function(pathtoffmpeg = Sys.which("ffmpeg"), pathtosox = Sys.which("sox")) {
  if (pathtoffmpeg == "") {
    message("ffmpeg not in the path variable")
  } else {
    options(avutils_ffmpeg = normalizePath(pathtoffmpeg, winslash = "/"))
    message("ffmpeg set to this location: ", as.character(getOption("avutils_ffmpeg")))
  }
  if (pathtosox == "") {
    message("sox not in the path variable")
  } else {
    options(avutils_sox = normalizePath(pathtosox, winslash = "/"))
    message("sox set to this location: ", as.character(getOption("avutils_sox")))
  }
}

