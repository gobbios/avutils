#' test whether sox, ffmpeg, git and vagrant binaries work
#'
#' @param printmessages logical, should messages be returned
#' @return a message
#' @importFrom tuneR sine
#' @export
#'

test_binaries <- function(printmessages = TRUE) {
  allres <- c(sox = FALSE, ffmpeg = FALSE, git = FALSE, vagrant = FALSE)

  if (!is.null(getOption("avutils_ffmpeg"))) {
    res <- system2(command = getOption("avutils_ffmpeg"),
                   args = "-version",
                   stdout = TRUE,
                   stderr = TRUE)
    if (length(res) > 0) {
      res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))[3]
      if (printmessages) message("ffmpeg seems to work; version: ", res)
      allres["ffmpeg"] <- TRUE
    }
  } else {
    if (printmessages) message("ffmpeg seems not to work...")
  }

  if (!is.null(getOption("avutils_sox"))) {
    res <- system2(command = getOption("avutils_sox"),
                   args = "--version",
                   stdout = TRUE,
                   stderr = TRUE)
    if (length(res) > 0) {
      res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))
      res <- res[length(res)]
      if (printmessages) message("sox seems to work; version: ", res)
      allres["sox"] <- TRUE
    }
  } else {
    if (printmessages) message("sox seems not to work...")
  }



  res <- tryCatch(system2(command = "git",
                          args = "--version",
                          stderr = TRUE,
                          stdout = TRUE),
                  error = function(e){})
  if (length(res) > 0) {
    if (printmessages) message("git seems to work; version: ", res)
    allres["git"] <- TRUE
  } else {
    if (printmessages) message("git not found")
  }

  res <- tryCatch(system2(command = "vagrant",
                          args = "--version",
                          stderr = TRUE,
                          stdout = TRUE),
                  error = function(e){})
  if (length(res) > 0) {
    if (printmessages) message("vagrant seems to work; version: ", res)
    allres["vagrant"] <- TRUE
  } else {
    if (printmessages) message("vagrant not found")
  }

  invisible(allres)
}
