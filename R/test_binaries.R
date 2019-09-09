#' test whether sox and ffmpeg binaries work
#'
#' @return a message
#' @importFrom tuneR sine
#' @export
#'

test_binaries <- function() {
  res <- system2(command = getOption("avutils_ffmpeg"),
                 args = "-version",
                 stdout = TRUE,
                 stderr = TRUE)
  if (length(res) > 0) {
    res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))[3]
    message("ffmpeg seems to work; version: ", res)
  }

  res <- system2(command = getOption("avutils_sox"),
                 args = "--version",
                 stdout = TRUE,
                 stderr = TRUE)
  if (length(res) > 0) {
    res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))
    res <- res[length(res)]
    message("sox seems to work; version: ", res)
  }

  res <- tryCatch(system2(command = "git",
                          args = "--version",
                          stderr = TRUE,
                          stdout = TRUE),
                  error = function(e){})
  if (length(res) > 0) {
    message("git seems to work; version: ", res)
  } else {
    message("git not found")
  }

  res <- tryCatch(system2(command = "vagrant",
                          args = "--version",
                          stderr = TRUE,
                          stdout = TRUE),
                  error = function(e){})
  if (length(res) > 0) {
    message("vagrant seems to work; version: ", res)
  } else {
    message("vagrant not found")
  }

}
