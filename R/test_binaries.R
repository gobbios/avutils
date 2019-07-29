#' test whether sox and ffmpeg binaries work
#'
#' @return a message
#' @importFrom tuneR sine
#' @export
#'

test_binaries <- function() {
  res <- system(paste(getOption("avutils_ffmpeg"), "-version"), intern = TRUE)
  if(length(res) > 0) {
    res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))[3]
    message("ffmpeg seems to work; version: ", res)
  }

  res <- system(paste(getOption("avutils_sox"), "--version"), intern = TRUE)
  if(length(res) > 0) {
    res <- unlist(strsplit(res[1], split = " ", fixed = TRUE))
    res <- res[length(res)]
    message("sox seems to work; version: ", res)
  }
}

