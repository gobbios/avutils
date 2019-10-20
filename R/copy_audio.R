
#' copy audio files with optional silence added at the end
#'
#' @param from character, file path for the source
#' @param to character, file path to the target location
#' @param appendsilence numeric, number of seconds for added silence at the end of the file. \code{NULL} by default where the file is simply copied without any processing through \code{sox}.
#' @param pathtosox character, path to sox binary
#'
#' @details In case you want to add silence to the audio file, \code{sox} is required (see \code{\link{set_binaries}} for details).
#' @return logical, indicating whether the file \code{to} exists, i.e. whether copying was successful.
#' @export
copy_audio <- function(from, to, appendsilence = NULL, pathtosox = getOption("avutils_sox")) {
  res <- FALSE
  if (is.null(appendsilence)) {
    res <- file.copy(from = from, to = to)
  }

  if (!is.null(appendsilence)) {
    from <- normalizePath(from)
    to <- normalizePath(to, mustWork = FALSE)
    cm <- paste(from, to, "pad 0", appendsilence)
    xres <- system2(command = pathtosox, args = cm, stdout = TRUE, stderr = TRUE)
    res <- file.exists(to)
  }
  file.exists(to)
}
