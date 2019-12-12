#' extract sound file from video source
#'
#' ouput is fixed to 16bit 44.1k mono wav, ready for DiViMe tools
#'
#' @param videofile character, path to video file(s)
#' @param pathout character, path where the audio should be saved (by default the same as the video file)
#' @param messages logical, should the file names of each processed file be printed
#' @param progbar logical, should a progressbar be printed
#' @param pathtoffmpeg character, the path to the ffmpeg binary
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return a \code{data.frame} with locations of input and output files
#' @export
#'

extract_audio <- function(videofile,
                          pathout = NULL,
                          messages = TRUE,
                          progbar = FALSE,
                          pathtoffmpeg = getOption("avutils_ffmpeg")) {
  xin <- normalizePath(videofile, winslash = "/", mustWork = FALSE)
  if (!file.exists(pathtoffmpeg)) stop("ffmpeg binary not found", call. = FALSE)

  if (progbar) pb <- txtProgressBar(min = 0, max = length(xin),
                                    initial = 0, style = 3)

  res <- matrix(ncol = 2, nrow = length(xin), "")

  for (i in 1:length(xin)) {
    if (!file.exists(xin[i])) stop("video file not found", call. = FALSE)

    # create output file names with locations
    if (is.null(pathout)) {
      targetloc <- paste0(file_path_sans_ext(xin[i]), ".wav")
    } else {
      pathout <- normalizePath(pathout, winslash = "/")
      targetloc <- file.path(pathout, basename(paste0(file_path_sans_ext(xin[i]), ".wav")))
    }

    # create shell command
    cmargs <- paste("-i",
                    shQuote(xin[i]),
                    "-y -ar 44100 -ac 1",
                    shQuote(targetloc),
                    "-hide_banner")
    xres <- suppressWarnings(
      system2(command = normalizePath(pathtoffmpeg, winslash = "/"),
              args = cmargs,
              stderr = TRUE,
              stdout = TRUE))
    res[i, 1] <- xin[i]
    if (is.null(attr(xres, "status"))) {
      res[i, 2] <- targetloc
      if (messages) message(xin[i], "  -->  ", targetloc)
    } else {
      if (attr(xres, "status") == 1) {
        warning("file ", xin[i], " seems to be broken and has not been processed",
                call. = FALSE)
        res[i, 2] <- NA
        if (messages) message("!!! ", xin[i], "  X-X-X>X  ", targetloc, " !!!")
      }
    }

    if (progbar) setTxtProgressBar(pb = pb, value = i)
  }
  if (progbar) close(pb)
  data.frame(input = res[, 1], targetloc = res[, 2], stringsAsFactors = FALSE)
}
