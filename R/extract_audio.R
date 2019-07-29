#' extract mono wav file from video source
#'
#' @param videofile character, path to video file(s)
#' @param pathout character, path where the audio should be saved (by default the same as the video file)
#' @param messages logical, should the file names of each processed file be printed
#' @param progbar logical, should a progressbar be printed
#' @param pathtoffmpeg character, the path to the ffmpeg binary
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @return a character string with the path to the written audio file
#' @export
#'

extract_audio <- function(videofile,
                          pathout = NULL,
                          messages = TRUE,
                          progbar = FALSE,
                          pathtoffmpeg = getOption("avutils_ffmpeg")) {
  xin <- normalizePath(videofile)
  if (!file.exists(pathtoffmpeg)) stop("ffmpeg binary not found", call. = FALSE)

  if (progbar) pb <- txtProgressBar(min = 0, max = length(xin),
                                    initial = 0, style = 3)

  res <- matrix(ncol = 2, nrow = length(xin), "")

  for (i in 1:length(xin)) {
    if (!file.exists(xin[i])) stop("video file not found", call. = FALSE)


    temp <- unlist(strsplit(x = xin[i], split = "/", fixed = TRUE))
    newfilename <- unlist(strsplit(x = temp[length(temp)],
                                   split = ".",
                                   fixed = TRUE))
    newfilename[length(newfilename)] <- "wav"
    newfilename <- paste(newfilename, collapse = ".")
    if (is.null(pathout)) {
      temp[length(temp)] <- newfilename
      targetloc <- paste(temp, collapse = "/")
    } else {
      targetloc <- paste(pathout, newfilename, sep = "/")
    }

    cmargs <- paste("-i",
                    xin[i],
                    "-y -ar 44100 -ac 1",
                    targetloc, "-hide_banner")
    xres <- system2(command = normalizePath(pathtoffmpeg),
                    args = cmargs,
                    stderr = TRUE,
                    stdout = TRUE)
    res[i, 1] <- xin[i]
    res[i, 2] <- targetloc
    if (messages) message(xin[i], "  -->  ", targetloc)
    if (progbar) setTxtProgressBar(pb = pb, value = i)
  }
  data.frame(input = res[, 1], targetloc = res[, 2], stringsAsFactors = FALSE)
}

