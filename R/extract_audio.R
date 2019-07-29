
# pathtoffmpeg = "~/Documents/utilities/ffmpeg"
# videofile = "~/Downloads/00007.copy.mp4"
# pathout = NULL
#' extract mono wav file from video source
#'
#' @param videofile character, path to video file(s)
#' @param pathout character, path where the audio should be saved (by default the same as the video file)
#' @param pathtoffmpeg character, the path to the ffmpeg binary
#'
#' @return a character string with the path to the written audio file
#' @export
#'

extract_audio <- function(videofile, pathout = NULL, pathtoffmpeg = getOption("avutils_ffmpeg")) {
  xin <- normalizePath(videofile)
  res <- matrix(ncol = 2, nrow = length(xin), "")

  for (i in 1:length(xin)) {
    if (!file.exists(xin[i])) stop("video file not found", call. = FALSE)
    if (!file.exists(pathtoffmpeg)) stop("ffmpeg binary not found", call. = FALSE)

    temp <- unlist(strsplit(x = xin[i], split = "/", fixed = TRUE))
    newfilename <- unlist(strsplit(x = temp[length(temp)], split = ".", fixed = TRUE))
    newfilename[length(newfilename)] <- "wav"
    newfilename <- paste(newfilename, collapse = ".")
    if (is.null(pathout)) {
      temp[length(temp)] <- newfilename
      targetloc <- paste(temp, collapse = "/")
    } else {
      targetloc <- paste(pathout, newfilename, sep = "/")
    }

    # build command
    cm <- paste(pathtoffmpeg,
                "-i",
                xin[i],
                "-y -ar 44100 -ac 1",
                targetloc,
                "-hide_banner")

    xres <- system(command = cm, intern = TRUE)
    res[i, 1] <- xin[i]
    res[i, 2] <- targetloc
  }
  data.frame(input = res[, 1], targetloc = res[, 2], stringsAsFactors = FALSE)
}

# temp <- extractaudiofromvideo(videofile = "~/Downloads/00007.copy.mp4",
#                       pathout = NULL,
#                       pathtoffmpeg = "~/Documents/utilities/ffmpeg")
# audiofileinfo(temp, pathtosox = "~/Documents/utilities/sox")
