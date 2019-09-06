#' get media information
#'
#' video information via ffmpeg
#'
#' @param filein character, path to audio file
#' @param pathtoffmpeg character, path to ffmpeg binary
#'
#' @return a data.frame with features of video files
#' @export
#'

video_info <- function(filein, pathtoffmpeg = getOption("avutils_ffmpeg")) {
  if (!file.exists(pathtoffmpeg)) stop("ffmpeg binary not found", call. = FALSE)

  filein <- normalizePath(filein, winslash = "/")

  out <- data.frame(filein = filein, video_format = NA,
                    duration = NA, bitrate = NA,
                    resol = NA, width = NA, height = NA, fps = NA,
                    hasaudio = NA)
  for (i in 1:nrow(out)) {
    if (!file.exists(filein[i])) stop("file not found", call. = FALSE)

    cmargs <- paste("-i", shQuote(filein[i]))

    res <- suppressWarnings(system2(command = normalizePath(pathtoffmpeg),
                                    args = cmargs,
                                    stderr = TRUE,
                                    stdout = TRUE))
    hasaudio <- length(res[grep("Audio: ", res)]) > 0
    duration <- res[grep("Duration: ", res)]
    res <- res[grep("Video: ", res)]
    if (length(res) == 1) {
      res <- unlist(strsplit(res, ","))
      v <- unlist(strsplit(res[1], "Video: "))
      out$video_format[i] <- v[length(v)]
      X <- res[grep("[[:digit:]]{2,5}x[[:digit:]]{2,5}", res)]
      X <- unlist(strsplit(X, " "))
      X <- X[grep("[[:digit:]]{2,5}x[[:digit:]]{2,5}", X)]
      out$resol[i] <- X[1]
      out$width[i] <- suppressWarnings(as.numeric(unlist(strsplit(X[1], "x"))[1]))
      out$height[i] <- suppressWarnings(as.numeric(unlist(strsplit(X[1], "x"))[2]))
      X <- res[grep(" fps", res)]
      X <- unlist(strsplit(X, "fps", fixed = TRUE))
      out$fps[i] <- suppressWarnings(as.numeric(X))
      out$hasaudio[i] <- hasaudio
      temp <- unlist(strsplit(duration, " ", fixed = TRUE))
      temp <- unlist(strsplit(temp, ",", fixed = TRUE))
      temp <- temp[temp != ""]
      dur <- as.numeric(unlist(strsplit(temp[2], ":")))
      out$duration[i] <- dur[1] * 3600 + dur[2] * 60 + dur[3]
      out$bitrate[i] <- as.numeric(temp[6])

    } else {
      out$video_format[i] <- "not recognized"
    }
  }
  out
}
