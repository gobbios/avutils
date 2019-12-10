#' get media information
#'
#' audio information via sox or ffmpeg
#' @param filein character, path to audio files
#' @param pathtosox character, path to sox binary
#' @param pathtoffmpeg character, path to ffmepg binary
#' @param use_sox logical, should \code{sox} be used (default is \code{TRUE}, and if \code{FALSE} \code{ffmpeg} is used)
#' @details Getting information about audio files is possible both via \code{sox} and \code{ffmpeg}. The advantage of \code{sox} is that it provides more detailed information, but does not by default work with some file types (e.g. mp3). \code{ffmpeg} on the other seems to work with more file types, but returns less detailed information. If you use the default option (i.e. \code{use_sox = TRUE}) and see missing information in the output it might be a good idea to try via \code{ffmpeg}, i.e. \code{use_sox = FALSE}.
#' @return a \code{data.frame} with features of audio files
#' @export
#'

audio_info <- function(filein, pathtosox = getOption("avutils_sox"), pathtoffmpeg = getOption("avutils_ffmpeg"), use_sox = TRUE) {
  if (use_sox) {
    if (!file.exists(pathtosox)) {
      stop("sox binary not found", call. = FALSE)
    }
    commands <- paste("--i", shQuote(filein))
    bin <- pathtosox
  } else {
    if (!file.exists(pathtoffmpeg)) {
      stop("ffmpeg binary not found", call. = FALSE)
    }
    commands <- paste("-i", shQuote(filein), "-hide_banner")
    bin <- pathtoffmpeg
  }

  filein <- normalizePath(filein, winslash = "/", mustWork = FALSE)

  out <- data.frame(filename = filein, channels = NA, samplerate = NA,
                    resol = NA, samples = NA, duration = NA, format = NA,
                    filesize = NA, filesize_sox = NA)

  for (i in 1:nrow(out)) {
    if (!file.exists(filein[i])) {
      warning(shQuote(filein[i]), " not found", call. = FALSE)
    } else {
      cm <- commands[i]
      res <- suppressWarnings(system2(command = bin,
                                      args = cm,
                                      stdout = TRUE,
                                      stderr = TRUE))

      if (use_sox) {
        if (!is.null(attr(res, "status"))) {
          if (attr(res, "status") == 1) {
            next
          }
        }
        res1 <- unlist(strsplit(x = res, split = ":", fixed = TRUE))
        res1 <- gsub(pattern = " ", replacement = "", x = res1)
        res2 <- unlist(strsplit(x = res, split = " ", fixed = TRUE))
        res3 <- unlist(strsplit(x = res, split = ":", fixed = TRUE))

        out$channels[i] <- as.numeric(res1[which(res1 == "Channels") + 1])
        out$samplerate[i] <- as.numeric(res1[which(res1 == "SampleRate") + 1])
        out$resol[i] <- as.numeric(unlist(strsplit(x = res1[which(res1 == "Precision") + 1], split = "-bit", fixed = TRUE)))
        out$samples[i] <- as.numeric(res2[which(res2 == "samples") - 1])
        out$duration[i] <- out$samples[i] / out$samplerate[i]
        out$format[i] <- res3[which(res3 == "Sample Encoding") + 1]
        out$filesize[i] <- file.size(filein[i])
        out$filesize_sox[i] <- res1[which(res1 == "FileSize") + 1]

      }

      if (!use_sox) {
        res1 <- res[grep(pattern = "  Stream ", x = res)]
        res1 <- unlist(strsplit(x = res1, split = ", "))
        out$channels[i] <- as.numeric(res1[3] == "stereo") + 1
        out$samplerate[i] <- as.numeric(unlist(strsplit(res1[2], " "))[1])

        res2 <- unlist(strsplit(x = res1, split = ", "))[1]
        res2 <- unlist(strsplit(x = res2, split = "Audio: "))[2]
        res2 <- unlist(strsplit(x = res2, split = " "))[1]
        if (res2 == "mp3") {
          out$format[i] <- "mp3"
        }
        if (grepl(pattern = "pcm", x = res2)) {
          out$format[i] <- "pcm/wav"
        }

        out$filesize[i] <- file.size(filein[i])

        res3 <- res[grep(pattern = "  Duration: ", x = res)]
        res3 <- unlist(strsplit(x = res3, split = "Duration: "))
        res3 <- unlist(strsplit(x = res3, split = ", "))[2]
        res3 <- as.numeric(unlist(strsplit(res3, ":")))
        out$duration[i] <- res3[1]*60*60 + res3[2]*60 + res3[3]
      }
    }

  }

  if (!use_sox) {
    out$filesize_sox <- NULL
  }
  out
}

