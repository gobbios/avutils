#' get media information
#'
#' audio information via sox
#' @param filein character, path to audio files
#' @param pathtosox character, path to sox binary
#'
#' @return a dataframe with features of audio files
#' @export
#'

audio_info <- function(filein, pathtosox = getOption("avutils_sox")) {
  if (!file.exists(pathtosox)) stop("sox binary not found", call. = FALSE)

  out <- data.frame(filename = filein, channels = NA, samplerate = NA,
                    resol = NA, samples = NA, duration = NA, format = NA,
                    filesize = NA, filesize_sox = NA)
  for (i in 1:nrow(out)) {
    if (!file.exists(filein[i])) stop("file not found", call. = FALSE)

    cm <- paste(pathtosox, "--i", paste0("'", filein[i], "'"))
    res <- system(cm, intern = TRUE)
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
  out
}

