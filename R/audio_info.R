#' get information about audio file via sox
#'
#' @param filein character, path to audio file
#' @param pathtosox character, path to sox binary
#'
#' @return a list with features of audio file
#' @export
#'

# filein = "~/Downloads/wavfrommts.wav"
# pathtosox = getOption("avutils_sox")
# pathtosox = "~/Documents/utilities/sox"

audio_info <- function(filein, pathtosox = getOption("avutils_sox")) {
  if (!file.exists(pathtosox)) stop("sox binary not found", call. = FALSE)
  if (!file.exists(filein)) stop("file not found", call. = FALSE)

  cm <- paste(pathtosox, "--i", filein)
  res <- system(cm, intern = TRUE)
  res1 <- unlist(strsplit(x = res, split = ":", fixed = TRUE))
  res1 <- gsub(pattern = " ", replacement = "", x = res1)
  res2 <- unlist(strsplit(x = res, split = " ", fixed = TRUE))
  res3 <- unlist(strsplit(x = res, split = ":", fixed = TRUE))

  out <- list()
  out$filename <- res1[which(res1 == "InputFile") + 1]
  out$channels <- as.numeric(res1[which(res1 == "Channels") + 1])
  out$samplerate <- as.numeric(res1[which(res1 == "SampleRate") + 1])
  out$resolution <- as.numeric(unlist(strsplit(x = res1[which(res1 == "Precision") + 1], split = "-bit", fixed = TRUE)))
  out$samples <- as.numeric(res2[which(res2 == "samples") - 1])
  out$duration <- out$samples / out$samplerate
  out$format <- res3[which(res3 == "Sample Encoding") + 1]
  out$filesize <- file.size(filein)
  out$filesize_sox <- res1[which(res1 == "FileSize") + 1]
  out
}


