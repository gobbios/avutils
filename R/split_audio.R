#' split audio file into chunks of regular or irregular length
#'
#' @param filein character, path to input audio
#' @param split numeric or a data.frame, see details
#' @param pathtosox character, path to sox binary
#' @param ndigits numeric, the number of characters a time step should comprise (see details and \code{\link{leadingzeros}})
#' @param pathout character, path were output is stored, by default the same as the source
#' @details The \code{split=} argument can be supplied in two ways. If you supply a single number the input file will be split into file of equal lengths (determined by \code{split=} in seconds). If you supply a data.frame, it is assumed this is a .rttm file, where the fourth column reflects the starting point and the fifth column determines the length of the segment.
#'
#' Output files have the time stamps appended in the file name (unit of milliseconds) in the form of filename_fromtime_totime.wav. The \code{ndigits=} argument determines the number of characters the two time chunks have.
#' @return a vector with the paths to the split files
#' @export
#' @examples
#' \dontrun{
#' set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg",
#'              pathtosox = "~/Documents/utilities/sox")
#' tdir <- tempdir()
#' w1 <- sine(freq = 440, duration = 1.3, bit = 32, stereo = TRUE, xunit = "time")
#' writeWave(w1, filename = file.path(tdir, "file1.wav"))
#' tdir
#' x <- list.files(tdir, pattern = "\\.wav$", full.names = TRUE)
#' audio_info(filein = x)
#' y <- split_audio(filein = x, split = 0.4)
#' audio_info(filein = y)
#'
#' x <- system.file("synthetic_speech.wav", package = "avutils")
#' res <- split_audio(filein = x, split = 10, pathout = tdir)
#' audio_info(filein = res)
#' # remove files
#' file.remove(res)
#'
#' # using and .rttm file as input
#' split <- read.table(system.file("opensmileSad_synthetic_speech.rttm", package = "avutils"))
#' head(split)
#' res <- split_audio(filein = x, split = split, pathout = tdir)
#' audio_info(filein = res)
#' }


split_audio <- function(filein, split, pathout = NULL, ndigits = NULL, pathtosox = getOption("avutils_sox")) {
  xin <- normalizePath(filein, winslash = "/")
  if (!file.exists(xin)) stop("audio file not found", call. = FALSE)
  if (!file.exists(pathtosox)) stop("sox binary not found", call. = FALSE)

  # get length of audio file
  audiolength <- audio_info(filein = xin)$duration[1]

  # equal-length chunks
  if (length(split) == 1) {
    temp <- seq(0, audiolength, by = split)
    if (temp[length(temp)] != audiolength) temp <- c(temp, audiolength)
    segs <- matrix(floor(temp[1:(length(temp) - 1)] * 1000), ncol = 1)
    segs <- cbind(segs, floor(temp[2 : length(temp)] * 1000))
  }
  # assuming rttm input
  if (length(split) > 1) {
    segs <- cbind(floor(split[, 4] * 1000),
                  floor((split[, 4] + split[, 5]) * 1000))
  }

  if (is.null(ndigits)) ndigits <- max(nchar(segs))

  f1 <- apply(segs, 1, function(X) leadingzeros(X[1], ndigits = ndigits))
  f2 <- apply(segs, 1, function(X) leadingzeros(X[2], ndigits = ndigits, ".wav"))
  fns <- paste(f1, f2, sep = "_")

  bnm <- unlist(strsplit(basename(xin), split = ".", fixed = TRUE))
  bnm <- bnm[-length(bnm)]
  bnm <- paste(bnm, collapse = ".")

  if (is.null(pathout)) {
    outlocs <- paste(dirname(xin), paste(bnm, fns, sep = "_"), sep = "/")
    pathout <- dirname(xin)
  } else {
    outlocs <- normalizePath(paste(pathout,
                                   paste(bnm, fns, sep = "_"), sep = "/"),
                             winslash = "/", mustWork = FALSE)
  }

  for (i in 1:nrow(segs)) {
    cm <- paste(shQuote(xin),
                shQuote(outlocs[i]),
                "trim",
                segs[i, 1]/1000,
                paste0("=", segs[i, 2]/1000))
    system2(command = pathtosox, args = cm, stdout = TRUE, stderr = TRUE)
  }

  list.files(pathout, pattern = bnm, full.names = TRUE)
}
