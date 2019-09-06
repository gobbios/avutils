#' split audio file into chunks of defined length
#'
#' @param filein character, path to input audio
#' @param split numeric, duration of split in seconds
#' @param pathtosox character, path to sox binary
#' @param pathout character, path were output is stored, by default the same as the source
#'
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
#' audio_info(filein = y)}


split_audio <- function(filein, split, pathout = NULL, pathtosox = getOption("avutils_sox")) {
  xin <- normalizePath(filein, winslash = "/")
  if (!file.exists(xin)) stop("audio file not found", call. = FALSE)
  if (!file.exists(pathtosox)) stop("sox binary not found", call. = FALSE)

  temp <- unlist(strsplit(x = xin, split = "/", fixed = TRUE))
  newfilename <- unlist(strsplit(x = temp[length(temp)], split = ".", fixed = TRUE))
  newfilename[length(newfilename) - 1] <- paste0(newfilename[length(newfilename) - 1], "_split")
  newfilename <- paste(newfilename, collapse = ".")
  if (is.null(pathout)) {
    temp[length(temp)] <- newfilename
    targetloc <- paste(temp, collapse = "/")
  } else {
    targetloc <- paste(pathout, newfilename, sep = "/")
  }

  cm <- paste(shQuote(xin), shQuote(targetloc), "trim 0", split, ": newfile : restart" )
  # system(command = cm, intern = TRUE)
  system2(command = pathtosox, args = cm, stdout = TRUE, stderr = TRUE)
  outpath <- unlist(strsplit(x = targetloc, split = "/", fixed = TRUE))
  outpath <- outpath[-length(outpath)]
  outpath <- paste(outpath, collapse = "/")
  nameroot <-  unlist(strsplit(x = targetloc, split = "/", fixed = TRUE))
  nameroot <- unlist(strsplit(nameroot[length(nameroot)], split = ".", fixed = TRUE))
  nameroot <- paste(nameroot[-length(nameroot)], collapse = ".")

  list.files(outpath, pattern = nameroot, full.names = TRUE)

}
