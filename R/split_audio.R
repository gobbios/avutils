# pathtosox="~/Documents/utilities/sox"
# filein="~/Downloads/00007.copy.wav"
# split = 1.1
# pathout = NULL
#' split audio file into chunks of defined length
#'
#' @param filein character, path to input audio
#' @param split numeric, duration of split in seconds
#' @param pathtosox character, path to sox binary
#' @param pathout character, path were output is stored, by default the same as the source
#'
#' @return a vector with the paths to the split files
#' @export
#'
split_audio <- function(filein, split, pathtosox, pathout = NULL) {
  xin <- normalizePath(filein)
  if (!file.exists(xin)) stop("video file not found", call. = FALSE)
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



  cm <- paste(pathtosox, xin, targetloc, "trim 0", split, ": newfile : restart" )
  system(command = cm, intern = TRUE)
  outpath <- unlist(strsplit(x = targetloc, split = "/", fixed = TRUE))
  outpath <- outpath[-length(outpath)]
  outpath <- paste(outpath, collapse = "/")
  nameroot <-  unlist(strsplit(x = targetloc, split = "/", fixed = TRUE))
  nameroot <- unlist(strsplit(nameroot[length(nameroot)], split = ".", fixed = TRUE))
  nameroot <- paste(nameroot[-length(nameroot)], collapse = ".")

  list.files(outpath, pattern = nameroot, full.names = TRUE)

}
