#' convert audio file format
#'
#' @param filein character, path to input audio
#' @param outformat a list with specifications of the desired output format, see details
#' @param overwrite logical, should output files be overwritten if it already exists
#' @param pathtoffmpeg character, path to ffmpeg binary
#' @param pathout character, path were output is stored, by default the same as the source
#' @details the \code{outformat=} argument currently provides four options, and by default a mono (\code{mono = TRUE}) .wav files is produced (\code{filetpye = "wav"}) with 44.1kHz resolution (\code{res = 441000}).
#' @return a vector with the paths to the split files
#' @export
#'
convert_audio <- function(filein,
                          outformat = list(filetype = "wav",
                                           res = 44100,
                                           mono = TRUE),
                          overwrite = FALSE,
                          pathout = NULL,
                          pathtoffmpeg = getOption("avutils_ffmpeg")) {

  # filein = "~/Downloads/john_oliver_replies_to_jack_warner.mp3"
  # outformat = list(filetype = "wav", res = 44100, mono = TRUE); pathout = NULL
  # set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg"); pathtoffmpeg = getOption("avutils_ffmpeg")

  xin <- normalizePath(filein)
  if (!file.exists(xin)) stop("audio file not found", call. = FALSE)
  if (!file.exists(pathtoffmpeg)) stop("ffmpeg binary not found", call. = FALSE)

  # get file names and folders
  temp <- unlist(strsplit(xin, "/", fixed = TRUE))
  if ("" %in% temp) temp <- temp[temp != ""]
  infilename <- temp[length(temp)]
  outfilename <- unlist(strsplit(infilename, ".", fixed = TRUE))
  outfilename <- outfilename[-length(outfilename)]
  outfilename <- paste(outfilename, collapse = ".")
  outfilename <- paste0(outfilename, ".", outformat$filetype)

  outloc <- paste0("/", paste(temp[-length(temp)], collapse = "/"), "/", outfilename)
  if (!is.null(pathout)) {
    outloc <- paste0(normalizePath(pathout), "/", outfilename)
  }

  # create the command
  cm <- paste(pathtoffmpeg, ifelse(overwrite, "-y", "-n"), "-i", xin, "-vn", "-ar", outformat$res, "-ac", ifelse(outformat$mono, 1, 2), outloc)
  # run the command
  system(command = cm, intern = TRUE)
  #

  outloc
}
