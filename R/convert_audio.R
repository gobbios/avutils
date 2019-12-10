#' convert audio file format
#'
#' convert from mp3 and possibly other audio file formats to .wav
#'
#' @param filein character, paths to input audio files
#' @param outformat a list with specifications of the desired output format, see details
#' @param overwrite logical, should output files be overwritten if they already exist
#' @param pathtoffmpeg character, path to ffmpeg binary
#' @param pathout character, path were output is written, by default the same as the source
#' @details The \code{outformat=} argument currently provides three options, and by default a mono (\code{mono = TRUE}) .wav files is produced (\code{filetpye = "wav"}) with 44.1kHz sampling rate (\code{res = 441000}).
#'
#' Files will be potentially renamed, i.e. the file extension of the new output file will change to '.wav'.
#'
#' Note that if the input file has the same name and is in the same location as the output (e.g. if you convert x.wav with \code{pathout = NULL}), nothing will happen even if you select \code{overwrite = TRUE} because \code{ffmpeg} won't overwrite its own source.
#'
#' @return a \code{data.frame} with diagnostics
#' @importFrom tools file_path_sans_ext
#' @export
#'
convert_audio <- function(filein,
                          outformat = list(filetype = "wav",
                                           res = 44100,
                                           mono = TRUE),
                          overwrite = FALSE,
                          pathout = NULL,
                          pathtoffmpeg = getOption("avutils_ffmpeg")) {

  # some prelims
  xin <- normalizePath(filein, winslash = "/", mustWork = FALSE)
  check1 <- !vapply(xin, file.exists, FALSE)
  if (sum(check1) > 0) {
    mess <- paste(shQuote(names(check1)[check1]), collapse = "\n")
    warning("the following audio files were not found:\n", mess, call. = FALSE)
  }
  if (!file.exists(pathtoffmpeg)) {
    stop("ffmpeg binary not found", call. = FALSE)
  }
  if (outformat$filetype != "wav") {
    stop("only 'wav' is supported as output file type for now, not '", outformat$filetype, "'", call. = FALSE)
  }

  # get file names and folders
  infolders <- dirname(xin)
  basenames <- basename(xin)

  # outnames (replace file extension with 'wav')
  outnames <- paste0(file_path_sans_ext(basenames), ".wav")

  # and set outlocs
  if (is.null(pathout)) {
    outlocs <- file.path(infolders, outnames)
  } else {
    pathout <- normalizePath(pathout, winslash = "/")
    outlocs <- file.path(pathout, outnames)
  }

  outres <- data.frame(infile = basenames,
                       outfile = basename(outlocs),
                       duplicate = duplicated(outlocs),
                       overwritten = FALSE,
                       success = FALSE,
                       outloc = outlocs,
                       stringsAsFactors = FALSE)

  outres$duplicate[outres$outloc %in% outlocs[duplicated(outlocs)]] <- TRUE


  if (sum(outres$duplicate) > 0) {
    warning("You have files that have the same output name and location.\nThese were ignored.", call. = FALSE)
  }

  for (i in 1:nrow(outres)) {
    cm <- paste(ifelse(overwrite, "-y", "-n"),
                "-i", xin[i], "-vn",  # input file igoring video
                "-ar", outformat$res, # sampling resolution
                "-ac", ifelse(outformat$mono, 1, 2), # number of channels
                outlocs[i])
    if (!outres$dupli[i]) {
      # special case: output file is the same as input file
      # won't do anything because ffmpeg can't overwrite its own source
      if (xin[i] == outres$outloc[i]) {
        # do nothing
      } else {
        # check whether file already exists
        outalreadythere <- file.exists(outres$outloc[i])
        if (outalreadythere) {
          if (overwrite) {
            info1 <- file.info(outres$outloc[i])[1, "ctime"]
            res <- suppressWarnings(system2(command = pathtoffmpeg,
                                            args = cm,
                                            stdout = TRUE,
                                            stderr = TRUE))
            info2 <- file.info(outres$outloc[i])[1, "ctime"]
            dtime <- as.numeric(difftime(info2, info1, units = "min"))
            if (dtime > 0) {
              outres$success[i] <- TRUE
            }
            outres$overwritten[i] <- TRUE
          }

        } else {
          res <- suppressWarnings(system2(command = pathtoffmpeg,
                                          args = cm,
                                          stdout = TRUE,
                                          stderr = TRUE))
          if (file.exists(outres$outloc[i])) {
            outres$success[i] <- TRUE
          }
        }
      }
    }
  }

  outres
}
