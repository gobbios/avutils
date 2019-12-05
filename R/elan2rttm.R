#' convert ELAN .eaf to .rttm
#'
#' @param x character, path to elan (.eaf) file(s)
#' @param outpath character, path to output directory (if \code{NULL} (the default), files are written in the same location as the respective input files)
#' @param clean_at logical, remove tiers with @@ in their name (dependent tiers)
#' @param use_py logical, use DiViMe python script (per default \code{TRUE}), or R style
#' @details to install the required python package use \code{py_install("pympi-ling")}.
#'
#' If you supply an output location, the \code{outpath=} argument needs to be a folder location (and not file names). The output file name is 'fixed' in the sense that it's the same as the input, except that file extension of the name is changed to .rttm.
#'
#' If you supply a \emph{single} output location, but input files are in different locations, all output files will be written to the same output folder. This might cause trouble if you have input files of the same name but in different subfolders. E.g. \code{elan2rttm(x = c("file1.eaf", "sub/file1.eaf"), outpath = "~/Desktop")} will create only one output file because the actual file name occurs more than once. Consider renaming input files, or doing the conversion 'piece-meal' i.e. by sub-folder.
#'
#' If you use the python version of this function (\code{use_py = TRUE}), the ELAN files need to meet at least the following requirement: the tiers must not only have a 'tier name', but also must have a 'particpant' assigned to each tier. The underlying \code{pympi} python package will throw a message 'parsing unknown version of ELAN spec... This could result in errors ...', in case you use \emph{not} ELAN 2.6 or 2.7. Since current ELAN version are far above that, there is no way
#'
#' The python version also re-sorts the output, i.e. entries are sorted according to tier name whereas the R version sorts according to starting time.
#'
#' If a file exists with the same name as the target file, it will be overwritten.
#'
#' @return .rttm file(s) written and data.frame with file name(s) and location(s) is returned
#' @importFrom reticulate source_python
#' @export
#' @examples
#' \dontrun{
#' tdir <- tempdir()
#' elanfile <- system.file("spanish.eaf", package = "avutils")
#'
#' # use R
#' elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
#' "spanish.rttm" %in% list.files(tdir)
#' temp <- read_rttm(paste0(tdir, "/spanish.rttm"))
#' temp <- temp[order(temp$start), ]
#' head(temp)
#' file.remove(paste0(tdir, "/spanish.rttm"))
#'
#' # use python internally
#' elan2rttm(x = elanfile, outpath = tdir, use_py = TRUE)
#' "spanish.rttm" %in% list.files(tdir)
#' temp <- read_rttm(paste0(tdir, "/spanish.rttm"))
#' temp <- temp[order(temp$start), ]
#' head(temp)
#' file.remove(paste0(tdir, "/spanish.rttm"))
#'
#' # multiple files in different locations
#' dir.create(file.path(tdir, "test"))
#' file.copy(system.file("spanish.eaf", package = "avutils"), file.path(tdir, "spanish.eaf"))
#' file.copy(system.file("synthetic_speech.eaf", package = "avutils"), file.path(tdir, "synthetic_speech.eaf"))
#' file.copy(system.file("synthetic_speech_overlap.eaf", package = "avutils"), file.path(tdir, "test/synthetic_speech_overlap.eaf"))
#' X <- list.files(tdir, pattern = ".eaf", full.names = T, recursive = TRUE)
#' X
#' elan2rttm(x = X, use_py = FALSE)
#' }

elan2rttm <- function(x, outpath = NULL, clean_at = TRUE, use_py = TRUE) {
  x <- normalizePath(x, winslash = "/", mustWork = FALSE)

  bn <- basename(x)
  outname <- basename(gsub(".eaf", ".rttm", x, fixed = TRUE))
  if (is.null(outpath)) {
    outpath <- normalizePath(dirname(x), winslash = "/", mustWork = FALSE)
  } else {
    outpath <- normalizePath(outpath, winslash = "/", mustWork = FALSE)
  }
  outloc <- file.path(outpath, outname)

  if (use_py) {
    pympi <- import("pympi")
    source_python(system.file("elan2rttm.py", package = "avutils"))
    for (i in 1:length(x)) {
      py$eaf2rttm(path_to_eaf = x[i], path_to_write_rttm = outpath[i])
    }

  } else {
    for (i in 1:length(x)) {
      # read file
      xdata <- read_elan(x[i])
      if (clean_at) {
        xdata <- xdata[!grepl(pattern = "@", x = xdata$tier), ]
      }
      # build output
      out <- data.frame(col1 = "SPEAKER",
                        col2 = gsub(".eaf", "", bn[i], fixed = TRUE),
                        col3 = 1,
                        start = xdata$start,
                        dur = round(xdata$duration, 2),
                        col6 = as.factor(NA),
                        col7 = as.factor(NA),
                        tier = xdata$tier,
                        col9 = 1
      )
      rownames(out) <- NULL
      # write output
      write.table(out, file = outloc[i], quote = FALSE, sep = " ",
                  row.names = FALSE, col.names = FALSE)
      rm(xdata, out)
    }

  }

  data.frame(file_in = bn, file_out = outname, out_location = outpath, success = file.exists(outloc))

}
