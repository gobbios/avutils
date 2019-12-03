#' convert ELAN .eaf to .rttm
#'
#' @param x character, path to elan (.eaf) file
#' @param outpath character, path to output directory (if \code{NULL} (the default), file is written in the same location as the input)
#' @param clean_at logical, remove tiers with @@ in their name (dependent tiers)
#' @param use_py logical, use DiViMe python script (per default \code{TRUE}), or R style
#' @details to install the required python package use \code{py_install("pympi-ling")}.
#'
#' Also note that the \code{output=} argument needs to be a folder location. The output file name is 'fixed' in the sense that it's the same as the input, except that file extension is changed to .rttm.
#'
#' If you use the python version of this function (\code{use_py = TRUE}), the ELAN files need to meet at least the following requirement: the tiers must not only have a 'tier name', but also must have a 'particpant' assigned to each tier. The underlying \code{pympi} python package will throw a message 'parsing unknown version of ELAN spec... This could result in errors ...', in case you use \emph{not} ELAN 2.6 or 2.7. Since current ELAN version are far above that, there is no way
#'
#' The python version also re-sorts the output, i.e. entries are sorted according to tier name whereas the R version sorts according to starting time.
#'
#' If a file exists with the same name as the target file, it will be overwritten.
#'
#' @return a file is written and the path to that file is returned as character
#' @importFrom reticulate source_python
#' @export
#' @examples
#' \dontrun{
#' tdir <- tempdir()
#' elanfile <- system.file("spanish.eaf", package = "avutils")
#'
#' # use R
#' dest <- elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
#' "spanish.rttm" %in% list.files(tdir)
#' temp <- read_rttm(paste0(tdir, "/spanish.rttm"))
#' temp <- temp[order(temp$start), ]
#' head(temp)
#' file.remove(paste0(tdir, "/spanish.rttm"))
#'
#' # use python internally
#' dest <- elan2rttm(x = elanfile, outpath = tdir, use_py = TRUE)
#' "spanish.rttm" %in% list.files(tdir)
#' temp <- read_rttm(paste0(tdir, "/spanish.rttm"))
#' temp <- temp[order(temp$start), ]
#' head(temp)
#' file.remove(paste0(tdir, "/spanish.rttm"))
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
    py$eaf2rttm(path_to_eaf = x, path_to_write_rttm = outpath)
  } else {
    # read file
    xdata <- read_elan(x)
    if (clean_at) {
      xdata <- xdata[!grepl(pattern = "@", x = xdata$tier), ]
    }
    # build output
    out <- data.frame(col1 = "SPEAKER",
                      col2 = gsub(".eaf", "", bn, fixed = TRUE),
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
    write.table(out, file = outloc, quote = FALSE, sep = " ",
                row.names = FALSE, col.names = FALSE)
  }

  if (file.exists(outloc)) {
    return(outloc)
  }

}
