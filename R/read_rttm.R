#' read rttm files
#'
#' read and prettify rttm files
#'
#' @param x character, path to rttm file
#' @details The function adds an end time column and assigns meaningful column names for the most relevant columns. The actual content of the eighth column (\code{tier}) depends on the rttm file. For example, 'yunitator' rttm files have levels \code{CHI}, \code{FEM} and \code{MAL}, while SAD outputs contain \code{speech} (or in the case of the full noisemes module other labels like \code{background}, \code{noise} etc).
#' @return a data.frame
#' @export
#'
#' @examples
#' rttm <- system.file("noisemesSad_synthetic_speech_overlap.rttm", package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("noisemesFull_synthetic_speech_overlap.rttm", package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("yunitator_english_synthetic_speech_overlap.rttm", package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("tocomboSad_synthetic_speech_overlap.rttm", package = "avutils")
#' read_rttm(rttm)
read_rttm <- function(x) {
  res <- read.table(x, header = FALSE)
  # special handling for tocombo files?
  # if (grepl("tocomboSad_", basename(x)))
  # colnames(res)[c(7, 10, 14)] <- c("start", "duration", "tier")

  colnames(res)[c(4, 5, 8)] <- c("start", "duration", "tier")

  res$end <- res$start + res$duration
  res
}
