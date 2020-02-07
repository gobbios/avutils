#' read rttm files
#'
#' read and prettify rttm files
#'
#' @param x character, path to rttm file
#' @details The function adds an end time column and assigns meaningful column
#' names for the most relevant columns. The actual content of the eighth column
#' (\code{tier}) depends on the rttm file. For example, 'yunitator' rttm files
#' have levels \code{CHI}, \code{FEM} and \code{MAL}, while SAD outputs contain
#' \code{speech} (or in the case of the full noisemes module other labels like
#' \code{background}, \code{noise} etc).
#'
#' If the rttm file is empty (i.e. contains no annotations), the result is a
#' data frame with no rows.
#' @return a data.frame
#' @export
#'
#' @examples
#' rttm <- system.file("noisemesSad_synthetic_speech_overlap.rttm",
#'                     package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("noisemesFull_synthetic_speech_overlap.rttm",
#'                     package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("yunitator_english_synthetic_speech_overlap.rttm",
#'                     package = "avutils")
#' read_rttm(rttm)
#' rttm <- system.file("tocomboSad_synthetic_speech_overlap.rttm",
#'                      package = "avutils")
#' read_rttm(rttm)

read_rttm <- function(x) {
  if (length(readLines(x)) > 0) {
    res <- read.table(x, header = FALSE)
    colnames(res)[c(4, 5, 8)] <- c("start", "duration", "tier")
    res$end <- res$start + res$duration
  } else {
    res <- matrix(0, nrow = 0, ncol = 10)
    # res[1, ] <- NA
    colnames(res) <- paste0("V", 1:ncol(res))
    colnames(res)[c(4, 5, 8, 10)] <- c("start", "duration", "tier", "end")
    res <- data.frame(res)
  }

  attributes(res)$filename <- basename(x)
  res
}
