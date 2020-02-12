#' read rttm files
#'
#' read and prettify rttm files
#'
#' @param x character, path to rttm file
#' @param from,to numeric, specify subset along the time axis (by default both
#' are \code{NULL}, i.e. the entire file is considered)
#' @details The function adds an end time column and assigns meaningful column
#' names for the most relevant columns. The actual content of the eighth column
#' (\code{tier}) depends on the rttm file. For example, 'yunitator' rttm files
#' have levels \code{CHI}, \code{FEM} and \code{MAL}, while SAD outputs contain
#' \code{speech} (or in the case of the full noisemes module other labels like
#' \code{background}, \code{noise} etc).
#'
#' If the rttm file is empty (i.e. contains no annotations), the result is a
#' data frame with no rows.
#'
#' If either \code{from} or \code{to} are specified, segments that overlap are
#' cut accordingly. For example, if \code{from = 8} and there is a segment that
#' starts at 7.5 and ends 8.7, this segment will be included as starting at 8
#' (i.e. the \code{from} value) and ending at 8.7
#'
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
#' # time subsetting
#' read_rttm(rttm, from = 8, to = 11)
#' # empty result because no annotations before 5 seconds
#' read_rttm(rttm, to = 5)

read_rttm <- function(x,
                      from = NULL,
                      to = NULL) {
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

  if (!is.null(from)) {
    res <- res[res$end > from, ]
    if (nrow(res) > 0) {
      if (res$start[1] < from) {
        res$start[1] <- from
        res$duration[1] <- res$end[1] - res$start[1]
      }
    }
  }

  if (!is.null(to)) {
    res <- res[res$start < to, ]
    if (nrow(res) > 0) {
      if (res$end[nrow(res)] > to) {
        res$end[nrow(res)] <- to
        res$duration[nrow(res)] <- res$end[nrow(res)] - res$start[nrow(res)]
      }
    }
  }


  attributes(res)$filename <- basename(x)
  rownames(res) <- NULL
  res
}
