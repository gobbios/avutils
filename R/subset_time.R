#' select subset of elan or rttm annotations
#'
#' @param x a data.frame (result from \code{\link{read_elan}} or \code{\link{read_rttm}})
#' @param from numeric, include annotations from this point forward (by default \code{NULL}, i.e. take everything from the first line of the data)
#' @param to numeric, include annotations up to that point (by default \code{NULL}, i.e. take up to the last line)
#' @param include_incomplete logical, should annotations that cross \code{from = } and/or \code{to = } be kept (by default \code{TRUE})
#' @param shift_times logical, not implemented (yet)
#' @details If you keep annotations that cross the from and/or to boundaries, the durations of those annotations will be updated.
#' @return a data.frame
#' @export
#'
#' @examples
#' rttm <- system.file("yunitator_old_synthetic_speech_overlap.rttm", package = "avutils")
#' x <- read_rttm(rttm)
#' # leave unchanged
#' subset_time(x)
#' # up to 26
#' subset_time(x, to = 26)
#' # remove incomplete anno (25.6 - 26.3)
#' subset_time(x, to = 26, include_incomplete = FALSE)
#'
#' elan <- system.file("synthetic_speech_overlap.eaf", package = "avutils")
#' x <- read_elan(elan)
#' # leave unchanged
#' subset_time(x)
#' # up to 29
#' subset_time(x, to = 29)
#' # remove incomplete anno (28.3 - 30.5)
#' subset_time(x, to = 29, include_incomplete = FALSE)

subset_time <- function(x, from = NULL, to = NULL, include_incomplete = TRUE, shift_times = FALSE) {

  if (!is.null(from)) {
    x$start[x$start < from] <- from
  }

  if (!is.null(to)) {
    x$end[x$end > to] <- to
  }

  newdur <- x$end - x$start
  x <- x[newdur > 0, ]

  cut_segments <- which(round(x$end - x$start, 5) >= x$duration)

  if (!include_incomplete) {
    x <- x[cut_segments, ]
  }
  x$duration <- x$end - x$start
  x
}
