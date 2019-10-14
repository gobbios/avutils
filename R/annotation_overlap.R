#' calculate overlap between two binary annotation sets
#'
#' @param x1 data.frame or matrix with annotations, needs columns named \code{"start"} and \code{"end"}
#' @param x2 data.frame or matrix with annotations, needs columns named \code{"start"} and \code{"end"}
#' @param overlap_res numeric, resolution for overlap (default is \code{100}), see details
#' @param seg_length numeric, total duration of the data (default is \code{NULL}, i.e. it's guessed from the data), see details
#' @param return_raw logical, by default \code{FALSE}, i.e. return the proportion. Otherwise return a 0/1 vector where 1 indicates a congruent comparison.
#' @details The function works like this. The total duration is segmented into \code{overlap_res} segments. The midpoints of these segments serve as test values. For example, if the resolution is 100 and the duration is 60 seconds, we will obtain 100 segments of 0.6 seconds each. The test values lay in the center of each segment, i.e. at 0.3, 0.9, 1.2 etc. Now, for each of these test values, check in \code{x1} and \code{x2} whether an annotation exists.
#'
#' Also, this function is more of a 'helper' function, that primarily is used inside the \code{\link{evaluate_sad}} function.
#'
#' @return a numeric value of overlap proportion, or a 0/1 vector.
#' @export
#'
#' @examples
#' x1 <- data.frame(start = c(1, 4, 7), end = c(2.3, 4.2, 8))
#' x2 <- data.frame(start = c(0, 5), end = c(4, 6))
#' annotation_overlap(x1, x2)
#' annotation_overlap(x1, x2, seg_length = 60)
#' # one annotation set is empty
#' x3 <- x2[0, ]
#' annotation_overlap(x1, x3)
#' # with 'real' data
#' x1 <- read.table(system.file("noisemesSAD_synthetic_speech.rttm", package = "avutils"))
#' colnames(x1)[c(4, 5)] <- c("start", "end")
#' x1$end <- x1$start + x1$end
#' x2 <- read_elan(system.file("synthetic_speech.eaf", package = "avutils"))
#' x2$start <- x2$start/1000
#' x2$end <- x2$end/1000
#' annotation_overlap(x1, x2, overlap_res = 10)
#' annotation_overlap(x1, x2, overlap_res = 10, return_raw = TRUE)

annotation_overlap <- function(x1, x2, seg_length = NULL, overlap_res = 100, return_raw = FALSE) {
  x1 <- as.matrix(x1[, c("start", "end"), drop = FALSE])
  x2 <- as.matrix(x2[, c("start", "end"), drop = FALSE])

  if (is.null(seg_length)) {
    # take the max of the two end columns
    seg_length <- max(c(x1, x2))
  }

  res <- numeric(length = overlap_res)
  # res <- matrix(nrow = overlap_res + 1, ncol = 3)
  xvals <- matrix(seq(from = 0, to = seg_length, length.out = overlap_res + 1),
                  ncol = 1)
  # transform into midpoints of segments
  xvals[, 1] <- xvals[, 1] + (xvals[2, 1] - xvals[1, 1])/2
  xvals <- xvals[-nrow(xvals), ]
  xvals <- cbind(xvals, NA, NA)

  for (i in 1:length(res)) {
    xvals[i, 2] <- sum(xvals[i, 1] >= x1[, 1] & xvals[i, 1] <= x1[, 2])
    xvals[i, 3] <- sum(xvals[i, 1] >= x2[, 1] & xvals[i, 1] <= x2[, 2])
  }
  xvals[, 2][xvals[, 2] == 2] <- 1
  xvals[, 3][xvals[, 3] == 2] <- 1
  res <- as.numeric(xvals[, 2] == xvals[, 3])
  out <- sum(res) / length(res)
  xvals <- cbind(xvals, res)
  colnames(xvals) <- c("timestamp", "x1_speech", "x2_speech", "both_same")

  if (return_raw) {
    return(xvals)
  } else {
    return(out)
  }

}
