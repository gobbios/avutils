#' calculate overlap between two binary annotation sets
#'
#' @param x1 data.frame or matrix with annotations, needs columns named \code{"start"} and \code{"end"}
#' @param x2 data.frame or matrix with annotations, needs columns named \code{"start"} and \code{"end"}
#' @param overlap_res numeric, resolution for overlap (default is \code{100}), see details
#' @param seg_length numeric, total duration of the data (default is \code{NULL}, i.e. it's guessed from the data), see details
#' @param return_raw logical, by default \code{FALSE}, i.e. return the proportion. Otherwise return a 0/1 vector where 1 indicates a congruent comparison.
#' @details The function works like this. Given the total duration of an annotation file (e.g. 60 seconds), create a sequence of test values between 0 and the duration in steps determined by \code{overlap_res}. For example, if the resolution is 100 and the duration is 60 seconds, these values will be 0, 0.6, 1.2, 1.8 etc. (Technically, this corresponds to 100 + 1 values). Now, for each of these test values, check in \code{x1} and \code{x2} whether an annotation exists that comprises this value (or not). In our example this would lead to 100 \emph{pairs} of values (i.e. 100 comparisons). Each comparison can be either congruent (i.e. the test value is either covered in both \code{x1} and \code{x2}, or neither in \code{x1} and \code{x2}), or a comparison can be incongruent, i.e. it's covered in \code{x1} or \code{x2}. From this, we can calculate the proportion of comparisons that are congruent, which is the result of this function.
#'
#' For now, the function returns \code{NA} if one of the two annotation sets contains no data. If both annotation sets are empty, the function will return \code{1}.
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
annotation_overlap <- function(x1, x2, seg_length = NULL, overlap_res = 100, return_raw = FALSE) {
  x1 <- as.matrix(x1[, c("start", "end")])
  x2 <- as.matrix(x2[, c("start", "end")])

  if (is.null(seg_length)) {
    # take the max of the two end columns
    seg_length <- max(c(x1, x2))
  }

  res <- numeric(length = overlap_res + 1)
  xvals <- matrix(seq(from = 0, to = seg_length, length.out = overlap_res + 1), ncol = 1)
  xvals <- cbind(xvals, NA, NA)

  if (nrow(x1) == 0 & nrow(x2) == 0) {
    res[] <- 1
    out <- 1
  } else {
    if (nrow(x1) == 0) {
      x1 <- matrix(c(0, seg_length), nrow = 1,
                   dimnames = list(NULL, c("start", "end")))
      return(NA)
    }
    if (nrow(x2) == 0) {
      x2 <- matrix(c(0, seg_length), nrow = 1,
                   dimnames = list(NULL, c("start", "end")))
      return(NA)
    }

    for (i in 1:length(res)) {
      xvals[i, 2] <- sum(xvals[i, 1] >= x1[, 1] & xvals[i, 1] <= x1[, 2])
      xvals[i, 3] <- sum(xvals[i, 1] >= x2[, 1] & xvals[i, 1] <= x2[, 2])
    }
    xvals <- xvals
    res <- as.numeric(xvals[, 2] == xvals[, 3])
    out <- sum(res)/length(res)
  }

  if (return_raw) {
    output <- res
  } else {
    output <- out
  }

  output
}
