#' collapse overlapping annotations
#'
#' from overlapping tiers
#'
#' @param xdata data.frame, the data object with annotations (e.g. import from ELAN or .rttm file)
#' @param timecols character, the column names for start and end of the annotations (default is \code{c("start", "end")}). If there is no 'end' time stamp but only a duration for the annotation, use this column.
#' @param end_is_dur logical, is the column with the end time actually a duration (by default \code{FALSE}, i.e. end column is treated as a time stamp)
#' @param ignore_tiers character, at least of length 2, where the first item is the column name for the tiers, and subsequently lists all tier names that should be ignored
#' @param ignore_annos character, at least of length 2, where the first item is the column name for the annotations, and subsequently lists all annotation values that should be ignored
#'
#' @details this is a fairly inefficient function (=slow)
#'
#' Also, in the process annotations are re-ordered such that start time increases.
#' @return a matrix with two columns for the start and end times of (collapsed) annotations
#' @export
#'
#' @examples
#'
#' annos <- LETTERS[1:5]
#' start <- c(14, 17, 45, 65, 70)
#' end <- c(25, 23, 60, 80, 82)
#' dur <- end - start
#' xdata <- data.frame(start, end, dur, annos)
#' # second anno is merged into first because it's completely comprised in the first
#' # last two annos are 'combined' into one
#' collapse_tiers(xdata = xdata, timecols = c("start", "end"), end_is_dur = FALSE)

collapse_tiers <- function(xdata, timecols = c("start", "end"), end_is_dur = FALSE, ignore_tiers = NULL, ignore_annos = NULL) {

  # handle end time
  if (end_is_dur) {
    xdata$xend <- xdata[, timecols[1]] + xdata[, timecols[2]]
    timecols[2] <- "xend"
  }

  if (!is.null(ignore_tiers)) {
    xdata <- xdata[!xdata[, ignore_tiers[1]] %in% ignore_tiers[2:length(ignore_tiers)], ]
  }
  if (!is.null(ignore_annos)) {
    xdata <- xdata[!xdata[, ignore_annos[1]] %in% ignore_annos[2:length(ignore_annos)], ]
  }

  # make matrix for faster computation
  xd <- as.matrix(xdata[, timecols])
  # and remove duplicated rows because they are irrelevant
  xd <- xd[!duplicated(xd), , drop = FALSE]
  # and reorder
  xd <- xd[order(xd[, 2]), , drop = FALSE]
  xd <- xd[order(xd[, 1]), , drop = FALSE]

  # result matrix
  res <- matrix(ncol = 2, nrow = 0)

  continue <- TRUE
  while (continue) {
    # take first row (reference) and check for any overlap with subsequent rows
    starts <- xd[2:nrow(xd), 1]
    # ends <- xd[2:nrow(xd), 2]
    # target starts after reference start but before reference's end
    check1 <- starts > xd[1, 1] & starts < xd[1, 2]
    if (TRUE %in% check1) {
      # select one of the overlapping annos
      x <- as.numeric(which(check1))
      if (length(x) == 1) {
        target <- x + 1
      } else {
        target <- sample(x, 1) + 1
      }
      # two possibilities:
      # 1) target ends before reference, hence nothing to be done (target completely inside reference)
      # except removing the target line
      if (xd[target, 2] <= xd[1, 2]) {
        xd <- xd[-target, , drop = FALSE]
      } else {
        # 2) target runs beyond reference, hence reference end time needs to be modified before target line is removed
        xd[1, 2] <- xd[target, 2]
        xd <- xd[-target, , drop = FALSE]
      }

    } else {
      # there is no overlap with any subsequent row, hence reference line can be extracted/removed and added to final result
      res <- rbind(res, xd[1, ])
      xd <- xd[-1, , drop = FALSE]
    }

    if (nrow(xd) == 1) {
      res <- rbind(res, xd[1, ])
      continue <- FALSE
    }
  }

  as.data.frame(res)
}

