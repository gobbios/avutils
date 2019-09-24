#' split annotations into time-based intervals (e.g. per minute)
#'
#' @param xdata data.frame, the data object with annotations (e.g. import from ELAN or .rttm file)
#' @param splitduration numeric, the time interval in milliseconds (default is 60000, i.e. one minute)
#' @param timecols character, the column names for start and end of the annotations
#' @param end_is_dur logical, is the column with the end time actually a duration (by default \code{FALSE}, i.e. end column is treated as a time stamp)
#' @details note that annotations that cross over a given cut point will be split into two
#'
#' if \code{end_is_dur = TRUE} a new column ('\code{xend}') is added that adds the second time column (which in that case should be a duration) to the start time
#' @return a data.frame with a new column that indicates which time interval a given annotation belongs to
#' @export
#' @examples
#' annos <- LETTERS[1:5]
#' start <- c(14, 17, 45, 65, 70)
#' end <- c(25, 22, 63, 80, 73)
#'
#' xdata <- data.frame(start, end, annos)
#'
#' xdata
#' split_duration(xdata, splitduration = 30, timecols = c("start", "end"))
#'
#' # from an ELAN file:
#' eaf <- system.file("synthetic_speech.eaf", package = "avutils")
#' elanfile <- read_elan(eaf)
#' split_duration(elanfile, splitduration = 5000)

split_duration <- function(xdata, splitduration = 60000, timecols = c("start", "end"), end_is_dur = FALSE) {
  # handle end time
  if (end_is_dur) {
    xdata$xend <- xdata[, timecols[1]] + xdata[, timecols[2]]
    timecols[2] <- "xend"
  }

  # matrix of time cols
  xd <- as.matrix(xdata[, timecols])
  timeintervals <- 1:(max(xd[, 2]) %/% splitduration + 1)
  cutpoints <- timeintervals * splitduration
  xcases <- sapply(cutpoints, function(X)which(xd[, 1] < X & xd[, 2] > X),
                   simplify = FALSE)

  for (i in 1:length(xcases)) {
    x <- xcases[[i]]
    if (length(x) > 0) {
      for (k in x) {
        temp <- xdata[k, ]
        temp[1, timecols[1]] <- cutpoints[i]
        xdata[k, timecols[2]] <- cutpoints[i]
        xdata <- rbind(xdata, temp)
      }
    }
  }

  xdata <- xdata[order(xdata[, timecols[1]]), ]
  rownames(xdata) <- NULL

  # add time cats
  xdata$cat <- NA
  for (i in length(cutpoints):1) {
    xlines <- which(xdata[, timecols[2]] <= cutpoints[i])
    xdata$cat[xlines] <- i
  }

  xdata
}
