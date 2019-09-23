#' split annotations into time-based intervals (e.g. per minute)
#'
#' @param xdata data.frame, the data object with annotations (e.g. import from ELAN or .rttm file)
#' @param splitduration numeric, the time interval in milliseconds (default is 60000, i.e. one minute)
#' @param timecols character, the column names for start and end of the annotations
#' @details note that annotations that cross over a given cut point will be split into two
#' @return a data.frame with a new column that indicates which time interval a given annotation belongs to
#' @export

split_duration <- function(xdata, splitduration = 60000, timecols = c("start", "end")) {
  # matrix of time cols
  xd <- as.matrix(xdata[, timecols])
  timeintervals <- 1:(max(xd[, 2]) %/% splitduration + 1)
  cutpoints <- timeintervals * splitduration
  xcases <- sapply(cutpoints, function(X)which(xd[, 1] < X & xd[, 2] > X),
                   simplify = FALSE)
  # xdata$XFILTER <- TRUE
  # xdata$XFILTER[unlist(xcases)] <- FALSE

  for (i in 1:length(xcases)) {
    x <- xcases[[i]]
    if (length(x) > 0) {
      for (k in x) {
        temp <- xdata[k, ]
        temp[1, timecols[1]] <- cutpoints[i] + 1
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
