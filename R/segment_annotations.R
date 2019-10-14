#' segment annotations time-based intervals
#'
#' assign each annotation to regular (e.g. one minute) segments
#'
#' @param xdata data.frame, the data object with annotations (e.g. import from ELAN or .rttm file)
#' @param segment_dur numeric, the time interval in the relevant unit (no default, see details)
#' @param timecols character, the column names for start and end of the annotations (default is \code{c("start", "end")}). If there is no 'end' time stamp but only a duration for the annotation, use this column.
#' @param end_is_dur logical, is the column with the end time actually a duration (by default \code{FALSE}, i.e. end column is treated as a time stamp)
#' @details the unit of the \code{segment_dur=} argument depends on the unit of time in the source file. ELAN uses milliseconds, while the DiViMe tools typically use seconds.
#'
#' note that annotations that cross over a given cut point for a segment will be split into two (or more if an annotation is much longer than the segment duration)
#'
#' if \code{end_is_dur = TRUE} a new column ('\code{xend}') is added that adds the second time column (which in that case should be a duration) to the start time
#' @return a data.frame with a new column that indicates which time interval a given annotation belongs to
#' @export
#' @examples
#' annos <- LETTERS[1:5]
#' start <- c(14, 17, 45, 65, 70)
#' end <- c(25, 22, 60, 80, 73)
#' dur <- end - start
#' xdata <- data.frame(start, end, dur, annos)
#'
#' xdata
#' segment_annotations(xdata, segment_dur = 10, timecols = c("start", "end"))
#'
#' # from an ELAN file:
#' eaf <- system.file("synthetic_speech.eaf", package = "avutils")
#' elanfile <- read_elan(eaf)
#' segment_annotations(elanfile, segment_dur = 500)

segment_annotations <- function(xdata, segment_dur, timecols = c("start", "end"), end_is_dur = FALSE) {
  # special case when xdata is empty
  if (nrow(xdata) == 0) {
    res <- data.frame(start = 0, end = 0, cat = NA, xdur = 0)
    return(res[-1, ])
  }

  # calculate total sum of anno durations (for sanity check)
  if (end_is_dur) {
    totdur <- sum(xdata[, timecols[2]])
  } else {
    totdur <- sum(xdata[, timecols[2]] - xdata[, timecols[1]])
  }

  # handle end time
  if (end_is_dur) {
    xdata$xend <- xdata[, timecols[1]] + xdata[, timecols[2]]
    timecols[2] <- "xend"
  }

  # matrix of time cols
  xd <- as.matrix(xdata[, timecols])
  timeintervals <- 1:(max(xd[, 2]) %/% segment_dur + 1)
  cutpoints <- timeintervals * segment_dur

  # go through each annotation
  i = 1
  for (i in 1:nrow(xd)) {
    cp <- cutpoints[sapply(cutpoints, function(X)xd[i, 1] < X & xd[i, 2] > X)]
    if (length(cp) > 0) {
      temp <- xdata[i, ]
      xdata[i, timecols[2]] <- cp[1]
      k = 1
      for (k in 1:length(cp)) {
        newline <- temp
        newline[1, timecols[1]] <- cp[k]
        if (k != length(cp)) newline[1, timecols[2]] <- cp[k + 1]
        # append to xdata
        xdata <- rbind(xdata, newline)
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
  xdata$cat <- factor(xdata$cat, levels = 1:length(cutpoints))

  # and add new durations (per segment)
  xdata$xdur <- xdata[, timecols[2]] - xdata[, timecols[1]]

  # run sanity checks
  if (round(sum(xdata$xdur), 4) != round(totdur, 4)) {
    warning("something went wrong: the durations don't add up", call. = FALSE)
  }
  if (max(xdata$xdur) > segment_dur) {
    warning("something went wrong: longer segment-wise durations than allowed",
            call. = FALSE)
  }

  xdata
}
