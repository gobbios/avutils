#' visual comparison of speech analysers
#'
#' @param x1 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param x2 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param nlines numeric, the number of lines in output graphic
#' @param from numeric, starting point for selecting a subset of time
#' @param to numeric, starting point for selecting a subset of time
#' @param overlap numeric, the time increment for calculating overlap, by default \code{NULL}, i.e. no overlap is calculated
#' @param ... additional parameters for elan files (which tiers/annotations to ignore), see \code{\link{collapse_tiers}}
#' @importFrom graphics axis box plot segments
#' @return a numeric vector with proportions of agreement between the two SADs
#' @export

pair_sads <- function(x1, x2, nlines = 5, from = NULL, to = NULL, overlap = NULL, ...) {
  # read and process files
  for (i in 1:2) {
    temp <- ifelse (i == 1, x1, x2)
    if (grepl(pattern = ".eaf", temp, fixed = TRUE)) {
      temp <- read_elan(temp)
      temp$start <- temp$start/1000
      temp$end <- temp$end/1000
      temp <- collapse_tiers(temp)
    } else {
      temp <- read.table(temp, header = FALSE)
      temp <- data.frame(start = temp$V4, end = temp$V4 + temp$V5)
    }
    # subset time range (if desired)
    if (!is.null(from)) {
      temp <- temp[temp[, 2] >= from, ]
      temp[1, 1] <- from
    } else {
      from <- 0
    }
    if (!is.null(to)) {
      temp <- temp[temp[, 1] <= to, ]
      temp[nrow(temp), 2] <- to
    }
    # shift time stamps to begin from 0
    temp[, 1] <- temp[, 1] - from
    temp[, 2] <- temp[, 2] - from

    if (i == 1) p1 <- temp
    if (i == 2) p2 <- temp
  }

  # get range of times
  trange <- diff(range(c(p1, p2)))/(nlines)
  # assign time segments
  p1 <- segment_annotations(p1, segment_dur = trange)
  p1$cat <- as.numeric(as.character(p1$cat))
  p2 <- segment_annotations(p2, segment_dur = trange)
  p2$cat <- as.numeric(as.character(p2$cat))

  # set up plot
  plot(0, 0, type = "n", axes = FALSE, xlab = "time within segment", ylab = "segment",
       xlim = c(0, trange),
       ylim = c(nlines + 1, 0))
  ylabs <- character()
  ovvals <- character()
  i=1
  for (i in 1:nlines) {
    pd1 <- p1[p1$cat == i, ]
    pd2 <- p2[p2$cat == i, ]
    pd1[, 1:2] <- pd1[, 1:2] - ((i - 1) * trange)
    pd2[, 1:2] <- pd2[, 1:2] - ((i - 1) * trange)
    pd1$y <- i + 0.1
    pd2$y <- i - 0.1

    l1 <- round((i - 1) * trange + from, 1)
    l2 <- round(i * trange + from, 1)
    ylabs <- c(ylabs, paste(l1, l2, sep = " - "))

    segments(x0 = pd1$start, y0 = pd1$y, x1 = pd1$end, y1 = pd1$y)
    segments(x0 = pd2$start, y0 = pd2$y, x1 = pd2$end, y1 = pd2$y)
    # calculate overlap
    ov <- ""
    if (!is.null(overlap)) {
      if (nrow(pd1) == 0 & nrow(pd2) == 0) {
        ov <- 1
      } else {
        xvals <- matrix(seq(0, trange, by = overlap), ncol = 1)
        xvals <- cbind(xvals, NA, NA)
        for (k in 1:(nrow(xvals) - 1)) {
          xvals[k, 2] <- sum(xvals[k, 1] >= pd1[, 1] & xvals[k + 1, 1] <= pd1[, 2])
          xvals[k, 3] <- sum(xvals[k, 1] >= pd2[, 1] & xvals[k + 1, 1] <= pd2[, 2])
        }
        xvals <- xvals[-nrow(xvals), ]
        ov <- round(sum(xvals[, 2] == xvals[, 3])/nrow(xvals), 2)
      }
    }
    ovvals <- c(ovvals, ov)
  }

  axis(2, at = 1:nlines, tick = FALSE, labels = ylabs, las = 1, cex.axis = 0.5)
  axis(4, at = 1:nlines, tick = FALSE, labels = ovvals, las = 1, cex.axis = 0.5)
  axis(1)
  box()

  # reset overlap to NULL if it hasn't been calculated (for cleaner results output)
  if (length(ovvals) == 0) {
    res <- NULL
  } else {
    res <- as.numeric(ovvals)
  }

  res
}
