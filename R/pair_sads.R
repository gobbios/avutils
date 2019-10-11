#' visual comparison of speech analysers and overlap calculations
#'
#' @param x1 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param x2 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param nlines numeric, the number of lines in output graphic
#' @param from numeric, starting point for selecting a subset of time
#' @param to numeric, starting point for selecting a subset of time
#' @param overlap_res numeric, the time resolution for calculating overlap, by default \code{NULL}, i.e. no overlap is calculated, see \code{\link{annotation_overlap}}
#' @param doplot logical, should the plot actually be produced
#' @param ... additional parameters for elan files (which tiers/annotations to ignore), see \code{\link{collapse_tiers}}
#' @importFrom graphics axis box plot segments points
#' @importFrom grDevices adjustcolor
#' @return a numeric vector with proportions of agreement between the two SADs
#' @export
#' @examples
#' x1 <- system.file("synthetic_speech.eaf", package = "avutils")
#' x2 <- system.file("noisemesSad_synthetic_speech.rttm", package = "avutils")
#' # no overlap calculation
#' pair_sads(x1 = x1, x2 = x2, nlines = 2)
#' # fairly crude time resolution for overlap calculation
#' pair_sads(x1 = x1, x2 = x2, nlines = 2, overlap_res = 10)
#' # slightly higher time resolution
#' pair_sads(x1 = x1, x2 = x2, nlines = 2, overlap_res = 100)
#' # even more fine-grained
#' pair_sads(x1 = x1, x2 = x2, nlines = 2, overlap_res = 1000)

pair_sads <- function(x1, x2, nlines = 5, from = NULL, to = NULL, overlap_res = NULL, doplot = TRUE, ...) {
  # read and process input files
  for (i in 1:2) {
    temp <- ifelse (i == 1, x1, x2)
    if (grepl(pattern = ".eaf", temp, fixed = TRUE)) {
      temp <- read_elan(temp)
      temp$start <- temp$start/1000
      temp$end <- temp$end/1000
      temp <- collapse_tiers(temp, ...)
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
  if (doplot) {
    plot(0, 0, type = "n", axes = FALSE, xlab = "time within segment", ylab = "segment",
         xlim = c(0, trange),
         ylim = c(nlines + 1, 0))
    ylabs <- character()
    ovvals <- numeric()
    if (!is.null(overlap_res)) {
      xvals <- seq(from = 0, to = trange, length.out = overlap_res + 1)
      axis(3, at = xvals, tcl = -0.1, labels = NA)
    }

    i=2
    for (i in 1:nlines) {
      pd1 <- p1[p1$cat == i, ]
      pd2 <- p2[p2$cat == i, ]
      pd1[, 1:2] <- pd1[, 1:2] - ((i - 1) * trange)
      pd2[, 1:2] <- pd2[, 1:2] - ((i - 1) * trange)
      pd1$y <- i - 0.1
      pd2$y <- i + 0.1

      l1 <- round((i - 1) * trange + from, 1)
      l2 <- round(i * trange + from, 1)
      ylabs <- c(ylabs, paste(l1, l2, sep = " - "))

      segments(x0 = pd1$start, y0 = pd1$y, x1 = pd1$end, y1 = pd1$y)
      segments(x0 = pd2$start, y0 = pd2$y, x1 = pd2$end, y1 = pd2$y)
      # calculate overlap
      ov <- NA
      if (!is.null(overlap_res)) {
        ov <- annotation_overlap(x1 = pd1, x2 = pd2, return_raw = TRUE,
                                 seg_length = trange, overlap_res = overlap_res)
        px <- xvals
        px <- px[(ov * i) == 0]
        py <- rep(i, length(px))
        points(x = px, y = py, pch = 16, col = adjustcolor("red", 0.2), cex = 0.5)
        ov <- mean(ov)
      }
      ovvals <- c(ovvals, ov)
    }

    axis(2, at = 1:nlines, tick = FALSE, labels = ylabs, las = 1, cex.axis = 0.5)
    axis(4, at = 1:nlines, tick = FALSE, labels = round(ovvals, 2), las = 1, cex.axis = 0.5)
    axis(1)
    box()
  }

  if (!doplot) {
    if (!is.null(overlap_res)) {
      xvals <- seq(from = 0, to = trange, length.out = overlap_res + 1)
    }
    ovvals <- numeric()

    for (i in 1:nlines) {
      pd1 <- p1[p1$cat == i, ]
      pd2 <- p2[p2$cat == i, ]
      pd1[, 1:2] <- pd1[, 1:2] - ((i - 1) * trange)
      pd2[, 1:2] <- pd2[, 1:2] - ((i - 1) * trange)

      # calculate overlap
      ov <- NA
      if (!is.null(overlap_res)) {
        ov <- annotation_overlap(x1 = pd1, x2 = pd2, return_raw = TRUE,
                                 seg_length = trange, overlap_res = overlap_res)
        px <- xvals
        px <- px[(ov * i) == 0]
        py <- rep(i, length(px))
        ov <- mean(ov)
      }
      ovvals <- c(ovvals, ov)
    }

  }

  # reset overlap to NULL if it hasn't been calculated (for cleaner results output)
  if (length(ovvals) == 0) {
    res <- NULL
  } else {
    res <- as.numeric(ovvals)
  }

  res
}
