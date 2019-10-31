#' evaluation of speech detection tools
#'
#' @param x1 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param x2 character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param nsegments numeric, the number of segments in which the file is to be split (represented as different lines in output graphic). Default is \code{1}, i.e. the entire file is considered as one segment.
#' @param from numeric, starting point for selecting a subset of time
#' @param to numeric, starting point for selecting a subset of time
#' @param overlap_res numeric, the time resolution for calculating overlap, by default \code{NULL}, i.e. no overlap is calculated, see \code{\link{annotation_overlap}}
#' @param doplot logical, should the plot actually be produced (default is \code{TRUE})
#' @param raw_results logical, should the raw results be returnd, i.e. the values per 'sample' (by default \code{FALSE})
#' @param ... additional parameters for elan files (which tiers/annotations to ignore), see \code{\link{collapse_tiers}}
#' @importFrom graphics axis box plot segments points
#' @importFrom grDevices adjustcolor
#' @return a data.frame with proportions of agreement between the two SADs
#' @importFrom graphics mtext par
#' @export
#' @examples
#' x1 <- system.file("noisemesSad_synthetic_speech.rttm", package = "avutils")
#' x2 <- system.file("synthetic_speech.eaf", package = "avutils")
#' # no overlap calculation
#' evaluate_sad(x1 = x1, x2 = x2)
#' # fairly crude time resolution for overlap calculation
#' evaluate_sad(x1 = x1, x2 = x2, overlap_res = 10)
#' # slightly higher time resolution
#' evaluate_sad(x1 = x1, x2 = x2, overlap_res = 100)
#' # even more fine-grained
#' evaluate_sad(x1 = x1, x2 = x2, overlap_res = 1000)
#' # even more fine-grained and file split into multiple segments
#' evaluate_sad(x1 = x1, x2 = x2, nsegments = 10, overlap_res = 100)

evaluate_sad <- function(x1, x2, nsegments = 1, from = NULL, to = NULL, overlap_res = NULL, doplot = TRUE, raw_results = FALSE, ...) {
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
      temp <- temp[temp[, 2] >= from, , drop = FALSE]
      if (nrow(temp) > 0) {
        if (temp[1, 1] < from) temp[1, 1] <- from
      }
    } else {
      from <- 0
    }
    if (!is.null(to)) {
      temp <- temp[temp[, 1] <= to, , drop = FALSE]
      if (nrow(temp) > 0) {
        if (temp[nrow(temp), 2] > to) temp[nrow(temp), 2] <- to
      }
    }
    # shift time stamps to begin from 0
    temp[, 1] <- temp[, 1] - from
    temp[, 2] <- temp[, 2] - from

    if (i == 1) p1 <- temp
    if (i == 2) p2 <- temp
  }

  # get range of times
  if (!is.null(to)) {
    trange <- (to - from) / nsegments
  } else {
    trange <- (max(unlist(c(p1, p2))) - from) / nsegments
  }

  # assign time segments
  p1 <- segment_annotations(xdata = p1, segment_dur = trange)
  p1$cat <- as.numeric(as.character(p1$cat))
  p2 <- segment_annotations(xdata = p2, segment_dur = trange)
  p2$cat <- as.numeric(as.character(p2$cat))

  # create empty objects to store overlap/evaluation data and plotting coordinates
  evaldata <- matrix(ncol = 5, nrow = 0)
  colnames(evaldata) <- c("segment", "timestamp", "x1_speech", "x2_speech", "both_same")
  pdata <- matrix(ncol = 4, nrow = 0)
  colnames(pdata) <- c("file", "start", "end", "y")
  ylabs <- c()

  # evaluate and create plotting data
  for (i in 1:nsegments) {
    pd1 <- as.matrix(p1[p1$cat == i, , drop = FALSE])
    pd2 <- as.matrix(p2[p2$cat == i, , drop = FALSE])
    pd1[, 1:2] <- pd1[, 1:2] - ((i - 1) * trange)
    pd2[, 1:2] <- pd2[, 1:2] - ((i - 1) * trange)

    if (nrow(pd1) > 0) {
      pdata <- rbind(pdata,
                     cbind(1, pd1[, c("start", "end"), drop = FALSE], i - 0.1))
    }
    if (nrow(pd2) > 0) {
      pdata <- rbind(pdata,
                     cbind(2, pd2[, c("start", "end"), drop = FALSE], i + 0.1))
    }

    ylabs <- c(ylabs, paste(round((i - 1) * trange + from, 1),
                            round(i * trange + from, 1),
                            sep = " - "))

    # evaluation/overlap
    if (!is.null(overlap_res)) {
      ov <- annotation_overlap(x1 = pd1, x2 = pd2, return_raw = TRUE,
                               seg_length = trange, overlap_res = overlap_res)
      evaldata <- rbind(evaldata, cbind(i, ov))
    }
  }

  # generate plot if required
  if (doplot) {
    plot(0, 0, type = "n", axes = FALSE, xlab = "time within segment", ylab = "segment",
         xlim = c(0, trange),
         ylim = c(nsegments + 1, 0))
    if (!is.null(overlap_res)) {
      xvals <- seq(from = 0, to = trange, length.out = overlap_res + 1)
      xvals <- unique(evaldata[, "timestamp"])
      axis(3, at = xvals, tcl = -0.1, labels = NA, lwd = 0.5)
      mtext(text = "evaluation points", side = 3, line = 0.5, cex = 0.8)
    }


    segments(x0 = pdata[, "start"], y0 = pdata[, "y"], x1 = pdata[, "end"], y1 = pdata[, "y"],
             col = c("black", "gold")[pdata[, "file"]], lwd = 2)
    mismatches <- evaldata[evaldata[, "both_same"] == 0, , drop = FALSE]
    if (nrow(mismatches) > 0) {
      points(x = mismatches[, "timestamp"], y = mismatches[, "segment"],
             pch = 16, col = adjustcolor("red", 0.2), cex = 0.5)
    }
    axis(2, at = 1:nsegments, tick = FALSE, labels = ylabs, las = 1, cex.axis = 0.5)
    # axis(4, at = 1:nsegments, tick = FALSE, labels = round(ovvals, 2), las = 1, cex.axis = 0.5)
    axis(1)
    mtext(text = basename(x1), side = 3, line = 1, at = par("usr")[1], cex = 0.5, adj = 0)
    mtext(text = basename(x2), side = 3, line = 1, at = par("usr")[2], cex = 0.5, adj = 1, col = "gold")
    box()
  }

  # summarize evaluation data
  # false alarm (FA): proportion of frames labeled as speech that were non-speech in the gold annotation
  # missed speech rate (proportion of frames labeled as non-speech that were speech in the gold annotation
  # file 2 is the reference/gold standard
  edata <- data.frame(seg = as.factor(evaldata[, "segment"]),
                      x1speech = evaldata[, "x1_speech"],
                      x2speech = evaldata[, "x2_speech"])
  # edata <- edata[1:22, ]
  if (nrow(edata) > 0) {
    eres <- data.frame(seg = levels(edata$seg), FA = NA, MSR = NA, total = NA)
    for (i in 1:nrow(eres)) {
      temp1 <- edata[edata$seg == eres$seg[i] & edata$x2speech == 0, ]
      if (nrow(temp1) > 0) {
        eres$FA[i] <- sum(temp1$x1speech) / nrow(temp1)
      }

      temp2 <- edata[edata$seg == eres$seg[i] & edata$x2speech == 1, ]
      if (nrow(temp2) > 0) {
        eres$MSR[i] <- sum(temp2$x1speech == 0) / nrow(temp2)
      }

      temp3 <- edata[edata$seg == eres$seg[i], ]
      if (nrow(temp3) > 0) {
        eres$total[i] <- sum(temp3$x1speech == temp3$x2speech) / nrow(temp3)
      }

      # special cases
      # If the gold annotation was empty, and the SAD system returned no speech
      # labels, then the FA = 0 and M = 0; but if the SAD system returned some
      # speech labels, then FA = 100 and M = 0. Also, if the gold annotation was
      # not empty and the system did not find any speech, then this was treated
      # as FA = 0 and M=100.

      # no speech in standard
      if (nrow(temp2) == 0) {
        # no speech in test
        if (sum(temp3$x1speech == 1) == 0) {
          eres$FA[i] <- 0
          eres$MSR[i] <- 0
        }
        # some speech in test
        if (sum(temp3$x1speech == 1) > 0) {
          eres$FA[i] <- 1
          eres$MSR[i] <- 0
        }
      }
      # some speech in standard
      if (nrow(temp2) > 0) {
        # no speech in test
        if (sum(temp3$x1speech == 1) == 0) {
          eres$FA[i] <- 0
          eres$MSR[i] <- 1
        }
      }
    }
  } else {
    eres <- NULL
  }


  if (raw_results) {
    return(evaldata)
  } else {
    return(eres)
  }

}

