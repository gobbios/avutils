#' evaluation of speech detection tools
#'
#' @param test character, path to file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param reference character, path to reference file with annotations in either ELAN (.eaf) or DiViMe (.rttm) format
#' @param nsegments numeric, the number of segments in which the file is to be split (represented as different lines in output graphic). Default is \code{1}, i.e. the entire file is considered as one segment.
#' @param duration numeric, optional info about the duration of the audio. At its default \code{NULL}, the end of the last annotation is taken as the duration
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
#' test <- system.file("noisemesSad_synthetic_speech.rttm", package = "avutils")
#' reference <- system.file("synthetic_speech.eaf", package = "avutils")
#' # no overlap calculation
#' evaluate_sad(test = test, reference = reference)
#' # fairly crude time resolution for overlap calculation
#' evaluate_sad(test = test, reference = reference, overlap_res = 10)
#' # slightly higher time resolution
#' evaluate_sad(test = test, reference = reference, overlap_res = 100)
#' # even more fine-grained
#' evaluate_sad(test = test, reference = reference, overlap_res = 1000)
#' # even more fine-grained and file split into multiple segments
#' evaluate_sad(test = test, reference = reference, nsegments = 10, overlap_res = 100)

evaluate_sad <- function(test, reference, nsegments = 1, duration = NULL,
                         from = NULL, to = NULL, overlap_res = NULL,
                         doplot = TRUE, raw_results = FALSE, ...) {
  # nsegments = 1; duration = 100; from = NULL; to = NULL; overlap_res = 20; doplot = TRUE; raw_results = FALSE

  # set 'to' to duration if not supplied but duration is
  if (is.null(to)) {
    if (!is.null(duration)) {
      to <- duration
    }
  }

  # read and process input files
  for (i in 1:2) {
    temp <- ifelse (i == 1, test, reference)
    if (grepl(pattern = ".eaf", temp, fixed = TRUE)) {
      temp <- read_elan(temp)
      temp <- collapse_tiers(temp, ...)
    } else {
      temp <- read_rttm(temp)
      temp <- data.frame(start = temp$start, end = temp$end)
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
  colnames(evaldata) <- c("segment", "timestamp", "test_speech", "ref_speech", "both_same")
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
    mtext(text = basename(test), side = 3, line = 1, at = par("usr")[1], cex = 0.5, adj = 0)
    mtext(text = basename(reference), side = 3, line = 1, at = par("usr")[2], cex = 0.5, adj = 1, col = "gold")
    box()
  }

  # summarize evaluation data
  # false alarm (FA): proportion of frames labeled as speech that were non-speech in the gold annotation
  # missed speech rate (proportion of frames labeled as non-speech that were speech in the gold annotation
  # file 2 is the reference/gold standard
  edata <- data.frame(seg = as.factor(evaldata[, "segment"]),
                      test_speech = evaldata[, "test_speech"],
                      ref_speech = evaldata[, "ref_speech"])
  # edata <- edata[1:22, ]
  if (nrow(edata) > 0) {
    eres <- data.frame(seg = levels(edata$seg), FA = NA, MSR = NA, total = NA)
    for (i in 1:nrow(eres)) {
      temp1 <- edata[edata$seg == eres$seg[i] & edata$ref_speech == 0, ]
      if (nrow(temp1) > 0) {
        eres$FA[i] <- sum(temp1$test_speech) / nrow(temp1)
      }

      temp2 <- edata[edata$seg == eres$seg[i] & edata$ref_speech == 1, ]
      if (nrow(temp2) > 0) {
        eres$MSR[i] <- sum(temp2$test_speech == 0) / nrow(temp2)
      }

      temp3 <- edata[edata$seg == eres$seg[i], ]
      if (nrow(temp3) > 0) {
        eres$total[i] <- sum(temp3$test_speech == temp3$ref_speech) / nrow(temp3)
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
        if (sum(temp3$test_speech == 1) == 0) {
          eres$FA[i] <- 0
          eres$MSR[i] <- 0
        }
        # some speech in test
        if (sum(temp3$test_speech == 1) > 0) {
          eres$FA[i] <- 1
          eres$MSR[i] <- 0
        }
      }
      # some speech in standard
      if (nrow(temp2) > 0) {
        # no speech in test
        if (sum(temp3$test_speech == 1) == 0) {
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
