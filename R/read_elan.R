#' read annotations from Elan .eaf file
#'
#' @param x either character string, providing the path to the elan file, or an \code{xml_document} (read by \code{xml2::read_xml()})
#'
#' @importFrom xml2 xml_children xml_name xml_attr xml_text xml_child read_xml
#' @return a data frame
#' @export
#' @examples
#' elan <- system.file("synthetic_speech.eaf", package = "avutils")
#' read_elan(elan)

read_elan <- function(x) {
  fn <- x
  if (class(x)[1] == "character") x <- read_xml(x)
  raw <- xml_children(x = x)
  # handle time slots
  timeslots <- xml_children(raw[which(xml_name(raw) == "TIME_ORDER")])
  t1 <- unlist(lapply(timeslots, function(X)xml_attr(X, "TIME_SLOT_ID")))
  t2 <- unlist(lapply(timeslots, function(X)xml_attr(X, "TIME_VALUE")))
  timeslots <- matrix(t2, ncol = 1)
  rownames(timeslots) <- t1
  # output
  res <- matrix(ncol = 6, nrow = 0)
  colnames(res) <- c("tier", "label", "reflabel", "start", "end", "content")
  # read actual tiers with annotations
  tierlocs <- which(xml_name(raw) == "TIER")
  alltiers <- raw[tierlocs]
  for (i in 1:length(tierlocs)) {
    tierid <- xml_attr(alltiers[[i]], "TIER_ID")
    tier <- xml_children(alltiers[i])
    annolocs <- which(xml_name(tier) == "ANNOTATION")
    for (k in annolocs) {
      anno <- xml_children(tier[[k]])
      res <- rbind(res, NA)
      xline <- nrow(res)
      if (!is.na(xml_attr(anno, attr = "TIME_SLOT_REF1"))) {
        res[xline, "start"] <- timeslots[xml_attr(anno,
                                                  attr = "TIME_SLOT_REF1"), 1]
        res[xline, "end"] <- timeslots[xml_attr(anno,
                                                attr = "TIME_SLOT_REF2"), 1]
      }
      res[xline, "content"] <- xml_text(xml_child(anno, "ANNOTATION_VALUE"))
      res[xline, "label"] <- xml_attr(anno, attr = "ANNOTATION_ID")
      res[xline, "reflabel"] <- xml_attr(anno, attr = "ANNOTATION_REF")
      res[xline, "tier"] <- tierid
    }
  }


  # deal with dependent tiers
  # labels that have time
  timinglabels <- res[!is.na(res[, "start"]), "label"]
  # labels without time
  notiminglabels <- res[is.na(res[, "start"]), "label"]
  if (length(notiminglabels) > 0) {
    timingres <- matrix(ncol = 2, nrow = length(notiminglabels))
    timingres[, 1] <- notiminglabels
    for (i in 1:nrow(timingres)) {
      continueloop <- TRUE
      rewritetemp <- TRUE
      while (continueloop) {
        if (rewritetemp) temp <- res[res[, "label"] == timingres[i, 1], ,
                                     drop = FALSE]
        if (temp[1, "reflabel"] %in% timinglabels) {
          timingres[i, 2] <- temp[1, "reflabel"]
          continueloop <- FALSE
        } else {
          temp <- res[res[, "label"] == temp[1, "reflabel"], , drop = FALSE]
          rewritetemp <- FALSE
        }
      }
    }
    if (!all(timingres[, 2] %in% timinglabels)) stop("something went wrong...",
                                                     call. = FALSE)

    # now fill the timings
    for (i in 1:nrow(timingres)) {
      child <- timingres[i, 1]
      parent <- timingres[i, 2]
      times <- res[res[, "label"] == parent, c("start", "end"), drop = FALSE]
      res[which(res[, "label"] == child), c("start", "end")] <- times
    }

  }

  # create clean output
  xres <- data.frame(tier = factor(res[, "tier"]),
                     start = as.numeric(res[, "start"]),
                     end = as.numeric(res[, "end"]),
                     content = res[, "content"], stringsAsFactors = FALSE)
  xres <- xres[order(xres$start), ]
  rownames(xres) <- NULL

  # reformat times to milliseconds
  xres$start <- xres$start/1000
  xres$end <- xres$end/1000
  # add duration column
  xres$duration <- xres$end - xres$start
  attributes(xres)$filename <- basename(fn)

  xres
}
