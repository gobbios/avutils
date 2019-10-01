#' summarize Elan .eaf file
#'
#' @param x either character string, providing the path to the elan file, or an \code{xml_document} (read by \code{xml2::read_xml()})
#'
#' @return a data frame
#' @export
#'

summary_elan <- function(x) {
  X <- read_elan(x)

  # file name
  res <- data.frame(filename = rep(basename(x), nlevels(X$tier)))

  # tier names
  res$tier <- levels(X$tier)

  # number of annotations
  res$n_anno <- tapply(X$content, X$tier, length)

  # mean duration of annotations
  dur <- X$end - X$start
  res$mean_dur <- round(tapply(dur, X$tier, mean)/1000, 2)

  # check for empty/NA annotation values
  foo <- function(xdata) {
    length(which(xdata == "" | xdata == " " | is.na(xdata)))
  }
  res$empty_anno <- tapply(X$content, X$tier, foo)

  # count annos
  annos <- names(sort(table(as.character(X$content)), decreasing = TRUE))

  # most frequent anno
  res$most_freq_anno <- annos[1]

  list(summary = res, annos = annos)
}
