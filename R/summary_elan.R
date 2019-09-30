#' summarize Elan .eaf file
#'
#' @param x either character string, providing the path to the elan file, or an \code{xml_document} (read by \code{xml2::read_xml()})
#'
#' @return a data frame
#' @export
#'

summary_elan <- function(x) {
  x <- read_elan(x)

  res <- data.frame(tier = levels(x$tier))

  # number of annotations
  res$n_anno <- tapply(x$content, x$tier, length)

  # mean duration of annotations
  dur <- x$end - x$start
  res$mean_dur <- round(tapply(dur, x$tier, mean)/1000, 2)

  # check for empty/NA annotation values
  foo <- function(X) length(which(X == "" | X == " " | is.na(X)))
  res$empty_anno <- tapply(x$content, x$tier, foo)

  # most frequent anno
  foo <- function(X) {
    X <- sort(table(as.character(X)), decreasing = TRUE)
    names(X)[1]
  }
  res$most_freq_anno <- tapply(x$content, x$tier, foo)

  res
}
