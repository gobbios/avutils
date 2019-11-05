#' summarize Elan .eaf file
#'
#' @param x either character string, providing the path to the elan file(s)
#'
#' @return a list with two elements. The first is a data.frame with summaries and the second is a character vector with annotations ordered by their number of occurences (first is the most frequent)
#' @export
#'

summary_elan <- function(x) {
  res <- matrix(ncol = 6, nrow = 0)
  annos <- c()

  for (i in 1:length(x)) {
    X <- read_elan(x[i])

    # file name
    tempres <- data.frame(filename = rep(basename(x[i]), nlevels(X$tier)),
                          stringsAsFactors = FALSE)
    # tier names
    tempres$tier <- levels(X$tier)

    # number of annotations
    tempres$n_anno <- tapply(X$content, X$tier, length)

    # mean duration of annotations
    dur <- X$end - X$start
    tempres$mean_dur <- round(tapply(dur, X$tier, mean), 3)

    # check for empty/NA annotation values
    foo <- function(xdata) {
      length(which(xdata == "" | xdata == " " | is.na(xdata)))
    }
    tempres$empty_anno <- tapply(X$content, X$tier, foo)

    # most frequent anno
    foo <- function(xdata) {
      names(sort(table(as.character(xdata)), decreasing = TRUE))[1]
    }
    tempres$most_freq_anno <- tapply(X$content, X$tier, foo)

    annos <- c(annos, X$content)

    res <- rbind(res, tempres)
  }

  # count annos
  annos <- names(sort(table(annos), decreasing = TRUE))

  list(summary = res, annos = annos)
}
