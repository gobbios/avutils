

#' create confusion matrix
#'
#' @param test numeric 0/1 vector for the test file, indicating whether there was speech or not at a given timepoint
#' @param gold numeric 0/1 vector for the gold standard file, indicating whether there was speech or not at a given timepoint
#'
#' @return a 2x2 matrix, where row labels refer to the gold standard and column lables to the tested/evaluated file
#' @export
#'
#' @examples
#' x1 <- system.file("noisemesSad_synthetic_speech.rttm", package = "avutils")
#' x2 <- system.file("synthetic_speech.eaf", package = "avutils")
#' edata <- evaluate_sad(test = x1, reference = x2, overlap_res = 100,
#'                       raw_results = TRUE, doplot = FALSE)
#' confusion_matrix(test = edata[, "test_speech"], gold = edata[, "ref_speech"])

confusion_matrix <- function(test = NULL, gold = NULL) {
  # test <- edata[, "x1_speech"]
  # gold <- edata[, "x2_speech"]
  # test <- c(0, 0)
  # gold <- c(1, 1)

  # add values (to be subtracted later again), so that matrix dimensions will work
  test <- c(test, 0, 1)
  gold <- c(gold, 1, 0)
  res <- matrix(table(gold, test), nrow = 2)
  res[1, 2] <- res[1, 2] - 1
  res[2, 1] <- res[2, 1] - 1
  res <- res[2:1, 2:1]
  colnames(res) <- c("speech", "no_speech")
  rownames(res) <- c("speech", "no_speech")
  res
}
