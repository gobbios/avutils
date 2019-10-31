#' evaluate speaker roles
#'
#' @param xdata a data.frame, (result from \code{\link{read_elan}} or \code{\link{read_rttm}})
#' @param resolution numeric, the time increment in seconds, by default 1
#' @param duration numeric, optional info about the duration of the audio. If not supplied, the end of the last annotation is taken as the duration
#' @param tiers character of variable length, the tiers that should be considered
#'
#' @return a matrix
#' @export
#'
#' @examples
#' elan <- system.file("synthetic_speech_overlap.eaf", package = "avutils")
#' xdata <- read_elan(elan)
#' evaluate_roles(xdata)

evaluate_roles <- function(xdata, resolution = 1, duration = NULL, tiers = NULL) {
  # tiers=c("english_1", "english_2"); duration=50000; resolution=1000

  # filter requested tiers
  if (is.null(tiers)) {
    tiers <- unique(as.character(xdata$tier))
  }
  xdata <- xdata[xdata$tier %in% tiers, ]
  # generate sampling points (depending on whether duration is supplied)
  if (is.null(duration)) {
    duration <- max(xdata$end)
  }

  samplepoints <- seq(from = 0, to = duration, by = resolution)
  # remove first point
  samplepoints <- samplepoints[-1]
  # and last if it coincides with duration
  if (samplepoints[length(samplepoints)] == duration) {
    samplepoints <- samplepoints[-length(samplepoints)]
  }

  # results object
  # matrix with one column for each tier (and one column for sample time point)
  res <- matrix(ncol = length(tiers), nrow = length(samplepoints))
  colnames(res) <- tiers
  i = 1
  for (i in 1:length(tiers)) {
    temp <- as.matrix(xdata[xdata$tier == tiers[i], c("start", "end"), drop = FALSE])
    res[, i] <- colSums(sapply(samplepoints, function(X) X >= temp[, 1] & X <= temp[, 2]))
  }

  cbind(frame = samplepoints, res)
}
