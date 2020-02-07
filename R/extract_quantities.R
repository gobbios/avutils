#' extract child speech counts, duration and conversational turns
#'
#' @param path character, path to file with results from yunitator or Elan
#' @param turntakingthresh numeric, the threshold for the time between onsets of
#' two subsequent segments to be considered a turn. Default is \code{1} second.
#' @param tiernames a list with two elements. By default set to \code{NULL},
#' where tier names are guessed from the data type. If supplied, element 1
#' (\code{targetchild}) should be a character of length one, and the second
#' element (\code{adults}) a character vector of any length. See details.
#'
#' @details if the supplied file is an .rttm file, the following tier names are
#' assumed: \code{list(targetchild = "CHI", adults = c("FEM", "MAL"))}. If
#' supplying an .eaf file, tier names are assumed as follows:
#' \code{list(targetchild = "CHI", adults = c("FA1", "MA1"))}.
#'
#' @return a list with three elements.
#' \itemize{
#' \item \code{cum_dur} the cumulative duration for each speaker
#' \item \code{voc_count} the number of speech segments for each speaker
#' \item \code{turns} the number of turns
#' }
#' @export

extract_quantities <- function(path,
                               turntakingthresh = 1,
                               tiernames = NULL) {
  # determine data type
  type <- rev(unlist(strsplit(basename(path), ".", fixed = TRUE)))[1]

  # set tier names depending on file type
  if (is.null(tiernames)) {
    if (type == "rttm") {
      adults <- c("FEM", "MAL")
    }
    if (type == "eaf") {
      adults <- c("FA1", "MA1")
    }
    targetchild = "CHI"
  } else {
    targetchild <- tiernames$targetchild
    adults <- tiernames$adults
  }

  if (type == "rttm") {
    xdata <- read_rttm(x = path)
  }

  if (type == "eaf") {
    adults <- c("FA1", "MA1")
    xdata <- read_elan(x = path)
    xdata <- xdata[xdata$tier %in% c(targetchild, adults), ]
    xdata <- droplevels(xdata[xdata$content %in% c("s", "s?"), ])
  }

  # handle empty rttm files correctly
  if (nrow(xdata) > 0) {
    # count vocalizations and total duration of vocalizations
    res1 <- tapply(X = xdata$duration, INDEX = xdata$tier, sum)
    res2 <- tapply(X = xdata$duration, INDEX = xdata$tier, length)
    # turn taking
    xdata$tier <- as.character(xdata$tier)
    # calc inter turn interval
    xdata$iti <- xdata[, "start"] - c(NA, xdata[-nrow(xdata), "end"])
    # get previous seg type
    xdata$prev_label <- c(NA, xdata[-nrow(xdata),"tier"])
    # mark turns
    xdata$turn <- ifelse((xdata$tier %in% targetchild &
                            xdata$prev_label %in% adults &
                            xdata$iti < turntakingthresh) |
                           (xdata$tier %in% adults &
                              xdata$prev_label %in% targetchild &
                              xdata$iti < turntakingthresh), 1, 0)
    res3 <- sum(xdata$turn)

    return(list(cum_dur = res1,
                voc_count = res2,
                turns = res3))
  } else {
    return(list(cum_dur = NA,
                voc_count = NA,
                turns = NA))
  }

}
