#' responsiveness
#'
#' proportion of utterances by focus/target role that are responded to by
#' responder within some threshold time
#'
#' @param xfile character, file path to rttm or eaf file
#' @param responder character, either \code{"child"}, \code{"female"},
#' \code{"male"} or \code{"adult"}, whose responses are considered
#' @param focus character, either \code{"child"}, \code{"female"},
#' \code{"male"} or \code{"adult"}, whose utterances are checked whether they
#' are responded to
#' @param threshold numeric, the threshold to be considered (default is
#' \code{2}), i.e. utterance of \code{"responder"} must have started no later
#' than this number from the end of an utterance of \code{"focus"}
#'
#' @return a list with three items (\code{utterances}: the number of utterances
#' by focus/target and responder, \code{responses}: the number of responses by
#' \code{responder} and \code{threshold}: the threshold value used)
#' @export
#'
#' @examples
#' xfile <- system.file("yunitator_english_spanish.rttm", package = "avutils")
#' responsiveness(xfile, focus = "child", responder = "female")
#' responsiveness(xfile, focus = "child", responder = "male")
#' responsiveness(xfile, focus = "child", responder = "adult")

responsiveness <- function(xfile,
                           focus = c("child", "female", "male", "adult"),
                           responder = c("child", "female", "male", "adult"),
                           threshold = 2) {
  # read rttm
  if (grepl("rttm$", xfile)) {
    xd <- read_rttm(xfile)
    xd$tier <- as.character(xd$tier)
    if (responder == "child") responder <- "CHI"
    if (responder == "female") responder <- "FEM"
    if (responder == "male") responder <- "MAL"
    if (responder == "adult") responder <- c("FEM", "MAL")
    if (focus == "child") focus <- "CHI"
    if (focus == "female") focus <- "FEM"
    if (focus == "male") focus <- "MAL"
    if (focus == "adult") focus <- c("FEM", "MAL")
  }

  # read elan
  if (grepl("eaf$", xfile)) {
    xd <- read_elan(xfile)
    xd <- xd[xd$content %in% c("s", "s?"), ]
    xd$tier <- as.character(xd$tier)
    if (responder == "child") responder <- "CHI"
    if (responder == "female") responder <- c("FA1", "FA2", "FA3", "FA4")
    if (responder == "male") responder <- c("MA1", "MA2", "MA3", "MA4")
    if (responder == "adult") responder <- c("FA1", "FA2", "FA3", "FA4",
                                             "MA1", "MA2", "MA3", "MA4")
    if (focus == "child") focus <- "CHI"
    if (focus == "female") focus <- c("FA1", "FA2", "FA3", "FA4")
    if (focus == "male") focus <- c("MA1", "MA2", "MA3", "MA4")
    if (focus == "adult") focus <- c("FA1", "FA2", "FA3", "FA4",
                                     "MA1", "MA2", "MA3", "MA4")
  }

  xdata <- xd
  xdata$tier_ori <- xdata$tier
  xdata$tier[xdata$tier %in% responder] <- "resp"
  xdata$tier[xdata$tier %in% focus] <- "focus"
  xdata <- xdata[xdata$tier %in% c("resp", "focus"), ]
  xdata$received_response <- NA

  foc <- xdata[xdata$tier == "focus", ]
  # foc$end <- foc$end + threshold
  resp <- xdata[xdata$tier == "resp", ]
  tempres <- sapply(foc$end, function(x){
    temp <- resp$start - x
    length(temp[temp >= 0 & temp <= threshold]) > 0
  })
  xdata$received_response[xdata$tier == "focus"] <- tempres

  xdata$tier <- factor(xdata$tier, levels = c("focus", "resp"))
  utterances <- as.numeric(table(xdata$tier))
  names(utterances) <- c("target", "responder")

  list(utterances = utterances,
       responses = sum(xdata$received_response, na.rm = TRUE),
       threshold = threshold)
}
