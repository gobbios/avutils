#' add DiViMe SAD annotations to ELAN file
#'
#' @param elanfile character, path to elan file
#' @param audiofile character, path to audio file
#' @param tocombo character, path to tocombo rttm (optional)
#' @param noisemes character, path to noisemes rttm (optional)
#' @param opensmile character, path to opensmile rttm (optional)
#' @param noisemesFull character, path to noisemesFull rttm (optional)
#'
#' @details This is bascially a wrapper for \code{\link{rttm2elan}}, which takes care of homogenizing the format of .rttm files when they come from different sources (ELAN and DiViMe tools).
#'
#' Note also that this function can take a long time to finish (due to the conversion from an intermediate .rttm file to ELAN .eaf).
#' @return a file is created in the same location as \code{audiofile} and the path to it is returned as character
#' @export
#'

add2elan <- function(elanfile, audiofile, tocombo = NULL, noisemes = NULL, opensmile = NULL, noisemesFull = NULL) {

  # read DiViMe and combine
  temp1 <- matrix(ncol = 9, nrow = 0)
  if (!is.null(tocombo)) {
    toc <- read.table(tocombo, header = FALSE)
    toc[, 1] <- "tocombo"
    temp1 <- rbind(temp1, toc)
  }
  if (!is.null(noisemes)) {
    noi <- read.table(noisemes, header = FALSE)
    noi[, 1] <- "noisemes"
    temp1 <- rbind(temp1, noi)
  }
  if (!is.null(opensmile)) {
    ope <- read.table(opensmile, header = FALSE)
    ope[, 1] <- "opensmile"
    temp1 <- rbind(temp1, ope)
  }
  if (!is.null(noisemesFull)) {
    noif <- read.table(noisemesFull, header = FALSE)
    noif[, 1] <- "noisemesFull"
    temp1 <- rbind(temp1, noif)
  }

  # read elan input file
  temp2 <- read_elan(elanfile)
  temp2 <- data.frame(as.character(temp2$tier), NA, NA, temp2$start,
                      temp2$end - temp2$start, NA, NA, temp2$content, NA)
  temp2[, 4] <- temp2[, 4]/1000
  temp2[, 5] <- temp2[, 5]/1000
  temp2[, 8] <- gsub(pattern = " ", replacement = "_", x = temp2[, 8])
  temp2[, 1] <- gsub(pattern = " ", replacement = "_", x = temp2[, 1])
  colnames(temp2) <- paste0("V", 1:9)

  # combine
  temp <- rbind(temp2, temp1)
  temp[, c(6, 7, 9)] <- NA

  # create/find temp folder
  tdir <- tempdir()

  # write intermediate rttm file
  outname <- paste0("out_", basename(audiofile))
  outname1 <- gsub(pattern = ".wav", replacement = ".rttm", x = outname, fixed = TRUE)
  write.table(x = temp, file = paste0(tdir, "/", outname1),
              col.names = FALSE, quote = FALSE, row.names = FALSE)

  # convert to ELAN
  res <- rttm2elan(rttmfile = paste0(tdir, "/", outname1), audiofile = audiofile)
  if (file.exists(res)) {
    return(res)
  } else {
    return(NULL)
  }
}
