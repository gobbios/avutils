#' merge rttm files
#'
#' @param rttm_files character, paths to rttm files to merge
#' @param split_dur numeric, the duration of the underlying split audio
#' @param basename character, the name of the file (unmerged)
#'
#' @return a data.frame that can be written as rttm file
#' @export
#'

combine_rttm <- function(rttm_files, split_dur, basename = NULL) {
  res <- matrix(ncol = 10, nrow = 0)
  for (i in 1:length(rttm_files)) {
    if (length(readLines(rttm_files[i])) > 0) {
      temp <- read.table(rttm_files[i],
                         sep = " ",
                         header = FALSE,
                         stringsAsFactors = FALSE)
      temp$V4 <- temp$V4 + split_dur * (i - 1)
      res <- rbind(res, temp)
      rm(temp)
    }
  }

  if (!is.null(basename)) res$V2 <- basename
  res
}
