#' status and handling of VM
#'
#' @param divime_loc character, path to DiViMe directory
#' @param what character, what to do, \code{"status"} (default), \code{"start"}, \code{"suspend"}, or \code{"halt"}, see details
#' @param silent logical, should a message be printed
#' @details \code{what=} can be one of the following:
#' \itemize{
#'  \item \code{"status"}: default, what is the state of the VM
#'
#'  \item \code{"start"}: start the VM
#'
#'  \item \code{"suspend"}: suspend the VM
#'
#'  \item \code{"halt"}: shut down the VM
#' }
#'
#' in case of \code{what = "status"}, there is also an invisible result that indicates whether the VM is running
#'
#' @return a message (or modifies the state of the VM)
#' @export
#'

divime_vagrant_state <- function(divime_loc, what = "status", silent = FALSE) {
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"

  WD <- getwd()
  setwd(divime_loc)
  if (what == "status") {
    res <- system2(command = Sys.which("vagrant"),
                   args = "status",
                   stdout = TRUE)
    res <- res[grep("default   ", x = res)]
    res <- trimws(unlist(strsplit(res, "default")))
    res <- res[res != ""]
  }

  if (what == "suspend") {
    res <- system2(command = Sys.which("vagrant"),
                   args = "suspend",
                   stdout = TRUE)
  }

  if (what == "halt") {
    res <- system2(command = Sys.which("vagrant"),
                   args = "halt",
                   stdout = TRUE)
  }

  if (what == "start") {
    res <- system2(command = Sys.which("vagrant"),
                   args = "up",
                   stdout = TRUE)
    res <- res[grep("default   ", x = res)]
    res <- trimws(unlist(strsplit(res, "default")))
    res <- res[res != ""]
  }
  setwd(WD)

  if (!silent) message(res)
  if (what == "status") return(invisible(res))
}
