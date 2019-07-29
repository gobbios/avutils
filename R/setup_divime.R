#' setup DiViMe
#'
#' @param divime_loc path where the virtual machine should be installed, directory will be created if it doesn't exist
#'
#' @return creates a directory, clones the DiViMe repository into it and starts the virtual machine
#' @export
#'
setup_divime <- function(divime_loc) {
  # divime_loc = "/Volumes/Data/VM2/ooo"

  if (dir.exists(divime_loc)) {
    if (length(list.files(divime_loc)) > 0) {
      message("abort because directory exists and appears to be *not empty*")
      return(invisible(NULL))
    }
  } else {
    dir.create(divime_loc, recursive = TRUE)
  }
  divime_loc <- normalizePath(divime_loc)

  WD <- getwd()
  setwd(divime_loc)
  system2(command = Sys.which("git"), args = "clone https://github.com/srvk/DiViMe")
  setwd(paste0(divime_loc, "/DiViMe"))
  system2(command = Sys.which("vagrant"), args = "up")
  setwd(WD)
}

