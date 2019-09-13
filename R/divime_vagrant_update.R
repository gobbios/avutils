#' update/re-install DiViMe VM
#'
#' @param divime_loc character, path to DiViMe directory
#' @param memoryoverride numeric value for the memory allocated to the VM (by default 2048, i.e. the default setting from DiViMi), see details
#' @details this function destroys (deletes) the virtual machine, pulls the git repository again and installs the virtual machine
#'
#' with the default value allocated to the VM's memory, you might run into trouble analyzing larger files. Apparently a value of 4096 should work (although I personally use 6144 to be safe). Note that you probably shouldn't use more than about half of the memory your actual machine has...
#'
#' Also note that this process can take a long time to execute (expect at least 20 min up to one hour depending on your internet connection and computer power)
#' @export
#' @return creates a directory, clones the DiViMe repository into it and starts the virtual machine

divime_vagrant_update <- function(divime_loc, memoryoverride = 2048) {
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # memoryoverride = 2048
  divime_loc <- normalizePath(divime_loc, winslash = "/", mustWork = FALSE)

  res <- suppressWarnings(divime_vagrant_state(divime_loc = divime_loc, what = "status", silent = TRUE))
  if (length(res) == 0) {
    stop("no DiViMe machine found in ", divime_loc, call. = FALSE)
  }

  if (interactive()) {
    cont <- askYesNo(paste0("Are you sure? This will delete all data files in the DiViMe folder (", divime_loc, "). No guarantees..."))
  }

  if (cont) {
    WD <- getwd()
    setwd(divime_loc)
    xres <- system2(command = Sys.which("vagrant"),
                    args = "destroy -f",
                    stdout = TRUE,
                    stderr = TRUE)
    setwd("..")
    system2(command = "rm", args = "-R DiViMe")
    divime_vagrant_setup(divime_loc = getwd(), memoryoverride = memoryoverride)

    setwd(WD)
  }

}
