#' test DiViMe VM
#'
#' @param divime_loc path where DiViMe installed
#'
#' @return textual output
#' @export

test_divime <- function(divime_loc) {
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"

  if (file.exists(paste0(divime_loc, "/launcher/test.sh"))) {
    WD <- getwd()
    setwd(divime_loc)
    system2(command = Sys.which("vagrant"), args = "ssh -c 'launcher/test.sh'")
    setwd(WD)
  } else {
    message("DiViMe directory not found")
  }

}
