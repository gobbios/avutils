#' test DiViMe VM
#'
#' @param divime_loc path where DiViMe is installed
#'
#' @return textual output
#' @export

divime_test <- function(divime_loc) {
  if (file.exists(paste0(divime_loc, "/launcher/test.sh"))) {
    WD <- getwd()
    setwd(divime_loc)
    system2(command = Sys.which("vagrant"), args = "ssh -c 'launcher/test.sh'")
    setwd(WD)
  } else {
    message("DiViMe directory not found")
  }
}
