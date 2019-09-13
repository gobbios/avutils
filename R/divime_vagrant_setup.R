#' setup/install DiViMe
#'
#' @param divime_loc path where the virtual machine should be installed, directory will be created if it doesn't exist
#' @param memoryoverride numeric value for the memory allocated to the VM (by default 2048, i.e. the default setting from DiViMi), see details
#' @details with the default value allocated to the VM's memory, you might run into trouble analyzing larger files. Apparently a value of 4096 should work (although I personally use 6144 to be safe). Note that you probably shouldn't use more than about half of the memory your actual machine has...
#'
#' Also note that this process can take a long time to execute (expect at least 20 min up to one hour depending on your internet connection and computer power)
#' @return creates a directory, clones the DiViMe repository into it and starts the virtual machine
#' @export
#'
divime_vagrant_setup <- function(divime_loc, memoryoverride = 2048) {
  # memoryoverride = 6144
  # divime_loc = "/Volumes/Data/VMX2"

  divime_loc <- normalizePath(divime_loc, winslash = "/", mustWork = FALSE)

  if (dir.exists(divime_loc)) {
    if (length(list.files(divime_loc)) > 0) {
      message("abort because directory exists and appears to be *not empty*")
      return(invisible(NULL))
    }
  } else {
    dir.create(divime_loc, recursive = TRUE)
  }

  WD <- getwd()
  setwd(divime_loc)
  # clone git repository
  system2(command = Sys.which("git"),
          args = "clone https://github.com/srvk/DiViMe")
  setwd(paste0(divime_loc, "/DiViMe"))

  # allocate memory
  vf <- readLines("Vagrantfile")
  loc <- grep(pattern = "vbox.memory", x = vf)
  if (length(loc) == 1) {
    temp <- vf[loc]
    vf[loc] <- gsub(pattern = "[[:digit:]]{4,4}",
                    x = temp,
                    replacement = memoryoverride)
    writeLines(vf, "Vagrantfile")
  }

  # reset CPU value if necessary
  if (parallel::detectCores() == 1) {
    vf <- readLines("Vagrantfile")
    loc <- grep(pattern = "vbox.cpus", x = vf)
    if (length(loc) == 1) {
      temp <- vf[loc]
      vf[loc] <- gsub(pattern = "[[:digit:]]{1,1}",
                      x = temp,
                      replacement = 1)
      writeLines(vf, "Vagrantfile")
    }
  }

  # install plugins
  system2(command = Sys.which("vagrant"), args = "plugin install vagrant-vbguest")
  system2(command = Sys.which("vagrant"), args = "plugin install vagrant-sshfs")
  system2(command = Sys.which("vagrant"), args = "plugin install --plugin-version 1.0.1 fog-ovirt")
  system2(command = Sys.which("vagrant"), args = "plugin install vagrant-aws")

  # start to build the VM for the first time...
  system2(command = Sys.which("vagrant"), args = "up")
  setwd(WD)
}
