#' run a DiViMe SAD module
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param module character, which module to execute (default is \code{"noisemes"}), see details
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @details \code{module=} sets the SAD module to be used: can be either \code{"noisemes"}, \code{"opensmile"} or \code{"tocombo"}
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export
#'

divime_sad <- function(audio_loc,
                       divime_loc,
                       module = "noisemes",
                       vmshutdown = TRUE,
                       messages = TRUE,
                       overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # vmshutdown = F; messages = TRUE; overwrite = FALSE
  # module = "opensmile"

  audio_loc <- normalizePath(audio_loc)
  divime_loc <- normalizePath(divime_loc)

  vagrant <- Sys.which("vagrant")

  # check VM state and start if necessary
  vm_running <- divime_vagrant_state(divime_loc = divime_loc,
                                     what = "status",
                                     silent = TRUE)
  if(!vm_running) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "start",
                         silent = TRUE)
  }

  paths <- avutils:::handle_filenames(audio_loc = audio_loc,
                                      divime_loc = divime_loc)

  logres <- data.frame(audio = paths$filestoprocess,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       yuniproblem = NA)

  # create command depending on the desired module
  if (module == "noisemes") cm <- paste0("ssh -c 'noisemesSad.sh data/'")
  if (module == "opensmile") cm <- paste0("ssh -c 'opensmileSad.sh data/'")
  if (module == "tocombo") cm <- paste0("ssh -c 'tocomboSad.sh data/'")

  # loop through files
  for (i in 1:nrow(logres)) {
    # copy audio file
    logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
                                     to = paths$audiotarget_clean[i])
    # deal with working directories
    WD <- getwd()
    setwd(divime_loc)
    # run bash command
    xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
    setwd(WD)
    # remove audio file
    logres$audioremove[i] <- file.remove(paths$audiotarget_clean[i])

    output_file <- paste0(module, "Sad_", paths$root_clean[i], ".rttm")
    output_file_ori <- paste0(module, "Sad_", paths$root[i], ".rttm")


    # copy output back to source location and remove output from divime location
    outpath <- paste0(audio_loc, "/", paths$folder[i], output_file_ori)
    logres$resultscopy[i] <- file.copy(from = normalizePath(paste0(divime_loc, "/data/", output_file)),
                                      to = suppressWarnings(normalizePath(outpath)),
                                      overwrite = overwrite)
    logres$resultsremove[i] <- file.remove(normalizePath(paste0(divime_loc, "/data/", output_file)))

    logres$output[i] <- output_file

    # check for yunitator problem and log it
    X <- xres[grep("[[:digit:]]{1,10} Killed", xres)]
    if (length(X) > 0) {
      logres$yuniproblem[i] <- TRUE
      if (messages) message("[POTENTIAL PROBLEM]   :", paths$filestoprocess[i], "  -->  ", output_file)
      message("possibly yunitator problem with file: ", paths$filestoprocess[i])
    } else {
      logres$yuniproblem[i] <- FALSE
      if (messages) message(paths$filestoprocess[i], "  -->  ", output_file_ori)
    }

    # additional clean up
    if (module == "opensmile") {
      fn <- paste0(divime_loc, "/data/", paths$root_clean[i], ".txt")
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }
    # clean up
    rm(outpath, output_file, output_file_ori, X, xres)

  }

  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  logres
}
