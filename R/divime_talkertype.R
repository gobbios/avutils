#' run the DiViMe talker type module (yunitate)
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export
#'

divime_talkertype <- function(audio_loc,
                              divime_loc,
                              vmshutdown = TRUE,
                              messages = TRUE,
                              overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"

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

  # run check to see whether file names contain spaces (which are potentially problematic)
  if (sum(grepl(pattern = " ", x = paths$root)) > 0) {
    cat("at least one audio file has a space in its name, which might cause problems\n")
  }

  logres <- data.frame(audio = paths$filestoprocess,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       yuniproblem = NA)

  # create command depending on the desired module
  cm <- paste0("ssh -c 'yunitate.sh data/'")

  # loop through files
  for (i in 1:nrow(logres)) {
    # copy audio file
    logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
                                     to = paths$audiotarget[i])
    # deal with working directories
    WD <- getwd()
    setwd(divime_loc)
    # run bash command
    xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
    setwd(WD)
    # remove audio file
    logres$audioremove[i] <- file.remove(paths$audiotarget[i])

    output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                              recursive = TRUE,
                              pattern = "yunitat")

    # copy output back to source location and remove output from divime location
    outpath <- paste0(audio_loc, "/", paths$folder[i], output_file)
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
      if (messages) message(paths$filestoprocess[i], "  -->  ", output_file)
    }
    # clean up
    rm(outpath, output_file, X, xres)

  }

  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  logres
}
