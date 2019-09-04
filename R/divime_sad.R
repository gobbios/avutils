#' run a DiViMe SAD module
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param module character, which module to execute (default is \code{"noisemes"}), see details
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @details \code{module=} sets the SAD module to be used: can be either \code{"noisemes"}, \code{"opensmile"} or \code{"tocombo"}
#' @return a data.frame with the locations of the created rttm files
#' @export
#'

divime_sad <- function(audio_loc,
                       divime_loc,
                       module = "noisemes",
                       vmshutdown = TRUE,
                       messages = TRUE,
                       overwrite = FALSE) {
  # audio_loc = "~/Desktop/audio_files/batch0"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data/"
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
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

  pathto <- normalizePath(paste0(divime_loc, "/data/"))

  filestoprocess <- list.files(audio_loc, pattern = ".wav", recursive = TRUE)

  # run check to see whether file names contain spaces (which are problematic)



  filestoprocessroots <- unlist(strsplit(filestoprocess, split = ".wav"))
  filestoprocessrootsfolders <- strsplit(x = filestoprocess, split = "/", fixed = TRUE)

  logres <- data.frame(audio = filestoprocess,
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
    if (length(filestoprocessrootsfolders[[i]]) > 1) {
      xname <- filestoprocessrootsfolders[[i]][length(filestoprocessrootsfolders[[i]])]
    } else {
      xname <- filestoprocess[i]
    }
    logres$audiocopy[i] <- file.copy(from = paste0(audio_loc, "/", filestoprocess[i]),
                                     to = paste0(pathto, "/", xname))
    # deal with working directories
    WD <- getwd()
    setwd(divime_loc)
    # run bash command
    xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
    setwd(WD)
    # remove audio file
    logres$audioremove[i] <- file.remove(paste0(pathto, "/", xname))

    output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                              recursive = TRUE,
                              pattern = module)

    # copy output back to source location and remove output from divime location
    if (length(filestoprocessrootsfolders[[i]]) > 1) {
      xname <- filestoprocessrootsfolders[[i]][-c(length(filestoprocessrootsfolders[[i]]))]
      xname <- paste0(paste(c(audio_loc, xname), collapse = "/"), "/", output_file)
    } else {
      xname <- paste0(audio_loc, "/", output_file)
    }
    logres$resultscopy[i] <- file.copy(from = normalizePath(paste0(divime_loc, "/data/", output_file)),
                                      to = suppressWarnings(normalizePath(xname)),
                                      overwrite = overwrite)
    logres$resultsremove[i] <- file.remove(normalizePath(paste0(divime_loc, "/data/", output_file)))

    logres$output[i] <- output_file

    # check for yunitator problem and log it
    X <- xres[grep("[[:digit:]]{1,10} Killed", xres)]
    if (length(X) > 0) {
      logres$yuniproblem[i] <- TRUE
      if (messages) message("[POTENTIAL PROBLEM]   :", filestoprocess[i], "  -->  ", output_file)
      message("possibly yunitator problem with file: ", filestoprocess[i])
    } else {
      logres$yuniproblem[i] <- FALSE
      if (messages) message(filestoprocess[i], "  -->  ", output_file)
    }

    # additional clean up
    if (module == "opensmile") {
      fn <- unlist(strsplit(filestoprocessroots[i], split = "/", fixed = TRUE))
      fn <- fn[length(fn)]
      fn <- paste0(divime_loc, "/data/", fn, ".txt")
      if (file.exists(fn)) {
        file.remove(fn)
      }
    }
  }


  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  logres
}

# divime_sad_noisemes(audio_loc = "~/Desktop/audio_files/batch0", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")
# divime_sad_noisemes(audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")


