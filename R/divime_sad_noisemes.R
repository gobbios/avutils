#' run the DiBiMe SAD module noisemes
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param cleanup logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#'
#' @return a data.frame with the locations of the created rttm files
#' @export
#'

divime_sad_noisemes <- function(audio_loc,
                                divime_loc,
                                cleanup = TRUE,
                                messages = TRUE) {
  # audio_loc = "~/Desktop/audio_files/batch0"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data"
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

  # if files are already in the divime data folder, copying can be skipped
  if (grepl(pattern = divime_loc, x = audio_loc, ignore.case = TRUE)) {
    copy <- FALSE
  } else {
    copy <- TRUE
  }

  pathto <- normalizePath(paste0(divime_loc, "/data/"))

  filestoprocess <- list.files(audio_loc, pattern = ".wav", recursive = TRUE)

  if (copy) {
    folderstructure <- list.dirs(audio_loc, recursive = TRUE, full.names = FALSE)
    folderstructure <- folderstructure[folderstructure != ""]
    for (i in folderstructure) {
      dir.create(paste0(pathto, i), recursive = FALSE, showWarnings = FALSE)
    }
  }

  filestoprocessroots <- unlist(strsplit(filestoprocess, split = ".wav"))

  logres <- data.frame(audio = filestoprocess,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       yuniproblem = NA)

  # loop through files
  for (i in 1:nrow(logres)) {
    if (copy) {
      logres$audiocopy[i] <- file.copy(from = paste0(audio_loc, "/", filestoprocess[i]),
                                    to = paste0(pathto, "/", filestoprocess[i]))
    }

    cm <- paste0("ssh -c 'noisemesSad.sh data/'")
    WD <- getwd()
    setwd(divime_loc)
    xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
    setwd(WD)
    if (copy) {
      logres$audioremove[i] <- file.remove(paste0(pathto, "/", filestoprocess[i]))
    }

    output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                              recursive = TRUE,
                              pattern = ".rttm")
    if (copy) {
      logres$resultscopy[i] <- file.copy(from = normalizePath(paste0(divime_loc, "/data/", output_file)),
                                      to = suppressWarnings(normalizePath(paste0(audio_loc, "/", output_file))))
      logres$resultsremove[i] <- file.remove(normalizePath(paste0(divime_loc, "/data/", output_file)))
    }
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

  }


  # shut down if requested
  if (cleanup) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)

  }

  logres
}

# divime_sad_noisemes(audio_loc = "~/Desktop/audio_files/batch0", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")
# divime_sad_noisemes(audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")


