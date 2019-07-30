#' run the DiBiMe SAD module noisemes
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param cleanup logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#'
#' @return a data.frame with the locations of the created rttm files
#' @export
#'

divime_sad_noisemes <- function(audio_loc, divime_loc, cleanup = TRUE) {
  # audio_loc = "~/Desktop/audio_files/01/"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"

  audio_loc <- normalizePath(audio_loc)
  divime_loc <- normalizePath(divime_loc)

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
                       resultsremove = NA)
  if (copy) {
    logres$audiocopy <- file.copy(from = paste0(audio_loc, "/", filestoprocess),
                                  to = paste0(pathto, "/", filestoprocess))
  }

  cm <- paste0("ssh -c 'noisemesSad.sh data/'")
  WD <- getwd()
  setwd(divime_loc)
  system2(command = Sys.which("vagrant"), args = cm)
  setwd(WD)
  if (copy) {
    logres$audioremove <- file.remove(paste0(pathto, "/", filestoprocess))
  }

  output_files <- list.files(paste0(divime_loc, "/data"), recursive = TRUE, pattern = ".rttm")
  if (copy) {
    logres$resultscopy <- file.copy(from = paste0(divime_loc, "/data/", output_files),
                                    to = paste0(audio_loc, "/", output_files))
    logres$resultsremove <- file.remove(paste0(divime_loc, "/data/", output_files))
  }

  # shut down if requested
  if (cleanup) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)

  }

  logres$output <- output_files
  logres
}

# divime_sad_noisemes(audio_loc = "~/Desktop/audio_files/01/", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")
# divime_sad_noisemes(audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data", divime_loc = "/Volumes/Data/VM2/ooo/DiViMe")


