#' run the DiViMe diarization module diartk
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param speech_annos character, what kind of speech detection is available, see details
#' @param vmshutdown logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @details \code{speech_annos} needs to be one of the following: \code{"noisemes"}, \code{"opensmile"}, \code{"tocombo"} or \code{"custom"}. Currently, \code{"custom"} is not yet implemented.
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export

divime_diarization <- function(audio_loc,
                               divime_loc,
                               speech_annos = "noisemes",
                               vmshutdown = TRUE,
                               messages = TRUE,
                               overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # speech_annos = "noisemes"

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

  # check for the speech detection data
  # and set command
  if (speech_annos == "noisemes") {
    prefix <- "noisemesSad_"
    cm <- paste0("ssh -c 'diartk.sh data/ noisemesSad'")
  }
  if (speech_annos == "opensmile") {
    prefix <- "opensmileSad_"
    cm <- paste0("ssh -c 'diartk.sh data/ opensmileSad'")
  }
  if (speech_annos == "tocombo") {
    prefix <- "tocomboSad_"
    cm <- paste0("ssh -c 'diartk.sh data/ tocomboSad'")
  }

  sad <- paste0(prefix, paths$root, ".rttm")
  sadtarget <- paste0(divime_loc, "/data/", sad)
  sadsource <- paste0(audio_loc, "/", paths$folder, sad)

  logres <- data.frame(audio = paths$filestoprocess,
                       sadfile = NA,
                       sadcopy = NA,
                       sadremove = NA,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA) #, yuniproblem = NA

  # loop through files
  for (i in 1:nrow(logres)) {
    # copy audio
    logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
                                     to = paths$audiotarget[i])

    # only run if the sad source was found...
    if (file.exists(sadsource[i])) {
      logres$sadfile[i] <- sad[i]
      logres$sadcopy[i] <- file.copy(from = sadsource[i],
                                     to = sadtarget[i])

      WD <- getwd()
      setwd(divime_loc)
      xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
      setwd(WD)

      logres$audioremove[i] <- file.remove(paths$audiotarget[i])
      logres$sadremove[i] <- file.remove(sadtarget[i])

      output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                                recursive = FALSE,
                                pattern = "diartk_")

      outpath <- paste0(audio_loc, "/", paths$folder[i], output_file)

      logres$resultscopy[i] <- file.copy(from = paste0(divime_loc, "/data/", output_file),
                                         to = suppressWarnings(outpath),
                                         overwrite = overwrite)
      logres$resultsremove[i] <- file.remove(paste0(divime_loc, "/data/", output_file))

      logres$output[i] <- output_file

      if (messages) message(paths$filestoprocess[i], "  -->  ", output_file)
    } else {
      if (messages) message(paths$filestoprocess[i], "  -->  ", "XXXXXXXXX")
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
