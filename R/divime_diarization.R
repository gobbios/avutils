#' run the DiViMe diarization module diartk
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param speech_annos character, what kind of speech detection is available
#' @param vmshutdown logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @details \code{speech_annos} needs to be one of the following: \code{"noisemes"}, \code{"opensmile"}, \code{"tocombo"} or \code{"custom"}. Currently, only \code{'noisemes'} is implemented.
#' @return a data.frame with the locations of the created rttm files
#' @export
#'

divime_diarization <- function(audio_loc,
                               divime_loc,
                               speech_annos = "noisemes",
                               vmshutdown = TRUE,
                               messages = TRUE,
                               overwrite = FALSE) {
  # audio_loc = "~/Desktop/audio_files/batch0"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data"
  # audio_loc = "/Volumes/Data/VM2/ooo/DiViMe/data/"
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

  pathto <- normalizePath(paste0(divime_loc, "/data/"))

  filestoprocess <- list.files(audio_loc, pattern = ".wav", recursive = TRUE)
  folderlocs <- strsplit(filestoprocess, "/", fixed = TRUE)
  folders <- lapply(folderlocs, function(X)X[-length(X)])
  wavfiles <- unlist(lapply(folderlocs, function(X)X[length(X)]))
  fileroots <- unlist(lapply(folderlocs, function(X)X[length(X)]))
  fileroots <- unlist(strsplit(fileroots, ".wav"))

  # filestoprocessroots <- unlist(strsplit(filestoprocess, split = ".wav"))
  # filestoprocessrootsfolders <- strsplit(x = filestoprocess, split = "/", fixed = TRUE)
  # filestoprocessroots2 <- unlist(lapply(filestoprocessrootsfolders, function(X)X[length(X)]))
  # filestoprocessroots3 <- lapply(filestoprocessrootsfolders, function(X)X[-length(X)])

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

  sadfiles <- character(length(filestoprocess))
  for (i in 1:length(sadfiles)) {
    if (length(folders[[i]]) == 0) {
      sadfiles[i] <- paste0(prefix, fileroots[i], ".rttm")
    } else {
      sadfiles[i] <- paste0(paste(folders[[i]], collapse = "/"), "/", prefix, fileroots[i], ".rttm")
    }
  }
  sadfilesnofolder <- strsplit(sadfiles, "/", fixed = TRUE)
  sadfilesnofolder <- unlist(lapply(sadfilesnofolder, function(X)X[length(X)]))

  logres <- data.frame(audio = filestoprocess,
                       sadfile = NA,
                       sadcopy = NA,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       yuniproblem = NA)

  # loop through files
  for (i in 1:nrow(logres)) {
    # copy audio
    logres$audiocopy[i] <- file.copy(from = paste0(audio_loc, "/", filestoprocess[i]),
                                     to = paste0(pathto, "/", wavfiles[i]))
    if (file.exists(paste0(audio_loc, "/", sadfiles[i]))) {
      logres$sadfile[i] <- sadfiles[i]
      logres$sadcopy[i] <- file.copy(from = paste0(audio_loc, "/", sadfiles[i]),
                                     to = paste0(pathto, "/", sadfilesnofolder[i]))
    }

    WD <- getwd()
    setwd(divime_loc)
    xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
    setwd(WD)

    logres$audioremove[i] <- file.remove(paste0(pathto, "/", wavfiles[i]))
    logres$audioremove[i] <- file.remove(paste0(pathto, "/", sadfilesnofolder[i]))


    output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                              recursive = FALSE,
                              pattern = "diartk_")


    if (length(folders[[i]]) == 0) {
      targetforoutput <- paste0(audio_loc, "/", output_file)
    } else {
      targetforoutput <- paste0(audio_loc, "/", paste(folders[[i]], collapse = "/"), "/", output_file)
    }


    logres$resultscopy[i] <- file.copy(from = normalizePath(paste0(divime_loc, "/data/", output_file)),
                                       to = suppressWarnings(normalizePath(targetforoutput)),
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
      if (messages) message(filestoprocess[i], " (+ ", sadfiles[i], ")",  "  -->  ", output_file)
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
