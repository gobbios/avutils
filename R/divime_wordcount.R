#' run the DiViMe word count estimator
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param speech_annos character, what kind of speech detection is available, see details
#' @param vmstart logical, perform a check whether the VM is running and if not start it up (by default \code{TRUE}). Turning this off, will speed up the function a little bit, but requires that you are sure that the VM is indeed running in \code{divime_loc}.
#' @param vmshutdown logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @param ... further arguments for \code{\link{copy_audio}}, see details
#' @details \code{speech_annos} needs to be one of the following: \code{"noisemes"}, \code{"opensmile"}, \code{"tocombo"} or \code{"yunitator_english"}. The first three can be obtained from \code{\link{divime_sad}} and the latter from \code{\link{divime_talkertype}}.
#'
#' In case you choose \code{speech_annos = "yunitator_english"} you might not get results unless you append your source audio file with some silence. This can be done inside the function here, and requires you setting two additional arguments to \code{divime_wordcount}: \code{appendsilence = 5} adds 5 seconds of silence, and \code{pathtosox=} requires the location of \code{sox}. See \code{\link{copy_audio}} and \code{\link{set_binaries}} for more details.
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export

divime_wordcount <- function(audio_loc,
                             divime_loc,
                             speech_annos = "opensmile",
                             vmstart = TRUE,
                             vmshutdown = TRUE,
                             messages = TRUE,
                             overwrite = FALSE,
                             ...) {
  # audio_loc = "~/Desktop/test_audio/onefile"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # speech_annos = "opensmile"; vmshutdown = F; messages = TRUE; overwrite = FALSE
  # appendsilence = NULL;
  # set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg", pathtosox = "~/Documents/utilities/sox");
  # pathtosox = getOption("avutils_sox")

  audio_loc <- normalizePath(audio_loc)
  divime_loc <- normalizePath(divime_loc)

  vagrant <- Sys.which("vagrant")

  # check VM state and start if necessary
  if (vmstart) {
    vm_running <- divime_vagrant_state(divime_loc = divime_loc,
                                       what = "status",
                                       silent = TRUE)
    if (vm_running %in% c("running (virtualbox)")) {
      vm_running <- TRUE
    } else {
      vm_running <- FALSE
    }
    if (!vm_running) {
      divime_vagrant_state(divime_loc = divime_loc,
                           what = "start",
                           silent = TRUE)
    }
  }

  paths <- avutils:::handle_filenames(audio_loc = audio_loc,
                                      divime_loc = divime_loc)

  logres <- data.frame(audio = paths$filestoprocess,
                       size = paths$size,
                       processed = FALSE,
                       ptime = NA,
                       outlines = NA,
                       sadfile = NA,
                       sadcopy = NA,
                       sadremove = NA,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       empty_sad = FALSE)

  # check for the speech detection data
  # and set command

  if (speech_annos %in% c("noisemes", "opensmile", "tocombo")) {
    prefix <- paste0(speech_annos, "Sad_")
    outprefix <- paste0("WCE_", speech_annos, "Sad_")
    cm <- paste0("ssh -c '~/launcher/WCE_from_SAD_outputs.sh /vagrant/data/ ", speech_annos, "Sad'")

    sad <- paste0(prefix, paths$root, ".rttm")
    sad_clean <- paste0(prefix, paths$root_clean, ".rttm")
    sadtarget <- paste0(divime_loc, "/data/", sad_clean)
    sadsource <- paste0(audio_loc, "/", paths$folder, sad)
  }
  if (speech_annos == "yunitator_english") {
    prefix <- "yunitator_english_"
    outprefix <- "WCE_yunitator_english_"
    cm <- paste0("ssh -c '~/launcher/WCE_from_SAD_outputs.sh /vagrant/data/ yunitator_english'")
    sad <- paste0(prefix, paths$root, ".rttm")
    sad_clean <- paste0(prefix, paths$root_clean, ".rttm")
    sadtarget <- paste0(divime_loc, "/data/", sad_clean)
    sadsource <- paste0(audio_loc, "/", paths$folder, sad)
  }


  # loop through files
  for (i in 1:nrow(logres)) {
    # take time stamp
    t1 <- Sys.time()

    # only run if the sad source was found...
    if (file.exists(sadsource[i])) {
      output_file <- paste0(outprefix, paths$root_clean[i], ".rttm")
      output_file_ori <- paste0(outprefix, paths$root[i], ".rttm")
      output_file_to <- normalizePath(paste0(audio_loc, "/", paths$folder[i], output_file_ori),
                                      winslash = "/",
                                      mustWork = FALSE)
      output_file_from <- normalizePath(paste0(divime_loc, "/data/", output_file),
                                        winslash = "/",
                                        mustWork = FALSE)

      # if overwrite = FALSE, continue only if the target file does not yet exist
      # if it already exists, we can skip the processing in the VM
      output_exists <- file.exists(output_file_to)

      if (!(!overwrite & output_exists)) {
        # copy audio
        # logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
        #                                 to = paths$audiotarget_clean[i])
        logres$audiocopy[i] <- copy_audio(from = paths$audiosource[i], to = paths$audiotarget_clean[i], ...)
        # logres$audiocopy[i] <- copy_audio(from = paths$audiosource[i], to = paths$audiotarget_clean[i], appendsilence = appendsilence, pathtosox = pathtosox)
        # copy sad file
        logres$sadfile[i] <- sad[i]
        logres$sadcopy[i] <- file.copy(from = sadsource[i],
                                       to = sadtarget[i])

        # check whether SAD file is empty:
        emptysad <- length(readLines(sadtarget[i])) == 0

        # deal with working directories
        WD <- getwd()
        setwd(divime_loc)

        # run bash command
        xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
        setwd(WD)

        # in case of empty input Sad, write an empty output for
        if (emptysad) {
          if (!file.exists(output_file_from)) {
            file.create(output_file_from)
          }
          logres$empty_sad[i] <- TRUE
        }

        # log number of lines in output
        logres$outlines[i] <- length(readLines(output_file_from))

        # remove source files (audio and SAD)
        logres$audioremove[i] <- file.remove(paths$audiotarget_clean[i])
        logres$sadremove[i] <- file.remove(sadtarget[i])

        # copy output back to source location from divime location
        logres$resultscopy[i] <- file.copy(from = output_file_from,
                                           to = output_file_to,
                                           overwrite = overwrite)
        logres$resultsremove[i] <- file.remove(output_file_from)

        logres$output[i] <- output_file_ori
        logres$processed[i] <- TRUE

        if (messages) message(paths$filestoprocess[i], "  -->  ", output_file_ori)

        # clean up
        rm(xres, WD)

        # clean up temp directories
        unlink(file.path(divime_loc, "data/wav_tmp"), recursive = TRUE)
        unlink(file.path(divime_loc, "data/WCE_VM_TEMP"), recursive = TRUE)

      } # skip if not overwrite and output exists

      # clean up
      rm(output_exists, output_file, output_file_from, output_file_ori, output_file_to)
    } else {
      if (messages) message(paths$filestoprocess[i], "  -->  ", "XXXXXXXXX")
    } # only do if sad file exists

    # time stamp again
    t2 <- Sys.time()
    logres$ptime[i] <- as.numeric(round(difftime(t2, t1, units = "min"), 3))

    # predict time left
    temp <- na.omit(logres[, c("ptime", "size")])
    sizes <- logres$size[is.na(logres$ptime)]
    if (nrow(temp) > 1) {
      tempres <- lm(ptime ~ size, temp)
      if (length(sizes) > 0) {
        timeleft <- round(sum(predict(tempres, newdata = data.frame(size = sizes))), 1)
        cat("expected time until finish: ", timeleft, " minutes\n")
      }
    }

  } # go through file by file loop

  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  # print warning if empty files were created:
  if (sum(logres$empty_sad) > 0) {
    warning("there were empty input SAD files, hence empty word count files were produced")
  }

  logres
}
