#' run the DiViMe vocal classification module vcm
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param marvinator logical, look for Marvin's version of yunitator results, by default \code{TRUE}
#' @param vmstart logical, perform a check whether the VM is running and if not start it up (by default \code{TRUE}). Turning this off, will speed up the function a little bit, but requires that you are sure that the VM is indeed running in \code{divime_loc}.
#' @param vmshutdown logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @details The \code{marvinator=} argument determines which kind of yunitator file is looked for. If \code{TRUE} the file looked for is 'yunitator_english_X.rttm', where X is the name of the underlying audio file (which is the result of \code{divime_talkertype(..., marvinator = TRUE)}). If \code{FALSE} the file looked for is 'yunitator_old_X.rttm' (which is the result of \code{divime_talkertype(..., marvinator = FALSE)}). If the respective file is not found, the function aborts.
#' @export

divime_classify_vox <- function(audio_loc,
                                divime_loc,
                                marvinator = TRUE,
                                vmstart = TRUE,
                                vmshutdown = TRUE,
                                messages = TRUE,
                                overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/onefile"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # vmshutdown = F; messages = TRUE; overwrite = FALSE
  # marvinator = TRUE

  audio_loc <- normalizePath(audio_loc)
  divime_loc <- normalizePath(divime_loc)

  vagrant <- Sys.which("vagrant")

  # check VM state and start if necessary
  if (vmstart) {
    vm_running <- divime_vagrant_state(divime_loc = divime_loc,
                                       what = "status",
                                       silent = TRUE)
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
                       yunifile = NA,
                       yunicopy = NA,
                       yuniremove = NA,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA) #, yuniproblem = NA

  # set command
  if (marvinator) {
    cm <- paste0("ssh -c 'vcm.sh data/ english'")
    fileprefix <- "yunitator_english_"
    outprefix <- "yunitator_english_"
  } else {
    cm <- paste0("ssh -c 'vcm.sh data/'")
    fileprefix <- "yunitator_old_"
    outprefix <- "yunitator_universal_"
  }


  # loop through files
  for (i in 1:nrow(logres)) {
    # take time stamp
    t1 <- Sys.time()

    # only run if the yuni source was found in the source folder...
    yunifrom <- paste0(audio_loc, "/", paths$folder[i], fileprefix, paths$root[i], ".rttm")
    yunito <- paste0(divime_loc, "/data/", paste0(outprefix, paths$root_clean[i], ".rttm"))

    if (file.exists(yunifrom)) {
      output_file <- paste0("vcm_", paths$root_clean[i], ".rttm")
      output_file_ori <- paste0("vcm_", paths$root[i], ".rttm")
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
        logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
                                         to = paths$audiotarget_clean[i])
        logres$yunifile[i] <- paste0(fileprefix, paths$root[i], ".rttm")
        logres$yunicopy[i] <- file.copy(from = yunifrom,
                                        to = yunito)
        # deal with working directories
        WD <- getwd()
        setwd(divime_loc)

        # run bash command
        xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
        setwd(WD)

        # log number of lines in output
        logres$outlines[i] <- length(readLines(output_file_from))

        # remove source files (audio and ynui)
        logres$audioremove[i] <- file.remove(paths$audiotarget_clean[i])
        logres$yuniremove[i] <- file.remove(yunito)

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
      } else {
        if (messages) message(paths$filestoprocess[i], "  -->  ", "XXXXXXXXX")
      }

      # clean up
      rm(output_exists, output_file, output_file_from, output_file_ori, output_file_to)

    } else {
      if (messages) message(paths$filestoprocess[i], "  -->  ", "XXXXXXXXX")
      logres$yunifile[i] <- "not found"
    }

    # more clean up
    rm(yunifrom, yunito)

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
  }

  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  logres
}
