#' run the DiViMe noisemesFull SAD module
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @details in contrast to \code{divime_sad()} this function returns all the different noiseme classes
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export
#' @examples
#' \dontrun{
#' audio_loc <- "~/Desktop/test_audio/smallfiles/"
#' divime_loc <- "/Volumes/Data/VM2/ooo/DiViMe"
#' res <- divime_fullnoisemes(audio_loc = audio_loc, divime_loc = divime_loc, vmshutdown = FALSE)
#' }
#'

divime_fullnoisemes <- function(audio_loc,
                                divime_loc,
                                vmshutdown = TRUE,
                                messages = TRUE,
                                overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # vmshutdown = F; messages = TRUE; overwrite = TRUE

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
                       size = paths$size,
                       processed = FALSE,
                       ptime = NA,
                       outlines = NA,
                       output = NA,
                       audiocopy = NA,
                       audioremove = NA,
                       resultscopy = NA,
                       resultsremove = NA,
                       yuniproblem = NA)

  # create command
  cm <- paste0("ssh -c 'noisemesFull.sh data/'")

  # loop through files
  for (i in 1:nrow(logres)) {
    # take time stamp
    t1 <- Sys.time()

    # create names and locations for output rttm
    output_file <- paste0("noisemesFull_", paths$root_clean[i], ".rttm")
    output_file_ori <- paste0("noisemesFull_", paths$root[i], ".rttm")
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
      # copy audio file
      logres$audiocopy[i] <- file.copy(from = paths$audiosource[i],
                                       to = paths$audiotarget_clean[i])
      # deal with working directories
      WD <- getwd()
      setwd(divime_loc)

      # run bash command
      xres <- system2(command = vagrant,
                      args = cm,
                      stdout = TRUE,
                      stderr = TRUE)
      setwd(WD)

      # log number of lines in output
      logres$outlines[i] <- length(readLines(output_file_from))

      # copy output back to source location from divime location
      logres$resultscopy[i] <- file.copy(from = output_file_from,
                                         to = output_file_to,
                                         overwrite = overwrite)

      # clean audio file and output from divimi location
      logres$audioremove[i] <- file.remove(paths$audiotarget_clean[i])
      logres$resultsremove[i] <- file.remove(output_file_from)

      logres$output[i] <- output_file_ori
      logres$processed[i] <- TRUE

      # check for yunitator problem and log it
      X <- xres[grep("[[:digit:]]{1,10} Killed", xres)]
      if (length(X) > 0) {
        logres$yuniproblem[i] <- TRUE
        if (messages) message("[POTENTIAL PROBLEM]   :",
                              paths$filestoprocess[i],
                              "  -->  ",
                              output_file)
        message("possibly yunitator problem with file: ",
                paths$filestoprocess[i])
      } else {
        logres$yuniproblem[i] <- FALSE
        if (messages) message(paths$filestoprocess[i],
                              "  -->  ",
                              output_file_ori)
      }

      # clean up
      rm(X, xres)

    }

    # clean up
    rm(output_exists, output_file, output_file_from, output_file_ori, output_file_to)

    t2 <- Sys.time()
    logres$ptime[i] <- as.numeric(round(difftime(t2, t1, unit = "min"), 3))

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
