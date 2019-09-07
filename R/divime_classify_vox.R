#' run the DiViMe vocal classification module vcm
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with running VM
#' @param vmshutdown logical, should the VM be shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @return a data.frame with the locations of the created rttm files and some diagnostics
#' @export

divime_classify_vox <- function(audio_loc,
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

  # set command
  cm <- paste0("ssh -c 'vcm.sh data/'")

  logres <- data.frame(audio = paths$filestoprocess,
                       yunifile = NA,
                       yunicopy = NA,
                       yuniremove = NA,
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

    # only run if the yuni source was found in the source folder...
    yunifile <- list.files(path = audio_loc, pattern = paths$root[i], recursive = TRUE, ignore.case = TRUE, full.names = FALSE)
    yunifile <- yunifile[grep(pattern = "yunitat", x = yunifile)]
    yunifile <- unlist(strsplit(yunifile, "/", fixed = TRUE))
    yunifile <- yunifile[length(yunifile)]

    check <- FALSE
    if (length(yunifile) == 1) {
      yunifrom <- paste0(audio_loc, "/", paths$folder[i], yunifile)
      yunito <- paste0(divime_loc, "/data/", paste0("yunitator_universal_", paths$root[i], ".rttm"))
      if (file.exists(yunifrom)) {
        check <- TRUE
      }
    }
    if (check) {
      logres$yunifile[i] <- yunifile
      logres$yunicopy[i] <- file.copy(from = yunifrom,
                                     to = yunito)

      WD <- getwd()
      setwd(divime_loc)
      xres <- system2(command = vagrant, args = cm, stdout = TRUE, stderr = TRUE)
      setwd(WD)

      logres$audioremove[i] <- file.remove(paths$audiotarget[i])
      logres$yuniremove[i] <- file.remove(yunito)

      output_file <- list.files(normalizePath(paste0(divime_loc, "/data")),
                                recursive = FALSE,
                                pattern = "vcm_")

      outpath <- paste0(audio_loc, "/", paths$folder[i], output_file)

      logres$resultscopy[i] <- file.copy(from = paste0(divime_loc, "/data/", output_file),
                                         to = suppressWarnings(outpath),
                                         overwrite = overwrite)
      logres$resultsremove[i] <- file.remove(paste0(divime_loc, "/data/", output_file))

      logres$output[i] <- output_file

      if (messages) message(paths$filestoprocess[i], "  -->  ", output_file)

      # clean up
      rm(yunifrom, yunito, outpath, output_file)

    } else {
      if (messages) message(paths$filestoprocess[i], "  -->  ", "XXXXXXXXX")
    }

    # more clean up
    rm(yunifile)
  }

  # shut down if requested
  if (vmshutdown) {
    divime_vagrant_state(divime_loc = divime_loc,
                         what = "halt",
                         silent = TRUE)
  }

  logres
}
