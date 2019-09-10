#' run all DiViMe modules
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @return a list of data.frames with the locations of the created rttm files and some diagnostics
#' @export

divime_run_all <- function(audio_loc,
                           divime_loc,
                           vmshutdown = TRUE,
                           messages = TRUE,
                           overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/smallfiles"
  # audio_loc = "~/Desktop/error-producing files"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # vmshutdown = F; messages = TRUE; overwrite = FALSE
  logres <- list()

  temp1 <- divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "noisemes", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$sad_noisemes <- temp1

  temp2 <- divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "opensmile", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$sad_opensmile <- temp2

  temp3 <- divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "tocombo", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$sad_tocombo <- temp3

  temp4 <- divime_diarization(audio_loc = audio_loc, divime_loc = divime_loc, speech_annos = "noisemes", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$diartk_noisemes <- temp4

  temp5 <- divime_diarization(audio_loc = audio_loc, divime_loc = divime_loc, speech_annos = "opensmile", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$diartk_opensmile <- temp5

  temp6 <- divime_diarization(audio_loc = audio_loc, divime_loc = divime_loc, speech_annos = "tocombo", vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$diartk_tocombo <- temp6

  temp7 <- divime_talkertype(audio_loc = audio_loc, divime_loc = divime_loc, vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$yuni <- temp7

  temp8 <- divime_classify_vox(audio_loc = audio_loc, divime_loc = divime_loc, vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$vcm <- temp8

  temp9 <- divime_fullnoisemes(audio_loc = audio_loc, divime_loc = divime_loc, vmshutdown = FALSE, messages = messages, overwrite = overwrite)
  logres$fullnoisemes <- temp9

  logres
}
