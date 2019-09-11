#' run all DiViMe modules
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#' @param vmstart logical, perform a check whether the VM is running and if not start it up (by default \code{TRUE}). Turning this off, will speed up the function a little bit, but requires that you are sure that the VM is indeed running in \code{divime_loc}.
#' @param vmshutdown logical, should the VM shut down after the operations are done (by default \code{TRUE})
#' @param messages logical, should the file names of each processed file be printed
#' @param overwrite logical, should output files be overwritten if they already exist (default is \code{FALSE})
#' @return a list of data.frames with the locations of the created rttm files and some diagnostics
#' @export

divime_run_all <- function(audio_loc,
                           divime_loc,
                           vmstart = TRUE,
                           vmshutdown = TRUE,
                           messages = TRUE,
                           overwrite = FALSE) {
  # audio_loc = "~/Desktop/test_audio/smallfiles"
  # audio_loc = "~/Desktop/error-producing files"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"
  # vmstart = T; vmshutdown = F; messages = TRUE; overwrite = FALSE
  logres <- list()

  cat("###### SAD noisemes ###########\n")
  temp1 <- divime_sad(audio_loc = audio_loc,
                      divime_loc = divime_loc,
                      module = "noisemes",
                      vmstart = vmstart,
                      vmshutdown = FALSE,
                      messages = messages,
                      overwrite = overwrite)
  logres$sad_noisemes <- temp1

  cat("###### SAD opensmile ###########\n")
  temp2 <- divime_sad(audio_loc = audio_loc,
                      divime_loc = divime_loc,
                      module = "opensmile",
                      vmstart = FALSE,
                      vmshutdown = FALSE,
                      messages = messages,
                      overwrite = overwrite)
  logres$sad_opensmile <- temp2

  cat("###### SAD tocombo ###########\n")
  temp3 <- divime_sad(audio_loc = audio_loc,
                      divime_loc = divime_loc,
                      module = "tocombo",
                      vmstart = FALSE,
                      vmshutdown = FALSE,
                      messages = messages,
                      overwrite = overwrite)
  logres$sad_tocombo <- temp3

  cat("###### diartk noisemes ###########\n")
  temp4 <- divime_diarization(audio_loc = audio_loc,
                              divime_loc = divime_loc,
                              speech_annos = "noisemes",
                              vmstart = FALSE,
                              vmshutdown = FALSE,
                              messages = messages,
                              overwrite = overwrite)
  logres$diartk_noisemes <- temp4

  cat("###### diartk opensmile ###########\n")
  temp5 <- divime_diarization(audio_loc = audio_loc,
                              divime_loc = divime_loc,
                              speech_annos = "opensmile",
                              vmstart = FALSE,
                              vmshutdown = FALSE,
                              messages = messages,
                              overwrite = overwrite)
  logres$diartk_opensmile <- temp5

  cat("###### diartk tocombo ###########\n")
  temp6 <- divime_diarization(audio_loc = audio_loc,
                              divime_loc = divime_loc,
                              speech_annos = "tocombo",
                              vmstart = FALSE,
                              vmshutdown = FALSE,
                              messages = messages,
                              overwrite = overwrite)
  logres$diartk_tocombo <- temp6

  cat("###### yunitate ###########\n")
  temp7 <- divime_talkertype(audio_loc = audio_loc,
                             divime_loc = divime_loc,
                             vmstart = FALSE,
                             vmshutdown = FALSE,
                             messages = messages,
                             overwrite = overwrite)
  logres$yuni <- temp7

  cat("###### vcm ###########\n")
  temp8 <- divime_classify_vox(audio_loc = audio_loc,
                               divime_loc = divime_loc,
                               vmstart = FALSE,
                               vmshutdown = FALSE,
                               messages = messages,
                               overwrite = overwrite)
  logres$vcm <- temp8

  cat("###### full noisemes ###########\n")
  temp9 <- divime_fullnoisemes(audio_loc = audio_loc,
                               divime_loc = divime_loc,
                               vmstart = FALSE,
                               vmshutdown = vmshutdown,
                               messages = messages,
                               overwrite = overwrite)
  logres$fullnoisemes <- temp9

  logres
}
