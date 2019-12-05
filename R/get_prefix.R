#' get DiViMe prefixes for all rttm files in a location
#'
#' @param loc character, path that contains .rttm files
#' @param include_subfolders logical, include files in subfolder (default is \code{FALSE})
#'
#' @return a data frame
#' @export
#'
#' @examples
#' X <- dirname(list.files(system.file("", package = "avutils"), full.names = TRUE)[1])
#' get_prefix(X)
#' # with files in subfolders:
#' tdir <- tempdir()
#' dir.create(file.path(tdir, "test"))
#' dir.create(file.path(tdir, "test/subfolder"))
#' file.copy(from = system.file("yunitator_old_spanish.rttm", package = "avutils"),
#'           to = file.path(tdir, "test/subfolder/yunitator_old_spanish.rttm"))
#' file.copy(from = system.file("tocomboSad_BER_0485_12_07_09123.rttm", package = "avutils"),
#'           to = file.path(tdir, "test/tocomboSad_BER_0485_12_07_09123.rttm"))
#' get_prefix(file.path(tdir, "test"))
#' get_prefix(file.path(tdir, "test"), include_subfolders = TRUE)

get_prefix <- function(loc, include_subfolders = FALSE) {
  loc <- normalizePath(loc, winslash = "/", mustWork = FALSE)
  if (!dir.exists(loc)) {
    stop("location ", shQuote(loc), " not found", call. = FALSE)
  }

  # find all .rttm files
  X <- list.files(path = loc, pattern = "", full.names = TRUE, recursive = include_subfolders)
  X <- X[grepl(pattern = "\\.rttm", x = X)]

  # look for prefixes
  prefixes <- rep("", length(X))
  prefixes[grepl(pattern = "^noisemesSad_", x = basename(X))] <- "noisemesSad_"
  prefixes[grepl(pattern = "^opensmileSad_", x = basename(X))] <- "opensmileSad_"
  prefixes[grepl(pattern = "^tocomboSad_", x = basename(X))] <- "tocomboSad_"
  prefixes[grepl(pattern = "^noisemesFull_", x = basename(X))] <- "noisemesFull_"

  prefixes[grepl(pattern = "^diartk_noisemesSad_", x = basename(X))] <- "diartk_noisemesSad_"
  prefixes[grepl(pattern = "^diartk_opensmileSad_", x = basename(X))] <- "diartk_opensmileSad_"
  prefixes[grepl(pattern = "^diartk_tocomboSad_", x = basename(X))] <- "diartk_tocomboSad_"

  prefixes[grepl(pattern = "^yunitator_english_", x = basename(X))] <- "yunitator_english_"
  prefixes[grepl(pattern = "^yunitator_old_", x = basename(X))] <- "yunitator_old_"

  prefixes[grepl(pattern = "^WCE_noisemesSad_", x = basename(X))] <- "WCE_noisemesSad_"
  prefixes[grepl(pattern = "^WCE_opensmileSad_", x = basename(X))] <- "WCE_opensmileSad_"
  prefixes[grepl(pattern = "^WCE_tocomboSad_", x = basename(X))] <- "WCE_tocomboSad_"
  prefixes[grepl(pattern = "^WCE_yunitator_english_", x = basename(X))] <- "WCE_yunitator_english_"
  prefixes[grepl(pattern = "^WCE_yunitator_old_", x = basename(X))] <- "WCE_yunitator_old_"

  prefixes[grepl(pattern = "^vcm_", x = basename(X))] <- "vcm_"
  # prefixes[grepl(pattern = "^tocomboSad_", x = basename(X))] <- "tocomboSad_"

  # check for corresponding audio
  audio_names <- vapply(1:length(X), function(Z) {
    gsub(pattern = prefixes[Z], replacement = "", x = basename(X)[Z])
    },
    FUN.VALUE = "")
  audio_names <- gsub(pattern = ".rttm$", replacement = ".wav", x = audio_names)
  # audio_names[is.na(audio_names)] <- gsub(pattern = ".rttm$", replacement = ".wav", x = basename(X)[is.na(audio_names)])
  has_audio <- file.exists(file.path(dirname(X), audio_names))
  data.frame(rttm_name = basename(X),
             rttm_prefix = prefixes,
             audio_name = audio_names,
             audio_exists = has_audio,
             location = dirname(X),
             full_name = X,
             stringsAsFactors = FALSE)

}
