
#' file and folder names
#'
#' @param audio_loc character, path to the audio files
#' @param divime_loc character, path to the DiViMe directory with a VM
#'
#' @details the functions also cleans file names (temporarily) to get rid of spaces and special characters in file names, which otherwise would cause problems with some of the DiViMe tools
#'
#' @return a data.frame with various versions of file paths and file names

handle_filenames <- function(audio_loc,
                             divime_loc) {
  # audio_loc = "~/Desktop/test_audio/"
  # divime_loc = "/Volumes/Data/VM2/ooo/DiViMe"

  audio_loc <- normalizePath(audio_loc, winslash = "/")
  divime_loc <- normalizePath(divime_loc, winslash = "/")

  pathto <- normalizePath(paste0(divime_loc, "/data/"), winslash = "/")
  filestoprocess <- list.files(audio_loc, pattern = ".wav", recursive = TRUE, ignore.case = TRUE)

  res <- data.frame(filestoprocess)

  # split directory structure
  folderlocs <- strsplit(filestoprocess, "/", fixed = TRUE)
  # get filenames without extension and subfolders
  fileroots <- unlist(lapply(folderlocs, function(X)X[length(X)]))
  res$root <- unlist(strsplit(fileroots, split = "(?i).wav"))
  # deal with special characters
  res$root_clean <- gsub(pattern = " ",
                         replacement = "_",
                         x = res$root,
                         fixed = TRUE)
  res$root_clean <- gsub(pattern = "(",
                         replacement = "_",
                         x = res$root_clean,
                         fixed = TRUE)
  res$root_clean <- gsub(pattern = ")",
                         replacement = "_",
                         x = res$root_clean,
                         fixed = TRUE)

  # the folder substructure
  folders <- lapply(folderlocs, function(X)X[-length(X)])
  i = 1
  for (i in 1:length(filestoprocess)) {
    if (length(folders[[i]]) == 0) {
      res$folder[i] <- ""
    } else {
      res$folder[i] <- paste0(paste(folders[[i]], collapse = "/"), "/")
    }
  }
  # original wave file name (necessary for extension with different case)
  audioname <- unlist(lapply(folderlocs, function(X)X[length(X)]))

  # paths for the audio files (for copying)
  res$audiosource <- paste0(audio_loc, "/", filestoprocess)
  res$audiotarget <- paste0(pathto, "/", res$root, ".wav")
  res$audiotarget_clean <- paste0(pathto, "/", res$root_clean, ".wav")

  # file size
  res$size <- round(file.size(res$audiosource)/1024000, 2)
  res

}
