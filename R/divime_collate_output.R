#' combine .rttm output from DiViMe tools
#'
#' @param data_loc character, the location for the data folder (which may or may not be the same as the one where the source audio files are stored)
#'
#' @return a list with 8 components (for three SADs the actual SAD and corresponding diarization, yuni results and vcm)
#' @export

divime_collate_output <- function(data_loc) {
  # data_loc = "~/Desktop/test_audio/"

  data_loc <- normalizePath(data_loc, winslash = "/")

  # get names (and subfolders)
  paths <- data.frame(folder = list.files(data_loc, pattern = ".rttm", recursive = TRUE), stringsAsFactors = FALSE)
  paths$folder_full <- list.files(data_loc, pattern = ".rttm", recursive = TRUE, full.names = TRUE)
  # get names
  temp <- strsplit(x = paths$folder, split = "/", fixed = TRUE)
  paths$names <- unlist(lapply(temp, function(X)X[length(X)]))
  paths$roots <- unlist(strsplit(x = paths$names, split = ".rttm", fixed = TRUE))

  # searchstring="diartk.*noisemes"
  # splitstring="diartk_noisemesSad_"
  # pathdata = paths
  extract_foo <- function(searchstring, splitstring, pathdata) {
    # create output
    tempres <- matrix(ncol = 9, nrow = 0)
    fileroots <- c()
    # find relevant files
    x <- which(grepl(pattern = searchstring, x = pathdata$roots))
    # process files
    i = x[3]
    if (length(x) > 0) {
      for (i in x) {
        check <- readLines(pathdata$folder_full[i])
        dn <- strsplit(x = pathdata$roots[i], split = splitstring, fixed = TRUE)
        if (length(check) > 0) {
          temp <- read.table(pathdata$folder_full[i], header = FALSE)
          temp[, 2] <- dn[[1]][2]
          tempres <- rbind(tempres, temp)
        }
        fileroots <- c(fileroots, dn[[1]][2])
      }
      # clean up path data
      pathdata <- pathdata[-x, ]
    }

    # return extract data and updated file locations
    list(annos = tempres, pathdata = pathdata, fileroots = unique(fileroots))
  }

  allres <- list()
  # overview
  res <- data.frame()

  # full noisesmes first
  temp <- extract_foo(searchstring = "noisemesFull",
                      splitstring = "noisemesFull_",
                      pathdata = paths)
  allres$diartk_noisemes <- temp$annos
  paths <- temp$pathdata

  # then diarization
  temp <- extract_foo(searchstring = "diartk.*noisemes",
                      splitstring = "diartk_noisemesSad_",
                      pathdata = paths)
  allres$diartk_noisemes <- temp$annos
  paths <- temp$pathdata

  temp <- extract_foo(searchstring = "diartk.*opensmile",
                      splitstring = "diartk_opensmileSad_",
                      pathdata = paths)
  allres$diartk_opensmile <- temp$annos
  paths <- temp$pathdata

  temp <- extract_foo(searchstring = "diartk.*tocombo",
                      splitstring = "diartk_tocomboSad_",
                      pathdata = paths)
  allres$diartk_tocombo <- temp$annos
  paths <- temp$pathdata

  # now the SAD modules
  temp <- extract_foo(searchstring = "noisemes",
                      splitstring = "noisemesSad_",
                      pathdata = paths)
  allres$noisemes <- temp$annos
  paths <- temp$pathdata

  temp <- extract_foo(searchstring = "opensmile",
                      splitstring = "opensmileSad_",
                      pathdata = paths)
  allres$opensmile <- temp$annos
  paths <- temp$pathdata

  temp <- extract_foo(searchstring = "tocombo",
                      splitstring = "tocomboSad_",
                      pathdata = paths)
  allres$tocombo <- temp$annos
  paths <- temp$pathdata

  # yunitator
  temp <- extract_foo(searchstring = "yunitat",
                      splitstring = "yunitator_old_",
                      pathdata = paths)
  allres$yuni <- temp$annos
  paths <- temp$pathdata

  # vcm
  temp <- extract_foo(searchstring = "vcm",
                      splitstring = "vcm_",
                      pathdata = paths)
  allres$vcm <- temp$annos
  paths <- temp$pathdata

  allres
}
