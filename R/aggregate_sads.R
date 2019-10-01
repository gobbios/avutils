#' aggregate SAD files from DiViMe and (manually) annotated ELAN .eaf files
#'
#' for validation purposes
#'
#' @param sadloc character, the location of the DiViMe SAD result files (rttm)
#' @param elanloc character, the location of the ELAN .eaf files
#' @param segment_dur duration for the length of a segment (default is \code{60} (seconds))
#' @param ... additional parameters for elan files (which tiers/annotations to ignore), see \code{\link{collapse_tiers}}
#'
#' @return a \code{data.frame} with columns for each of the SADs and the duration of speech per segment
#' @export
#'
aggregate_sads <- function(sadloc, elanloc, segment_dur = 60, ...) {

  # read elan if provided
  if (!is.null(elanloc)) {
    elan_files <- list.files(elanloc, pattern = ".eaf", full.names = TRUE)
    elan_files <- elan_files[!grepl(pattern = ".eaf.", x = elan_files, fixed = TRUE)]
  }

  # or use all SAD files that are found in DiViMe SAD location
  if (is.null(elanloc)) {
    n_files <- list.files(sadloc, pattern = "noisemesSad_", full.names = TRUE)
    n_files <- n_files[!grepl("diartk_", n_files)]
    n_roots <- basename(n_files)
    n_roots <- unlist(lapply(strsplit(n_roots, split = "noisemesSad_"), function(X)X[2]))
    n_roots <- unlist(strsplit(n_roots, ".rttm", fixed = TRUE))

    nfull_files <- list.files(sadloc, pattern = "noisemesFul_", full.names = TRUE)
    nfull_files <- nfull_files[!grepl("diartk_", nfull_files)]
    nfull_roots <- basename(nfull_files)
    nfull_roots <- unlist(lapply(strsplit(nfull_roots, split = "noisemesFull_"), function(X)X[2]))
    if (!is.null(nfull_roots)) {
      nfull_roots <- unlist(strsplit(nfull_roots, ".rttm", fixed = TRUE))
    }

    t_files <- list.files(sadloc, pattern = "tocomboSad_", full.names = TRUE)
    t_files <- t_files[!grepl("diartk_", t_files)]
    t_roots <- basename(t_files)
    t_roots <- unlist(lapply(strsplit(t_roots, split = "tocomboSad_"), function(X)X[2]))
    t_roots <- unlist(strsplit(t_roots, ".rttm", fixed = TRUE))

    o_files <- list.files(sadloc, pattern = "opensmileSad_", full.names = TRUE)
    o_files <- o_files[!grepl("diartk_", o_files)]
    o_roots <- basename(o_files)
    o_roots <- unlist(lapply(strsplit(o_roots, split = "opensmileSad_"), function(X)X[2]))
    o_roots <- unlist(strsplit(o_roots, ".rttm", fixed = TRUE))

    xtab <- table(c(t_roots, n_roots, o_roots))
    xfiles <- names(xtab)[xtab == 3]
  }

  # results object
  res <- data.frame()

  for (i in 1:length(elan_files)) {
    root <- strsplit(basename(elan_files[i]), ".", fixed = TRUE)[[1]][1]

    elan <- read_elan(x = elan_files[i])
    elan <- collapse_tiers(xdata = elan, timecols = c("start", "end"), end_is_dur = FALSE, ...)
    elan <- segment_annotations(xdata = elan, segment_dur = segment_dur * 1000, timecols = c("start", "end"), end_is_dur = FALSE)
    elan$dur <- elan$end - elan$start
    elan <- tapply(elan$dur, INDEX = elan$cat, sum)/1000

    opensmile <- paste0(sadloc, "/opensmileSad_", root, ".rttm")
    opensmile <- read.table(opensmile, header = FALSE)
    opensmile <- collapse_tiers(xdata = opensmile, timecols = c("V4", "V5"), end_is_dur = TRUE)
    opensmile <- segment_annotations(xdata = opensmile, segment_dur = segment_dur, timecols = c("V4", "xend"), end_is_dur = FALSE)
    opensmile <- tapply(opensmile$xdur, INDEX = opensmile$cat, sum)
    opensmile[is.na(opensmile)] <- 0

    noisemes <- paste0(sadloc, "/noisemesSad_", root, ".rttm")
    noisemes <- read.table(noisemes, header = FALSE)
    noisemes <- collapse_tiers(xdata = noisemes, timecols = c("V4", "V5"), end_is_dur = TRUE)
    noisemes <- segment_annotations(xdata = noisemes, segment_dur = segment_dur, timecols = c("V4", "xend"), end_is_dur = FALSE)
    noisemes <- tapply(noisemes$xdur, INDEX = noisemes$cat, sum)
    noisemes[is.na(noisemes)] <- 0

    tocombo <- paste0(sadloc, "/tocomboSad_", root, ".rttm")
    tocombo <- read.table(tocombo, header = FALSE)
    tocombo <- collapse_tiers(xdata = tocombo, timecols = c("V4", "V5"), end_is_dur = TRUE)
    tocombo <- segment_annotations(xdata = tocombo, segment_dur = segment_dur, timecols = c("V4", "xend"), end_is_dur = FALSE)
    tocombo <- tapply(tocombo$xdur, INDEX = tocombo$cat, sum)
    tocombo[is.na(tocombo)] <- 0

    noisemes_noise <- paste0(sadloc, "/noisemesFull_", root, ".rttm")
    noisemes_noise <- read.table(noisemes_noise, header = FALSE)
    noisemes_noise <- noisemes_noise[noisemes_noise$V8 == "noise_ongoing", ]
    if (nrow(noisemes_noise) > 0) {
      noisemes_noise <- collapse_tiers(xdata = noisemes_noise, timecols = c("V4", "V5"), end_is_dur = TRUE)
      noisemes_noise <- segment_annotations(xdata = noisemes_noise, segment_dur = segment_dur, timecols = c("V4", "xend"), end_is_dur = FALSE)
      noisemes_noise <- tapply(noisemes_noise$xdur, INDEX = noisemes_noise$cat, sum)
      noisemes_noise[is.na(noisemes_noise)] <- 0
    } else {
      noisemes_noise <- NULL
    }



    # combine data
    xmax <- max(as.numeric(c(rev(names(tocombo))[1],
                             rev(names(elan))[1],
                             rev(names(opensmile))[1],
                             rev(names(noisemes))[1])))

    tempres <- data.frame(n = 1:xmax)
    tempres$file <- as.character(root)

    tempres$noisemes <- NA
    tempres$opensmile <- NA
    tempres$tocombo <- NA
    tempres$elan <- NA
    tempres$noise <- NA

    for (k in 1:nrow(tempres)) {
      if (!is.null(noisemes_noise)) {
        if (tempres$n[k] %in% names(noisemes_noise)) {
          tempres$noise[k] <- noisemes_noise[as.character(k)]
        }
      }

      if (tempres$n[k] %in% names(noisemes)) {
        tempres$noisemes[k] <- noisemes[as.character(k)]
      }
      if (tempres$n[k] %in% names(elan)) {
        tempres$elan[k] <- elan[as.character(k)]
      }
      if (tempres$n[k] %in% names(opensmile)) {
        tempres$opensmile[k] <- opensmile[as.character(k)]
      }
      if (tempres$n[k] %in% names(tocombo)) {
        tempres$tocombo[k] <- tocombo[as.character(k)]
      }
    }

    res <- rbind(res, tempres)
    rm(tempres, elan, noisemes, noisemes_noise, tocombo, opensmile, root, k, xmax)

  }


  res$tocombo[is.na(res$tocombo)] <- 0
  res$noisemes[is.na(res$noisemes)] <- 0
  res$opensmile[is.na(res$opensmile)] <- 0
  res$noise[is.na(res$noise)] <- 0

  res <- droplevels(na.omit(res))
  rownames(res) <- NULL
  res$opensmile <- round(res$opensmile, 2)
  res$elan <- round(res$elan, 2)

  res$file <- as.factor(res$file)

  res
}
