#' convert .rttm to .eaf (ELAN)
#'
#' A very crude translator from rttm files to ELAN format
#' @param rttmfile path to SAD file
#' @param audiofile path to audio file
#' @param targetloc path to targt location, \code{NULL} by default, see details
#'
#' @details the output file is named as the audio file, except for the file extension, e.g. if the audio file is 34_Ab.wav then the output from this function will be 34_Ab.eaf. By default the location for writing this new file is the same as the audio file. If the audio file is not found, or the folder in which it supposedly resides does not exist, the output will be written to location of the rttm file. You can also specify a folder to where you want to write the output in the \code{targetloc=} argument.
#'
#' Also note that you need to specify an audio file, even if it does not exist (or is not available on your machine at this time). There are two reasons for this: first it is needed for establishing the output file name. Second, the ELAN file needs the location of the audio. When opening the .eaf file created with this function in ELAN and if the audio file does not exist in the location provided, ELAN will ask you to locate it.
#'
#' @return writes a file
#' @export
#' @importFrom utils read.table
#' @importFrom xml2 as_xml_document write_xml
#'

rttm2elan <- function(rttmfile, audiofile, targetloc = NULL) {

  # handle media link
  audiofile <- normalizePath(audiofile, winslash = "/", mustWork = FALSE)
  if (!file.exists(audiofile)) warning(audiofile, " not found. \n.eaf file was created anyway, but you you will be prompted to locate the file when opening ELAN")
  ml <- paste0("file://", audiofile)
  rml <- basename(audiofile)
  rml <- paste0("./", rml)

  # rttm ----------------------------------------------------------------
  rttmfile <- normalizePath(rttmfile, winslash = "/", mustWork = TRUE)
  rttm <- read.table(rttmfile, header = FALSE)

  # file name for output
  # is based on audio file name (regardless of whether it exists)
  outname <- gsub(pattern = ".wav", replacement = ".eaf", x = basename(audiofile), fixed = TRUE)

  # and location for output
  if (is.null(targetloc)) {
    if (dir.exists(dirname(audiofile))) {
      outloc <- dirname(audiofile)
    } else {
      outloc <- dirname(rttmfile)
    }
  } else {
    outloc <- normalizePath(targetloc, winslash = "/", mustWork = TRUE)
  }

  ## time order -------------------------------------------------------------
  time_order <- structure(list())
  cnt <- 1
  i=1
  for (i in 1:nrow(rttm)) {
    for (k in 1:2) {
      if (k == 1) tval <- round(rttm[i, 4] * 1000)
      if (k == 2) tval <- round((rttm[i, 4] + rttm[i, 5]) * 1000)

    temp <- structure(list(), TIME_SLOT_ID = paste0("ts", cnt), TIME_VALUE = tval)
    time_order[[length(time_order) + 1]] <- temp
    cnt <- cnt + 1
    }
  }
  names(time_order) <- rep("TIME_SLOT", nrow(rttm) * 2)

  ## annotations
  tier1 <- structure(list(),
                     DEFAULT_LOCALE = "us",
                     LINGUISTIC_TYPE_REF = "default-lt",
                     TIER_ID = "default")

  cnt <- 1
  i=1
  for (i in 1:nrow(rttm)) {
    t1 <- paste0("ts", cnt)
    t2 <- paste0("ts", cnt + 1)

    temp_anno <- structure(list(ANNOTATION_VALUE = rttm[i, 8]))
    temp_anno <- structure(list(ALIGNABLE_ANNOTATION = structure(list(ANNOTATION_VALUE = temp_anno),
                                                                 ANNOTATION_ID = paste0("a", i),
                                                                 TIME_SLOT_REF1 = t1,
                                                                 TIME_SLOT_REF2 = t2)))
    tier1[[length(tier1) + 1]] <- temp_anno
    cnt <- cnt + 2
  }
  names(tier1) <- rep("ANNOTATION", nrow(rttm))

  # audio locations and meta data...
  media_descriptor <- structure(list(),
                                MEDIA_URL = ml,
                                MIME_TYPE = "audio/x-wav",
                                RELATIVE_MEDIA_URL = rml)
  property1 <- structure(list("urn:nl-mpi-tools-elan-eaf:28eb9227-5451-4c09-8cab-f0a959d09e20"),
                         NAME = "URN")
  property2 <- structure(list("lastUsedAnnotationId"),
                         NAME = 3)


  header <- structure(list(MEDIA_DESCRIPTOR = media_descriptor,
                           PROPERTY = property1,
                           PROPERTY = property2),
                      MEDIA_FILE = "",
                      TIME_UNITS = "milliseconds")

  linguistic_type <- structure(list(), GRAPHIC_REFERENCES = "false", LINGUISTIC_TYPE_ID = "default-lt", TIME_ALIGNABLE = "true")
  locale <- structure(list(), COUNTRY_CODE = "EN", LANGUAGE_CODE = "us")

  ANNOTATION_DOCUMENT <- structure(list(HEADER = header,
                                        TIME_ORDER = time_order,
                                        TIER = tier1,
                                        LINGUISTIC_TYPE = linguistic_type,
                                        LOCALE = locale),
                                   'AUTHOR' = "",
                                   DATE = "2019-09-19T20:17:07+01:00",
                                   FORMAT = "3.0",
                                   VERSION = "3.0",
                                   'xmlns:xsi' = "http://www.w3.org/2001/XMLSchema-instance",
                                   'xsi:noNamespaceSchemaLocation' = "http://www.mpi.nl/tools/elan/EAFv3.0.xsd")

  res <- list(ANNOTATION_DOCUMENT = ANNOTATION_DOCUMENT)
  res <- as_xml_document(res)

  # deal with output location
  out <- paste0(outloc, "/", outname)
  x <- write_xml(res, file = out, options = "format")
}
