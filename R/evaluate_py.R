#' run evaluation via python scripts
#'
#' @param test character, path to test file (must be .rttm)
#' @param reference character, path to reference file (must be .rttm)
#' @param metric character, the metric to be calculated
#' @param task character, the task
#' @param prefix character, the file prefix for the \code{test} file (the default option \code{NULL} tries to guess it from the file name)
#' @param check_python logical, test whether python is working
#' @param set_ident_flag logical, should the '--identification' flag be set for the call to the python script
#' @importFrom reticulate import py_available
#' @return numeric vector
#' @export
#' @examples
#' \dontrun{
#' # get file paths to two SAD files
#' sad1 <- system.file("tocomboSad_spanish.rttm", package = "avutils")
#' sad2 <- system.file("yunitator_english_spanish.rttm", package = "avutils")
#' # create rttm file from ELAN and copy to temp folder
#' tdir <- tempdir()
#' elanfile <- system.file("spanish.eaf", package = "avutils")
#' dest1 <- elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
#' file.exists(dest1)
#' evaluate_py(test = sad1, reference = dest1, metric = "accuracy", task = "detection")
#' evaluate_py(test = sad2, reference = dest1, metric = "accuracy", task = "detection")
#' dest2 <- elan2rttm(x = elanfile, outpath = tdir, use_py = TRUE)
#' file.exists(dest2)
#' evaluate_py(test = sad1, reference = dest2, metric = "accuracy", task = "detection")
#' evaluate_py(test = sad2, reference = dest2, metric = "accuracy", task = "detection")
#' }
#'

# metric = "accuracy" # "diaer", "coverage", "completeness", "homogeneity", "purity", "accuracy", "precision", "recall", "deter", "ider","idea"
# task = "detection" # "detection", "diarization", "identification"
# #
# reference <- "/Volumes/Data/VM2/ooo/DiViMe/data/myfile.rttm"
# test <- "/Volumes/Data/VM2/ooo/DiViMe/data/yunitator_english_myfile.rttm"
# # test <- "/Volumes/Data/VM2/ooo/DiViMe/data/tocomboSad_myfile.rttm"
# task  ="diarization"
# metric="diaer"

evaluate_py <- function(test, reference, metric = "accuracy", task = "detection", prefix = NULL, check_python = FALSE, set_ident_flag = FALSE) {

  # defaults:
  # prefix=NULL; check_python = FALSE; set_ident_flag = FALSE; task = "detection"; metric = "accuracy"

  # non-defaults
  # task = "diarization"; metric = "precision"
  test <- normalizePath(test, winslash = "/")
  reference <- normalizePath(reference, winslash = "/")


  if (check_python) {
    if (py_available()) message("python is installed")

    pyres <- tryCatch(expr = x <- import(module = "pyannote.metrics"),
             error = function(cond) {
               message("pyannote package seems to be absent.")
               message("Here's the original error message:")
               message(cond)
               # Choose a return value in case of error
               return(NULL)
             })
    if (as.character(pyres) == "Module(pyannote.metrics)") message("pyannote seems to work")
    return(invisible(NULL))
  }

  pyannote <- import("pyannote.core")


  # get prefix
  if (is.null(prefix)) {
    if (grepl("diartk_tocomboSad_", x = test, fixed = TRUE)) {
      prefix <- "diartk_tocomboSad"
    }
    if (grepl("diartk_noisemesSad_", x = test, fixed = TRUE)) {
      prefix <- "diartk_noisemesSad"
    }
    if (grepl("diartk_opensmileSad_", x = test, fixed = TRUE)) {
      prefix <- "diartk_opensmileSad"
    }
    # if still null try the others
    if (is.null(prefix)) {
      if (grepl("opensmileSad_", x = test, fixed = TRUE)) {
        prefix <- "opensmileSad"
      }
      if (grepl("noisemesSad_", x = test, fixed = TRUE)) {
        prefix <- "noisemesSad"
      }
      if (grepl("tocomboSad_", x = test, fixed = TRUE)) {
        prefix <- "tocomboSad"
      }
      if (grepl("yunitator_old_", x = test, fixed = TRUE)) {
        prefix <- "yunitator_old"
      }
      if (grepl("yunitator_english_", x = test, fixed = TRUE)) {
        prefix <- "yunitator_english"
      }
    }
  }

  # if still null, return error
  if (is.null(prefix)) {
    stop("could not determine prefix for ", shQuote(basename(test)))
  }


  # get base name of file
  bn <- basename(reference)

  pythonscript <- system.file("computemetrics.py", package = "avutils")
  cmd <- paste(pythonscript,
               "-ref", shQuote(reference),
               "-hyp", shQuote(test),
               "-p", shQuote(prefix),
               "-t", shQuote(task),
               "-m", shQuote(metric)
  )

  if (set_ident_flag) cmd <- paste(cmd, "--identification")

  res <- system2(command = "python", args = cmd, stdout = TRUE)
  res
  outnums <- unlist(strsplit(res[length(res) - 1], " ", fixed = TRUE))
  outnums <- outnums[outnums != ""]
  outnums <- as.numeric(outnums[2 : length(outnums)])

  if (task == "detection") {
    if (metric == "accuracy") {
      names(outnums) <- c("accuracy.pct", "tp", "tn", "fp", "fn")
    }
    if (metric == "precision") {
      names(outnums) <- c("detect.precision.pct", "retrieved", "relevant.retrieved")
    }

  }
  if (task == "identification") {
    if (metric == "ider") {
      names(outnums) <- c("ident.error.rate.pct", "total", "correct", "correct.pct", "false.alarm", "false.alarm.pct", "missed", "missed.pct", "confusion", "confusion.pct")
    }


  }
  if (task == "diarization") {
    if (metric == "diaer") {
      names(outnums) <- c("diar.error.rate.pct", "total", "correct", "correct.pct", "false.alarm", "false.alarm.pct", "missed", "missed.pct", "confusion", "confusion.pct")
    }
  }

  if (!is.null(names(outnums))) {
    return(outnums)
  } else {
    return(res)
  }

}


