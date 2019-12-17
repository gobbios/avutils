#' run evaluation via python scripts
#'
#' @param test character, path to test file (must be .rttm), typically with prefix
#' @param reference character, path to reference file (must be .rttm), typically without prefix
#' @param metric character, the metric to be calculated
#' @param task character, the task
#' @param prefix character, the file prefix for the \code{test} file (the default option \code{NULL} tries to guess it from the file name)
#' @param check_python logical, test whether python is working
#' @param set_ident_flag logical, should the '--identification' flag be set for the call to the python script
#' @param processed_output logical, should the process output be returned (a data.frame) or the raw textual output from the console (a list). Default is \code{TRUE}.
#' @param progressbar logical, display a progress bar (default is \code{FALSE})
#' @importFrom reticulate import py_available
#' @return a data.frame with one row per pair of test/reference and a varying number of columns (depending on task and metric).
#' @details So far the function only works for a small subset of evaluation task/metric combinations:
#' \itemize{
#'  \item{detection/accuracy}
#'  \item{detection/precision}
#'  \item{detection/recall}
#'  \item{identification/ider}
#'  \item{identification/precision}
#'  \item{identification/recall}
#'  \item{diarization/diaer}
#' }
#'
#' Pairs of test/reference are determined by the \code{\link{rttm_pairs}} function.
#'
#' Also, the function is not very efficient because it has to initialize python for each test/reference pair.
#' @export
#' @examples
#' \dontrun{
#' dataloc <- dirname(system.file("spanish.rttm", package = "avutils"))
#' # list three reference files, one containing a 'typo'
#' reference <- file.path(dataloc, c("BER_0485_12_07_09123.rttm", "spanish.rttm", "filewithtypo.rttm"))
#' # and a whole bunch of potential test files (all rttm in the same location)
#' test <- list.files(dataloc, pattern= ".rttm$", full.names = TRUE)
#'
#' # will produce a warning that there was one file that couldn't be processed...
#' res <- evaluate_py(test = test, reference = reference, metric = "accuracy", task = "detection",
#'                    progressbar = TRUE)
#' res
#' }
#'


evaluate_py <- function(test, reference, metric = "accuracy", task = "detection",
                        prefix = NULL, check_python = FALSE, set_ident_flag = FALSE,
                        processed_output = TRUE, progressbar = FALSE) {
  # metric = "accuracy" # "diaer", "coverage", "completeness", "homogeneity", "purity", "accuracy", "precision", "recall", "deter", "ider","idea"
  # task = "detection" # "detection", "diarization", "identification"

  # defaults:
  # prefix=NULL; check_python=F; set_ident_flag=F; task = "detection"; metric = "accuracy"
  # processed_output=T; progressbar=F

  # non-defaults
  # task = "diarization"; metric = "precision"; progressbar=T
  # reference = file.path("~/Dropbox/work/avutils/inst", c("BER_0485_12_07_09123.rttm", "spanish.rttm", "bam.txt", "dup.txt"))
  # test = list.files("inst/", pattern= ".rttm$", full.names = TRUE)

  # run python check
  if (check_python) {
    if (py_available()) message("python is installed")

    pyres <- tryCatch(expr = x <- import(module = "pyannote.metrics"),
                      error = function(cond) {
                        message("pyannote package seems to be absent.")
                        message("Here's the original error message:")
                        message(cond)
                        return(NULL) # choose a return value in case of error
                      })
    if (as.character(pyres) == "Module(pyannote.metrics)") message("pyannote seems to work")
    return(invisible(NULL))
  }

  # clean paths
  test <- normalizePath(test, winslash = "/", mustWork = FALSE)
  reference <- normalizePath(reference, winslash = "/", mustWork = FALSE)


  # check for missing files
  reftest <- !vapply(reference, file.exists, FUN.VALUE = FALSE)
  testtest <- !vapply(test, file.exists, FUN.VALUE = FALSE)

  if (sum(testtest) > 0) {
    missingfiles <- paste(shQuote(names(testtest[testtest])), collapse = "\n")
    warning("the following test files were not found:\n", missingfiles, call. = FALSE)
  }
  if (sum(reftest) > 0) {
    missingfiles <- paste(shQuote(names(reftest[reftest])), collapse = "\n")
    warning("the following reference files were not found:\n", missingfiles, call. = FALSE)
  }

  pyannote <- import("pyannote.core")
  pythonscript <- system.file("computemetrics.py", package = "avutils")


  # find pairs of relevant pairs given the 'reference' files
  xpairs <- rttm_pairs(test = test, reference = reference, prefix = prefix)
  xpairs$prefix <- substr(xpairs$prefix, 1, nchar(xpairs$prefix) - 1)


  # quick check for wrong/unknown prefixes
  allprefixes <- c("diartk_tocomboSad", "diartk_noisemesSad", "diartk_opensmileSad",
                   "opensmileSad", "noisemesSad", "tocomboSad", "yunitator_old", "yunitator_english")

  foundprefixes <- unique(xpairs$prefix)
  preftest <- !vapply(foundprefixes, function(X)X %in% allprefixes, FUN.VALUE = FALSE)
  if (sum(preftest) > 0) {
    missingfiles <- paste(shQuote(names(preftest[preftest])), collapse = "\n")
    warning("the following prefixes in your test files might(!) cause trouble:\n", missingfiles, call. = FALSE)
  }


  # prepare column names for output depending on task and metric
  if (task == "detection") {
    if (metric == "accuracy") {
      outnames <- c("accuracy_pct", "true_pos", "true_neg", "false_pos", "false_neg")
    }
    if (metric == "precision") {
      outnames <- c("detect_precision_pct", "retrieved", "relevant_retrieved")
    }
    if (metric == "recall") {
      outnames <- c("detect_recall_pct", "retrieved", "relevant_retrieved")
    }
  }

  if (task == "identification") {
    if (metric == "ider") {
      outnames <- c("ident_error_rate_pct", "total", "correct", "correct_pct",
                    "false_alarm", "false_alarm_pct", "missed", "missed_pct",
                    "confusion", "confusion_pct")
    }
    if (metric == "precision") {
      outnames <- c("detect_precision_pct", "retrieved", "relevant_retrieved")
    }
    if (metric == "recall") {
      outnames <- c("detect_recall_pct", "retrieved", "relevant_retrieved")
    }
  }

  if (task == "diarization") {
    if (metric == "diaer") {
      outnames <- c("diar_error_rate_pct", "total", "correct", "correct_pct",
                    "false_alarm", "false_alarm_pct", "missed", "missed_pct",
                    "confusion", "confusion_pct")
    }
  }


  # prepare output objects
  outdf <- matrix(ncol = 5 + length(outnames), nrow = nrow(xpairs))
  colnames(outdf) <- c("test", "reference", "task", "metric", "prefix", outnames)
  rawoutput <- list()

  if (progressbar) pb <- txtProgressBar(min = 0, max = nrow(xpairs), style = 3, char = ".")

  # loop through all file pairs
  i = 1
  for (i in 1:nrow(xpairs)) {
    # create shell command
    cmd <- paste(pythonscript,
                 "-ref", shQuote(xpairs$reference[i]),
                 "-hyp", shQuote(xpairs$test[i]),
                 "-p", shQuote(xpairs$prefix[i]),
                 "-t", shQuote(task),
                 "-m", shQuote(metric)
    )

    if (set_ident_flag) cmd <- paste(cmd, "--identification")

    res <- system2(command = "python", args = cmd, stdout = TRUE)
    if (progressbar) setTxtProgressBar(pb, i)
    rawoutput[[i]] <- res
    outnums <- unlist(strsplit(res[length(res) - 1], " ", fixed = TRUE))
    outnums <- outnums[outnums != ""]
    outnums <- as.numeric(outnums[2 : length(outnums)])

    # prepare final output
    outdf[i, "test"] <- xpairs$test_file[i]
    outdf[i, "reference"] <- xpairs$reference_file[i]
    outdf[i, "task"] <- task
    outdf[i, "metric"] <- metric
    outdf[i, "prefix"] <- xpairs$prefix[i]
    outdf[i, 6 : ncol(outdf)] <- outnums
  }

  # clean output
  outdf <- data.frame(outdf, stringsAsFactors = FALSE)
  outdf[, outnames] <- apply(outdf[, outnames], 2, as.numeric)

  if (progressbar) close(pb)

  if (processed_output) {
    return(outdf)
  } else {
    return(rawoutput)
  }

}

