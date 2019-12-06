#' find pairs of rttm files suitable for evaluations
#'
#' @param test character, file paths to reference files, i.e. rttm fiels with prefix
#' @param reference character, file paths to test files, i.e. rttm files without prefix
#' @param location character, path that contains .rttm files
#' @param prefix character, optionally, select only pairs for these prefixes. Default is \code{NULL}, i.e. all prefixes will be returned
#' @param include_subfolders logical, include files in subfolder (default is \code{FALSE})
#' @details The function works in one of two ways. First, you can specify a \code{location=} in which all rttm files will be checked.
#'
#' Second, you supply vectors for \code{test=} and \code{reference=} files.
#'
#' The way via \code{location=} takes precedence, i.e. if you supply all three arguments, only the \code{location=} option is used.
#' @return a data.frame with pairs of files
#' @export
#' @examples
#' # using location
#' location <- dirname(list.files(system.file("", package = "avutils"), full.names = TRUE)[1])
#' rttm_pairs(location = location)
#' rttm_pairs(location = location, prefix = c("tocomboSad_"))
#'
#' # using test and reference
#' reference <- system.file("BER_0485_12_07_09123.rttm", package = "avutils")
#' test <- list.files(system.file("", package = "avutils"), pattern = ".rttm$", full.names = TRUE)
#' rttm_pairs(test = test, reference = reference)
#' rttm_pairs(test = test, reference = reference, prefix = "tocomboSad_")

rttm_pairs <- function(test = NULL, reference = NULL, location = NULL, prefix = NULL, include_subfolders = FALSE) {
  out <- data.frame()

  if (!is.null(location)) {
    X <- get_prefix(location, include_subfolders = include_subfolders)
    # without prefix (= reference files)
    X <- X[X$audio_name %in% X$audio_name[which(X$rttm_prefix == "")], ]
    # match pairs
    if (nrow(X) > 0) {
      unique_files <- unique(X$audio_name[X$rttm_prefix == ""])
      i = unique_files[1]
      for (i in unique_files) {
        xlines <- which(X$audio_name == i & X$rttm_prefix != "")
        ref <- which(X$audio_name == i & X$rttm_prefix == "")
        if (length(xlines) > 0) {
          temp <- cbind(test_file = X$rttm_name[xlines],
                        reference_file = X$rttm_name[ref],
                        prefix = X$rttm_prefix[xlines],
                        test = X$full_name[xlines],
                        reference = X$full_name[ref])
          out <- rbind(out, temp)
        }
      }
    }

    # limit to desired prefixes
    if (nrow(out) > 0) {
      if (!is.null(prefix)) {
        out <- out[out$prefix %in% prefix, ]
        rownames(out) <- NULL
      }
    }
    # convert columns into character
    out[] <- lapply(out, as.character)

    return(out)
  }

  if (!is.null(reference) & !is.null(test)) {
    reference <- unique(normalizePath(reference, winslash = "/", mustWork = FALSE))
    test <- unique(normalizePath(test, winslash = "/", mustWork = FALSE))

    # remove potential duplicates from test files
    for (i in reference) {
      if (i %in% test) {
        test <- test[-c(which(test == i))]
      }
    }

    # basenames
    reference_bn <- basename(reference)
    test_bn <- basename(test)

    i = reference_bn[1]
    for (i in reference_bn) {
      xfiles <- grep(pattern = i, x = test_bn, fixed = TRUE)
      if (length(xfiles) > 0) {
        prefixes <- unlist(strsplit(test_bn[xfiles], split = i, fixed = TRUE))
        temp <- cbind(test_file = test_bn[xfiles],
                      reference_file = i,
                      prefix = prefixes,
                      test = test[xfiles],
                      reference = reference[reference_bn == i])
        out <- rbind(out, temp)
      }
    }

    # limit to desired prefixes
    if (nrow(out) > 0) {
      if (!is.null(prefix)) {
        out <- out[out$prefix %in% prefix, ]
        rownames(out) <- NULL
      }
    }

    # convert columns into character
    out[] <- lapply(out, as.character)

    return(out)
  }

  message("you need to supply either location, or test *and* reference")
}
