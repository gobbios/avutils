#' evaluate speaker roles
#'
#' @param test character, path to file with annotations in DiViMe (.rttm) format
#' @param reference character, path to reference file with annotations in DiViMe (.rttm) format
#' @param resolution numeric, the time increment in seconds, by default 1
#' @param duration numeric, optional info about the duration of the audio. At its default \code{NULL}, the end of the last annotation is taken as the duration
#' @param tiers a named list that controls which labels/speakers from reference and test annotation are used. The name of each element needs to correspond to the speakers/'tiers' in the \bold{test} file, e.g. \code{CHI}, \code{FEM} and \code{MAL}. The content of each element then lists the corresponding speakers/tiers of the reference. For example, the entry \code{FEM = c("FA1", "FA2")} maps the \code{FEM} speaker in the test file to the \code{FA1} and \code{FA2} tiers in the reference file.
#' @param test_ignore,ref_ignore character, the annotation values to be ignored. At its default \code{NULL}, nothing is ignored.
#' @param allspeech logical, should the speech detection be taken from \emph{all} tiers, i.e. even those that were ignored by setting \code{tiers=}. Default is \code{TRUE}.
#' @param summarize logical (default is \code{TRUE}), should summary metrics be returned or the detailed frame-by-frame results
#'
#' @return a data.frame (for \code{summarize = TRUE}) or matrix (for \code{summarize = FALSE})
#' @export
#'
#' @examples
#' reference <- read_elan(system.file("spanish.eaf", package = "avutils"))
#' test <- read_rttm(system.file("yunitator_old_spanish.rttm", package = "avutils"))
#' evaluate_roles(reference = reference, test = test, duration = 180, resolution = 10)
#' # use for pure speech detection
#' # not quite working yet...
#' # test <- read_rttm(system.file("tocomboSad_spanish.rttm", package = "avutils"))
#' # evaluate_roles(reference = reference, test = test, duration = 180, resolution = 10)

evaluate_roles <- function(test, reference, resolution = 1, duration = NULL, summarize = TRUE,
                           tiers = list(CHI = c("CHI", paste0("UC", 1:9)),
                                        FEM = paste0("FA", 1:9),
                                        MAL = paste0("MA", 1:9)),
                           test_ignore = NULL,
                           ref_ignore = NULL,
                           allspeech = TRUE) {
  # defaults
  # resolution = 1; duration = NULL; test_ignore = NULL; ref_ignore = NULL;
  # allspeech = TRUE; summarize = TRUE
  # tiers = list(CHI = c("CHI", paste0("UC", 1:9)), FEM = paste0("FA", 1:9), MAL = paste0("MA", 1:9))

  # resolution = 10

  # extract duration if not supplied
  if (is.null(duration)) {
    duration <- max(c(reference$end, test$end))
  }

  # remove unwanted annotation values
  if (!is.null(ref_ignore)) {
    xn <- colnames(reference)[which(colnames(reference) %in% c("content", "annotation", "annotation_value"))]
    if (length(xn) == 1) {
      reference <- reference[!reference[, xn] %in% ref_ignore, ]
    }
  }
  if (!is.null(test_ignore)) {
    xn <- colnames(test)[which(colnames(test) %in% c("content", "annotation", "annotation_value"))]
    if (length(xn) == 1) {
      test <- test[!test[, xn] %in% test_ignore, ]
    }
  }

  # keep copies without unwanted annos but with all tiers
  ref_copy <- reference
  test_copy <- test

  # rename tiers in reference file
  reference$tier <- as.character(reference$tier)
  reference <- reference[reference$tier %in% unlist(tiers), ]
  if (!is.null(tiers)) {
    for (i in 1:length(tiers)) {
      newname <- names(tiers)[[i]]
      reference$tier[reference$tier %in% tiers[[i]]] <- newname
    }
  }

  # generate sampling points (depending on whether duration is supplied) and remove first point at 0
  samplepoints <- seq(from = 0, to = duration, by = resolution)[-1]
  # and remove last if it coincides with duration
  if (samplepoints[length(samplepoints)] == duration) {
    samplepoints <- samplepoints[-length(samplepoints)]
  }

  # intermediate results
  # matrix with one column for each selected tier (and one column for sample time point)

  tempfoo <- function(xdata, xtiers, xpoints) {
    res <- matrix(ncol = length(xtiers), nrow = length(xpoints), 0)
    colnames(res) <- xtiers
    i = 2
    for (i in 1:length(xtiers)) {
      temp <- as.matrix(xdata[xdata$tier == xtiers[i], c("start", "end"), drop = FALSE])
      if (nrow(temp) > 0) {
        foo <- function(X) {
          X >= temp[, 1, drop = FALSE] & X <= temp[, 2, drop = FALSE]
        }
        if (nrow(temp) == 1) {
          res[, i] <- as.numeric(sapply(xpoints, foo))
        } else {
          res[, i] <- colSums(sapply(xpoints, foo))
        }
      }
    }
    res
  }

  res_ref <- tempfoo(reference, names(tiers), samplepoints)
  res_test <- tempfoo(test, names(tiers), samplepoints)
  res_ref2 <- tempfoo(ref_copy, unique(as.character(ref_copy$tier)), samplepoints)

  if (allspeech) {
    all_speech_samples <- rowSums(res_ref2) >= 1
  } else {
    all_speech_samples <- rowSums(res_ref) >= 1
  }

  # create overall detailed results object as dataframe
  detailed_res <- data.frame(res_test, res_ref)
  colnames(detailed_res) <- c(paste("test", colnames(res_test), sep = "_"),
                              paste("ref", colnames(res_ref), sep = "_"))
  detailed_res <- data.frame(slice = samplepoints, detailed_res)


  # FA (false alarm) is the number of frames during which there is no talk according to the human annotator but during which X found some talk
  detailed_res$fp <- rowSums(res_ref) == 0 & rowSums(res_test) >= 1
  # M (miss) is the number of frames during which there is talk according to the human annotator but during which X found no talk
  detailed_res$fn <- rowSums(res_ref) >= 1 & rowSums(res_test) == 0
  # true positives and true negatives
  detailed_res$tp <- rowSums(res_ref) >= 1 & rowSums(res_test) >= 1
  detailed_res$tn <- rowSums(res_ref) == 0 & rowSums(res_test) == 0

  # C (confusion) is the number of frames correctly classified by X as containing talk, but whose voice type has not been correctly identified (when X recognizes female adult speech where there is male adult speech for instance)
  detailed_res$ref_speech <- rowSums(res_ref) >= 1 # frames correctly classified by X as containing talk
  detailed_res$test_correct_speech <- rowSums(res_test) >= 1 & rowSums(res_ref) >= 1 # frames correctly classified by X as containing talk
  detailed_res$confused_role <- NA
  speech_frames <- which(detailed_res$test_correct_speech)
  res_test_sel <- res_test[speech_frames, , drop = FALSE]
  res_ref_sel <- res_ref[speech_frames, , drop = FALSE]

  correct_match <- logical(nrow(res_test_sel))
  i=1
  for (i in 1:length(tiers)) {
    # select test speaker
    tempindex <- which(res_test_sel[, names(tiers)[[i]]] == 1)
    if (length(tempindex) > 0) {
      correct_match[tempindex] <- rowSums(res_ref_sel[tempindex, names(tiers)[[i]], drop = FALSE]) >= 1
    }
  }
  detailed_res$confused_role[speech_frames] <- !correct_match


  # overlap categories
  # applies only to manual annotations (DiViMe doesn't classify overlap)
  # any overlap
  detailed_res$overlap <- rowSums(res_ref) > 1

  if ("CHI" %in% colnames(res_ref)) {
    # overlap with CHI
    detailed_res$overlap_chi <- (res_ref[, "CHI"] == 1) & (rowSums(res_ref[, colnames(res_ref) != "CHI"]) >= 1)
    # pure CHI, FEM and MAL (without considering overlap) from reference
    detailed_res$pure_chi <- res_ref[, "CHI"] == 1 & !detailed_res$overlap
    detailed_res$pure_fem <- res_ref[, "FEM"] == 1 & !detailed_res$overlap
    detailed_res$pure_mal <- res_ref[, "MAL"] == 1 & !detailed_res$overlap
  } else {
    detailed_res$overlap_chi <- NA
    detailed_res$pure_chi <- NA
    detailed_res$pure_fem <- NA
    detailed_res$pure_mal <- NA
  }


  # create summary results
  if (summarize) {
    fn1 <- attributes(test)$filename
    fn2 <- attributes(reference)$filename
    Tval <- sum(detailed_res$ref_speech)
    FA <- sum(detailed_res$fp)
    M <- sum(detailed_res$fn)
    if (is.nan(mean(detailed_res$confused_role, na.rm = TRUE))) {
      Cval <- NA
      DER <- NA
    } else {
      Cval <- sum(detailed_res$confused_role, na.rm = TRUE)
      DER <- (FA + M + Cval) / Tval
    }

    # precision and recall
    # In both precision and recall, the numerator is the intersection between a LENA® tag and a human tag (e.g., the number of frames that LENA® classified as CHN and the annotator classified as Key child). The denominator differs: To calculate precision, we divide that number by the total number of frames attributed to a category by LENA®, whereas for recall, we divide by the total number of frames attributed to a category by the human annotator.
    # new table for speaker aggregates (in case there is more than one FA or more than one MA)
    if ("CHI" %in% colnames(res_ref)) {
      xtab <- cbind(refr_chi = res_ref[, "CHI"] >= 1,
                    test_chi = res_test[, "CHI"] >= 1,
                    refr_fem = res_ref[, "FEM"] >= 1,
                    test_fem = res_test[, "FEM"] >= 1,
                    refr_mal = res_ref[, "MAL"] >= 1,
                    test_mal = res_test[, "MAL"] >= 1
      )
      # summaries for three roles
      chi_num <- sum(xtab[, "refr_chi"] + xtab[, "test_chi"] == 2)
      precis_chi <- chi_num / sum(xtab[, "test_chi"])
      recall_chi <- chi_num / sum(xtab[, "refr_chi"])
      fem_num <- sum(xtab[, "refr_fem"] + xtab[, "test_fem"] == 2)
      precis_fem <- fem_num / sum(xtab[, "test_fem"])
      recall_fem <- fem_num / sum(xtab[, "refr_fem"])
      mal_num <- sum(xtab[, "refr_mal"] + xtab[, "test_mal"] == 2)
      precis_mal <- mal_num / sum(xtab[, "test_mal"])
      recall_mal <- mal_num / sum(xtab[, "refr_mal"])
    }





    # speech detection: false positives etc
    FP <- sum(detailed_res$fp) / length(samplepoints)

    sum_res <- data.frame(test = ifelse(is.null(fn1), NA, fn1),
                          reference = ifelse(is.null(fn2), NA, fn2),
                          speech_frames = sum(detailed_res$ref_speech) / length(samplepoints),
                          false_alarm = FA / Tval,
                          missed = M / Tval,
                          confusion = Cval / Tval,
                          detect_error = DER,
                          FP = sum(detailed_res$fp) / length(samplepoints),
                          FN = sum(detailed_res$fn) / length(samplepoints),
                          TP = sum(detailed_res$tp) / length(samplepoints),
                          TN = sum(detailed_res$tn) / length(samplepoints),
                          overlapped = sum(detailed_res$overlap) / length(samplepoints)
    )

    if ("CHI" %in% colnames(res_ref)) {
      sum_res <- data.frame(sum_res,
      precis_chi,
      precis_fem,
      precis_mal,
      recall_chi,
      recall_fem,
      recall_mal,
      pure_chi = sum(detailed_res$pure_chi) / length(samplepoints),
      pure_fem = sum(detailed_res$pure_fem) / length(samplepoints),
      pure_mal = sum(detailed_res$pure_mal) / length(samplepoints)
      )
    }

    return(sum_res)

  } else {
    return(detailed_res)
  }

}
