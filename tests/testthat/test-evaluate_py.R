
dataloc <- dirname(system.file("spanish.rttm", package = "avutils"))

# list two reference files
reference <- file.path(dataloc, c("BER_0485_12_07_09123.rttm", "spanish.rttm"))
# and a bunch of potential test files
test <- list.files(dataloc, pattern= "[0485]", full.names = TRUE)

# typo files
referencetypo <- c(reference, file.path(dataloc, c("filewithtypo.rttm")))
testtypo <- c(reference, file.path(dataloc, c("filewithtypo2.rttm")))

test_that("missing file produces warning", {
  expect_warning(evaluate_py(test = test, reference = c(reference, referencetypo), metric = "accuracy", task = "detection"))
  expect_warning(evaluate_py(test = c(test, testtypo), reference = reference, metric = "accuracy", task = "detection"))
})


# and a bunch of potential test files
test <- list.files(dataloc, pattern= "[0485]", full.names = TRUE)

test_that("prefixes properly selected", {
  res <- evaluate_py(test = test, reference = reference, metric = "accuracy", task = "detection", prefix = "tocomboSad_", progressbar = TRUE)
  x1 <- length(unique(res$prefix))
  x2 <- names(table(res$prefix))[1]
  expect_true(x1 == 1)
  expect_true(x2 == "tocomboSad")
})

test_that("raw output is a list", {
  res <- evaluate_py(test = test, reference = reference, metric = "accuracy", task = "detection", prefix = "tocomboSad_", processed_output = FALSE)
  expect_true(is.list(res))
})

test_that("check_python works and stops before actual evaluation", {
  res <- evaluate_py(test = test, reference = reference, metric = "accuracy", task = "detection", check_python = TRUE)
  expect_true(is.null(res))
})


test_that("all tasks/metrics 'work'", {
  res <- evaluate_py(test = test, reference = reference, metric = "precision", task = "detection", prefix = "tocomboSad_")
  expect_true(inherits(res, "data.frame"))
  res <- evaluate_py(test = test, reference = reference, metric = "ider", task = "identification", prefix = "tocomboSad_")
  expect_true(inherits(res, "data.frame"))
  res <- evaluate_py(test = test, reference = reference, metric = "diaer", task = "diarization", prefix = "tocomboSad_")
  expect_true(inherits(res, "data.frame"))
})


# old examples
# # get file paths to two SAD files
# sad1 <- system.file("tocomboSad_spanish.rttm", package = "avutils")
# sad2 <- system.file("yunitator_english_spanish.rttm", package = "avutils")
# # create rttm file from ELAN and copy to temp folder
# tdir <- tempdir()
# elanfile <- system.file("spanish.eaf", package = "avutils")
# dest1 <- elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
# file.exists(dest1)
# evaluate_py(test = sad1, reference = dest1, metric = "accuracy", task = "detection")
# evaluate_py(test = sad2, reference = dest1, metric = "accuracy", task = "detection")
# dest2 <- elan2rttm(x = elanfile, outpath = tdir, use_py = TRUE)
# file.exists(dest2)
# evaluate_py(test = sad1, reference = dest2, metric = "accuracy", task = "detection")
# evaluate_py(test = sad2, reference = dest2, metric = "accuracy", task = "detection")






# will produce a warning that there was one file that couldn't be processed...

