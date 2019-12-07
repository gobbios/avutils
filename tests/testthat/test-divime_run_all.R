# create temp folder
tdir <- normalizePath(tempdir())
tdir <- file.path(tdir, "divimetest")
dir.create(tdir)
# copy audio
file.copy(from = system.file("synthetic_speech.wav", package = "avutils"),
          to = file.path(tdir, "speech.wav"))

audio_loc <- tdir
divime_loc <- "/Volumes/Data/VM2/ooo/DiViMe/"


res <- divime_run_all(audio_loc = audio_loc, divime_loc = divime_loc)
xsum <- sum(unlist(lapply(res, function(X)X$processed)))

test_that("divime_run_all successful", {
  expect_true(length(res) == 15)
  expect_true(xsum == 15)
})
