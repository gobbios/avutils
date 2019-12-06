library(avutils)
set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg",
             pathtosox = "~/Documents/utilities/sox")

tdir <- tempdir()
# system2("open", tdir)

if (!dir.exists(file.path(tdir, "audio"))) {
  dir.create(file.path(tdir, "audio"))
}

# copy audio from package to tempfolder
targetloc <- file.path(tdir, "audio/synthetic_speech.wav")
file.copy(from = system.file("synthetic_speech.wav", package = "avutils"), to = targetloc)

# duration of original file
fullfile <- audio_info(targetloc)$duration[1]
# split file
res1 <- split_audio(filein = targetloc, split = 10, pathout = tdir)
# calculate duration of split files
splitsum1 <- sum(audio_info(list.files(tdir, pattern = ".wav", full.names = TRUE))$duration)

test_that("splitting audio works with regular intervals", {
  expect_equal(splitsum1, fullfile, tolerance = 0.002)
})

file.remove(res1)

# splitting irregularly (according to data from rttm files) ---------------

# simulate rttm
rttm <- data.frame(v1 = 1:50, v2 = NA, v3 = NA,
                   start = round(sort(runif(50, 0, fullfile - 1)), 3),
                   dur = round(runif(50, 0, 20), 3))
# remove fragments that go beyond file duration
rttm <- rttm[rttm$start + rttm$dur < floor(fullfile), ]
xsum <- sum(rttm$dur)
# split according to this rttm
res2 <- split_audio(targetloc, split = rttm, pathout = NULL)

audioinfo <- audio_info(list.files(dirname(targetloc), pattern = ".wav", full.names = TRUE))
# remove original file from sum calculation
audioinfo <- audioinfo[basename(as.character(audioinfo$filename)) != "synthetic_speech.wav", ]
splitsum2 <- sum(audioinfo$duration)

test_that("splitting audio works with irregular intervals", {
  expect_equal(splitsum2, xsum, tolerance = 0.002)
})

file.remove(res2)

test_that("error if audio or sox does not exist", {
  expect_error(split_audio(filein = "whichdoesnotexist.wave", split = 3))
  expect_error(split_audio(filein = targetloc, split = 3, pathtosox = "doesnotexist"))
})




# duration <- round(runif(1, 10, 20), 3)
# w1 <- sine(freq = 440, duration = duration, bit = 32, stereo = TRUE, xunit = "time")
# dir.create(file.path(tdir, "audio"))
# writeWave(w1, filename = file.path(tdir, "audio/file1.wav"))
# tdir
# fullfile <- audio_info(file.path(tdir, "audio/file1.wav"))$duration[1]
# res <- split_audio(file.path(tdir, "audio/file1.wav"), split = 3, pathout = tdir)
# splitsum1 <- sum(audio_info(list.files(tdir, pattern = ".wav", full.names = TRUE))$duration)
#
# test_that("splitting audio works with regular intervals", {
#   expect_equal(duration, fullfile, tolerance = 0.002)
#   expect_equal(duration, splitsum1, tolerance = 0.002)
# })
#
#
# file.remove(res)
#
# # simulate rttm
# x <- data.frame(v1 = 1:50, v2 = NA, v3 = NA, start = round(sort(runif(50, 0, duration - 2.1)), 3), dur = round(runif(50, 0, 2), 3))
# x <- x[sort(sample(1:50, sample(10:50, 1))), ]
# xsum <- sum(x$dur)
# res <- split_audio(file.path(tdir, "audio/file1.wav"), split = x, pathout = tdir)
# splitsum2 <- sum(audio_info(list.files(tdir, pattern = ".wav", full.names = TRUE))$duration)
# file.remove(res)
# xsum - splitsum2
# test_that("splitting audio works with irregular intervals", {
#   expect_equal(xsum, splitsum2, tolerance = 0.002)
# })
#
#
