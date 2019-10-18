library(avutils)
set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg",
             pathtosox = "~/Documents/utilities/sox")
tdir <- tempdir()
duration <- round(runif(1, 10, 20), 3)
w1 <- sine(freq = 440, duration = duration, bit = 32, stereo = TRUE, xunit = "time")
dir.create(file.path(tdir, "audio"))
writeWave(w1, filename = file.path(tdir, "audio/file1.wav"))
tdir
fullfile <- audio_info(file.path(tdir, "audio/file1.wav"))$duration[1]
res <- split_audio(file.path(tdir, "audio/file1.wav"), split = 3, pathout = tdir)
splitsum1 <- sum(audio_info(list.files(tdir, pattern = ".wav", full.names = TRUE))$duration)

test_that("splitting audio works with regular intervals", {
  expect_equal(duration, fullfile, tolerance = 0.002)
  expect_equal(duration, splitsum1, tolerance = 0.002)
})


file.remove(res)

# simulate rttm
x <- data.frame(v1 = 1:50, v2 = NA, v3 = NA, start = round(sort(runif(50, 0, duration - 2.1)), 3), dur = round(runif(50, 0, 2), 3))
x <- x[sort(sample(1:50, sample(10:50, 1))), ]
xsum <- sum(x$dur)
res <- split_audio(file.path(tdir, "audio/file1.wav"), split = x, pathout = tdir)
splitsum2 <- sum(audio_info(list.files(tdir, pattern = ".wav", full.names = TRUE))$duration)
file.remove(res)
xsum - splitsum2
test_that("splitting audio works with irregular intervals", {
  expect_equal(xsum, splitsum2, tolerance = 0.002)
})


