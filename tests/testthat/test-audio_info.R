
set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg", pathtosox = "~/Documents/utilities/sox")

tdir <- file.path(tempdir(), "audio_info")
dir.create(tdir)
# system2("open", tdir)
# two audio files from the package
audio <- c(system.file(c("synthetic_speech.mp3", "synthetic_speech.wav"), package = "avutils"))
# and two created with the tuneR package
duration <- round(runif(2, 10, 20), 3)
srates <- runif(2, 4000, 48000)
w1 <- sine(freq = 440, duration = duration[1], bit = 32, stereo = TRUE, xunit = "time", samp.rate = srates[1])
w2 <- sine(freq = 880, duration = duration[2], bit = 8, stereo = FALSE, xunit = "time", pcm = TRUE, samp.rate = srates[2])
writeWave(object = w1, filename = file.path(tdir, "file1.wav"))
writeWave(object = w2, filename = file.path(tdir, "file2.wav"))
audio <- c(file.path(tdir, "file1.wav"), file.path(tdir, "file2.wav"), audio)

res1 <- audio_info(filein = audio, use_sox = TRUE)

test_that("sox works with wav files and ignores mp3", {
  expect_true(is.na(res1$duration[3])) # ignores mp3
  expect_equal(res1$duration[1], duration[1], tol = 0.001)
  expect_equal(res1$duration[2], duration[2], tol = 0.001)
  expect_equal(res1$duration[4], 47.74, tol = 0.005)
  expect_equal(res1$samplerate[1], srates[1], tol = 1)
  expect_equal(res1$samplerate[2], srates[2], tol = 1)
  expect_equal(res1$samplerate[4], 44100)
})

res2 <- audio_info(filein = audio, use_sox = FALSE)

test_that("ffmpeg works with wav and mp3", {
  expect_true(!is.na(res2$duration[3]))
  expect_equal(res2$duration[1], duration[1], tol = 0.001)
  expect_equal(res2$duration[2], duration[2], tol = 0.001)
  expect_equal(res2$duration[3], 47.84, tol = 0.001)
  expect_equal(res2$duration[4], 47.74, tol = 0.005)
  expect_equal(res2$samplerate[1], srates[1], tol = 1)
  expect_equal(res2$samplerate[2], srates[2], tol = 1)
  expect_equal(res2$samplerate[3], 32000)
  expect_equal(res2$samplerate[4], 44100)
})

test_that("missing files lead to warnings/errors", {
  expect_warning(audio_info("not/existing"))
  expect_error(audio_info(audio, pathtosox = "", pathtoffmpeg = "", use_sox = TRUE))
  expect_error(audio_info(audio, pathtosox = "", pathtoffmpeg = "", use_sox = FALSE))
})


