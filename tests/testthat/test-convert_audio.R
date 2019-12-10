library(avutils)
set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg", pathtosox = "~/Documents/utilities/sox")

tdir <- file.path(tempdir(), "audio_conversion")
outdir <- file.path(tempdir(), "audio_conversion_out")
dir.create(tdir)
dir.create(outdir)

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
audio <- c(audio, file.path(tdir, "file1.wav"), file.path(tdir, "file2.wav"))


res <- convert_audio(filein = audio[2:4], pathout = outdir)
outinfo <- audio_info(filein = res$outloc)
ininfo <- audio_info(filein = audio[2:4])


test_that("conversion between wav files works", {
  expect_true(all(res$success))
  expect_equal(ininfo$duration, outinfo$duration, tol = 0.001)
})

file.remove(res$outloc)



res <- convert_audio(filein = audio[1], pathout = outdir)
outinfo <- audio_info(filein = res$outloc, use_sox = FALSE)
ininfo <- audio_info(filein = audio[1], use_sox = FALSE)

test_that("conversion from mp3 to wav works", {
  expect_true(all(res$success))
  expect_equal(ininfo$duration, outinfo$duration, tol = 0.1)
})

file.remove(res$outloc)


# error catching
test_that("correct errors/warnings occur", {
  expect_warning(convert_audio(filein = "file/doesnotexist.mp3", pathout = outdir))
  expect_error(convert_audio(filein = audio[2], outformat = list(filetype = "mp3"), pathout = outdir))
  expect_warning(res <- convert_audio(filein = audio[1:2], pathout = outdir, overwrite = TRUE))
  expect_true(sum(res$overwritten) == 0)
  expect_true(sum(res$success) == 0)
  expect_true(sum(res$duplicate) == 2)
})


r1 <- convert_audio(filein = audio[3:4], pathout = NULL, overwrite = FALSE)
r2 <- convert_audio(filein = audio[3:4], pathout = NULL, overwrite = TRUE)
# nothing happened because can't overwrite the source for the conversion (regardless of overwrite-option)


r3 <- convert_audio(filein = audio[3:4], pathout = outdir, overwrite = FALSE)
# works
r4 <- convert_audio(filein = audio[3:4], pathout = outdir, overwrite = TRUE)
# works too

file.remove(r3$outloc)

test_that("overwriting works as expected", {
  # skip("skip_on_cran")
  expect_true(sum(r1$overwritten) == 0)
  expect_true(sum(r1$success) == 0)
  expect_true(sum(r2$overwritten) == 0)
  expect_true(sum(r2$success) == 0)
  expect_true(sum(r3$overwritten) == 0)
  expect_true(sum(r3$success) == 2)
  expect_true(sum(r4$overwritten) == 2)
  expect_true(sum(r4$success) == 2)
})


