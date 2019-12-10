
library(avutils)
# set divime loc
divime_loc <- "/Volumes/Data/VM2/ooo/DiViMe/"
# set sox for splitting audio
set_binaries(pathtosox = "~/Documents/utilities/sox")


audio_loc <- file.path(tempdir(), "split_test")
dir.create(audio_loc)
# system2("open", audio_loc)


file.copy(from = system.file("synthetic_speech.wav", package = "avutils"),
          to = file.path(audio_loc, "synthetic_speech.wav"))
# run module with internally split audio
divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "tocombo", vmshutdown = FALSE,
           splitaudio = 5)
x1 <- read_rttm(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))
# run module in one go
divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "tocombo", vmstart = FALSE, vmshutdown = FALSE,
           overwrite = TRUE)
x2 <- read_rttm(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))

# no formal tests done because the splitting will affect boundaries and hence the performance of the tools
# e.g.
nrow(x1)
nrow(x2)

sum(x1$duration)
sum(x2$duration)

# incidentally, there are two cases in which both runs return very similar results
x1[c(3, 6), ]
x2[c(3, 7), ]
# the example here is fairly extreme (splitting into 5-sec chunks)
# with longer audio and longer chunks, this should be much less severe


# another way of testing might be to take the audio file and add a lot of silence and make the chunks larger, so that effectively, the processed first chunk is the same for both cases

file.remove(file.path(audio_loc, "synthetic_speech.wav"))
file.remove(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))

# original audio is 48 seconds
audio_info(system.file("synthetic_speech.wav", package = "avutils"))

# copy original sound file with silence appended
copy_audio(from = system.file("synthetic_speech.wav", package = "avutils"),
           to = file.path(audio_loc, "synthetic_speech.wav"),
           appendsilence = 30)

# run module with internally split audio ()
divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "tocombo", vmstart = FALSE, vmshutdown = FALSE,
           splitaudio = 50)
x1 <- read_rttm(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))
# run module in one go
divime_sad(audio_loc = audio_loc, divime_loc = divime_loc, module = "tocombo", vmstart = FALSE,
           overwrite = TRUE)
x2 <- read_rttm(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))


test_that("combined results are (nearly) equal", {
  expect_equal(x1, x2, tol = 0.02)
})

# One thing to note here is that in fact, there is no actual merging of rttms done because the second rttm is empty.
# Still, there is a call to 'combine_rttm' involved.

file.remove(file.path(audio_loc, "synthetic_speech.wav"))
file.remove(file.path(audio_loc, "tocomboSad_synthetic_speech.rttm"))

