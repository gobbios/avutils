# joint testing for extract_audio, audio_info and video_info
library(avutils)

set_binaries(pathtoffmpeg = "~/Documents/utilities/ffmpeg", pathtosox = "~/Documents/utilities/sox")

# download an example video
tdir <- file.path(tempdir(), "audiotests")
dir.create(tdir)

vids <- c(
  mp4 = "https://www.sample-videos.com/video123/mp4/240/big_buck_bunny_240p_1mb.mp4",
  flv = "https://www.sample-videos.com/video123/flv/360/big_buck_bunny_360p_1mb.flv",
  gp3 = "https://www.sample-videos.com/video123/3gp/144/big_buck_bunny_144p_1mb.3gp"
)
vid <- sample(vids, 1)
(ext <- substr(vid, nchar(vid) - 3, nchar(vid)))
vidloc <- file.path(tdir, paste0("vid", ext))
download.file(url = vid, destfile = vidloc)

vidformats <- c("s263", "h263", "h264", "avc1")

vidinfo <- video_info(filein = vidloc)

format_check <- sum(vapply(vidformats, function(X)grepl(pattern = X, x = vidinfo$video_format[1]), FUN.VALUE = FALSE)) > 0

test_that("video is recognized", {
  expect_true(format_check)
  expect_true(vidinfo$duration[1] > 0)
  expect_true(vidinfo$width[1] > 0)
  expect_true(vidinfo$height[1] > 0)
  expect_true(vidinfo$hasaudio[1])
})


# extract audio

res <- extract_audio(videofile = vidloc, progbar = TRUE)

test_that("extract_audio creates audio", {
  expect_true(file.exists(res$targetloc))
})

ai <- audio_info(res$targetloc)
test_that("audio is indeed audio", {
  expect_true(ai$samples[1] > 0)
  expect_true(ai$duration[1] > 0)
})


test_that("audio and video match", {
  expect_equal(ai$duration[1], vidinfo$duration[1], tolerance = 0.05)
})





