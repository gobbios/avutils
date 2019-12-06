# create a temporary folder/file structure

tdir <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
# system2("open", tdir)

if (!dir.exists(file.path(tdir, "testfolder"))) dir.create(file.path(tdir, "testfolder"))
if (!dir.exists(file.path(tdir, "testfolder/somesubfolder"))) dir.create(file.path(tdir, "testfolder/somesubfolder"))

file.create(file.path(tdir, "testfolder/file1.rttm"))
file.create(file.path(tdir, "testfolder/file2.rttm"))
file.create(file.path(tdir, "testfolder/somesubfolder/file3.rttm"))

file.create(file.path(tdir, "testfolder/file2.wav"))
file.create(file.path(tdir, "testfolder/somesubfolder/file3.wav"))

file.create(file.path(tdir, "testfolder/tocomboSad_file1.rttm"))
file.create(file.path(tdir, "testfolder/tocomboSad_file2.rttm"))
file.create(file.path(tdir, "testfolder/somesubfolder/tocomboSad_file3.rttm"))

file.create(file.path(tdir, "testfolder/vcm_file1.rttm"))
file.create(file.path(tdir, "testfolder/vcm_file2.rttm"))
file.create(file.path(tdir, "testfolder/somesubfolder/vcm_file3.rttm"))

file.create(file.path(tdir, "testfolder/noisemesFull_file1.rttm"))
file.create(file.path(tdir, "testfolder/noisemesFull_file2.rttm"))
file.create(file.path(tdir, "testfolder/somesubfolder/noisemesFull_file3.rttm"))

file.create(file.path(tdir, "testfolder/yunitator_old_file1.rttm"))
file.create(file.path(tdir, "testfolder/yunitator_old_file2.rttm"))
file.create(file.path(tdir, "testfolder/somesubfolder/yunitator_old_file3.rttm"))

folder_loc <- file.path(tdir, "testfolder")

res1 <- get_prefix(folder_loc, include_subfolders = FALSE)
res2 <- get_prefix(folder_loc, include_subfolders = TRUE)

test_that("audio is matched", {
  expect_true(sum(res1$audio_exists) > 0)
  expect_true(sum(res2$audio_exists) > sum(res1$audio_exists))
})

test_that("wrong location is recognized", {
  expect_error(get_prefix("~/Some/very/unlikely/folder/location"))
})




