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

# via location setting ---------------------

res1 <- rttm_pairs(location = folder_loc, include_subfolders = FALSE)
res2 <- rttm_pairs(location = folder_loc, include_subfolders = TRUE)


test_that("rttm pairs are found subfolders", {
  expect_true(length(unique(res1$reference_file)) < length(unique(res2$reference_file)))
})

res3 <- rttm_pairs(location = folder_loc, include_subfolders = TRUE, prefix = c("vcm_", "tocomboSad_"))

test_that("selection of multiple prefixes", {
  expect_true(length(unique(res3$prefix)) == 2)
})

# via file path setting ---------------------
referencefiles <- file.path(tdir, c("testfolder/file1.rttm", "testfolder/file2.rttm", "testfolder/somesubfolder/file3.rttm"))
testfiles <- list.files(folder_loc, recursive = TRUE, full.names = TRUE)

res4 <- rttm_pairs(test = testfiles, reference = referencefiles)
res5 <- rttm_pairs(test = testfiles, reference = referencefiles, prefix = c("vcm_", "tocomboSad_"))


test_that("output is the same for file paths and location style", {
  expect_equivalent(res2, res4)
  expect_equivalent(res3, res5)
})






# wrong arguments ---------------------

test_that("message occurs if no arguments are supplied", {
  expect_message(rttm_pairs())
})



