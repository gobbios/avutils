library(avutils)


tdir <- file.path(tempdir(), "anno_conversion")
dir.create(tdir)
# system2("open", tdir)


elanfile <- system.file("spanish.eaf", package = "avutils")
# file.copy(elanfile, to = file.path(tdir, "spanish1.eaf"))
# file.copy(elanfile, to = file.path(tdir, "spanish2.eaf"))


elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
x1 <- read_rttm(file.path(tdir, "spanish.rttm"))
file.rename(file.path(tdir, "spanish.rttm"), file.path(tdir, "spanish1.rttm"))

elan2rttm(x = elanfile, outpath = tdir, use_py = TRUE)
x2 <- read_rttm(file.path(tdir, "spanish.rttm"))
x2 <- x2[order(x2$start), ]
rownames(x2) <- NULL
file.rename(file.path(tdir, "spanish.rttm"), file.path(tdir, "spanish2.rttm"))

# differences are due to rounding...

test_that("rttm return same start and end columns", {
  expect_equal(round(x1$start, 2), x2$start)
  expect_equal(round(x1$end, 2), x2$end)
})


test_that("error occurs if merge_tiers is set incorrectly", {
  expect_error(elan2rttm(x = elanfile, use_py = TRUE, merge_tiers = list(X = "A")))
  expect_error(elan2rttm(x = elanfile, use_py = FALSE, merge_tiers = list(CHI = c("CHI", "FEM"), MAL = c("FEM"))))
  expect_error(elan2rttm(x = elanfile, use_py = FALSE, merge_tiers = list(CHI = c("CHI", "FEM"), CHI = c("MAL"))))
})

elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE)
x1 <- read_rttm(file.path(tdir, "spanish.rttm"))
x1 <- table(x1$tier)

elan2rttm(x = elanfile, outpath = tdir, use_py = FALSE, merge_tiers = list(FEM = c("FA1", "FA2"), MAL = c("MA1")))
x2 <- read_rttm(file.path(tdir, "spanish.rttm"))
x2 <- table(x2$tier)

test_that("merge_tiers works", {
  expect_true(sum(x1) == sum(x2))
  expect_true(length(names(x1)) > length(names(x2)))
})



# using a non-matching (but existing) audio file as placeholder
rttm2elan(rttmfile = file.path(tdir, "spanish2.rttm"), audiofile = system.file("synthetic_speech.wav", package = "avutils"), targetloc = tdir)

test_that("waring occurs if audio does not exist", {
  expect_warning(rttm2elan(rttmfile = file.path(tdir, "spanish2.rttm"), audiofile = "doesnt/exist/synthetic_speech.wav", targetloc = tdir))
})
