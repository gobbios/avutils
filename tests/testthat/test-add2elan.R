
# an elan file:
elan <- system.file("synthetic_speech.eaf", package = "avutils")
# audio:
audio <- system.file("synthetic_speech.wav", package = "avutils")
# some annos
yuni <- system.file("yunitator_old_synthetic_speech.rttm", package = "avutils")
toco <- system.file("tocomboSad_synthetic_speech.rttm", package = "avutils")
opsm <- system.file("opensmileSad_synthetic_speech.rttm", package = "avutils")
nois <- system.file("noisemesSad_synthetic_speech.rttm", package = "avutils")
noisf <- system.file("noisemesFULL_synthetic_speech.rttm", package = "avutils")

# copy files to temp dir
tdir <- file.path(tempdir(), "add2elan_test")
if (!dir.exists(tdir)) dir.create(tdir)
# system2("open", tdir)
file.copy(from = elan, to = file.path(tdir, basename(elan)))
file.copy(from = audio, to = file.path(tdir, basename(audio)))
file.copy(from = yuni, to = file.path(tdir, basename(yuni)))
file.copy(from = opsm, to = file.path(tdir, basename(opsm)))
file.copy(from = toco, to = file.path(tdir, basename(toco)))
file.copy(from = nois, to = file.path(tdir, basename(nois)))
file.copy(from = noisf, to = file.path(tdir, basename(noisf)))


res <- add2elan(elanfile = file.path(tdir, basename(elan)),
                audiofile = file.path(tdir, basename(audio)),
                tocombo = file.path(tdir, basename(toco)),
                opensmile = file.path(tdir, basename(opsm)),
                noisemes = file.path(tdir, basename(nois)),
                noisemesFull = file.path(tdir, basename(noisf)),
                yuni = file.path(tdir, basename(yuni)))

# reread result
xdata <- read_elan(res)

test_that("reread elan file matches input rttm", {
  temp <- xdata[xdata$tier == "noisemes", ]
  temprttm <- read_rttm(nois)
  expect_equal(temp$start, temprttm$start, tol = 0.001)
  temp <- xdata[xdata$tier == "noisemesFull", ]
  temprttm <- read_rttm(noisf)
  expect_equal(temp$start, temprttm$start, tol = 0.001)
  temp <- xdata[xdata$tier == "opensmile", ]
  temprttm <- read_rttm(opsm)
  expect_equal(temp$start, temprttm$start, tol = 0.001)
  temp <- xdata[xdata$tier == "tocombo", ]
  temprttm <- read_rttm(toco)
  expect_equal(temp$start, temprttm$start, tol = 0.001)
  temp <- xdata[xdata$tier == "yunitator", ]
  temprttm <- read_rttm(yuni)
  expect_equal(temp$start, temprttm$start, tol = 0.001)
})
