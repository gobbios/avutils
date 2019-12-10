
elan <- read_elan(system.file("BER_0485_12_07_09123.eaf", package = "avutils"))
# remove dependent tiers
elan <- droplevels(elan[!grepl(pattern = "@", x = elan$tier), ])
rttm <- read_rttm(system.file("BER_0485_12_07_09123.rttm", package = "avutils"))


test_that("elan file has the same info as manual rttm file", {
  expect_equal(elan$tier, rttm$tier)
  expect_equal(elan$start, rttm$start)
})
