
res <- summary_elan(system.file(c("spanish.eaf", "synthetic_speech.eaf"), package = "avutils"))


test_that("result is a list", {
  expect_true(is.list(res))
})
