
context("unit conversion")

test_that("basic conversion", {
  x <- sunits_rcrd(c(1, 2), c("inches", "cm"))

  convert(x, x * 2.54, from = "inches", to = "cm")
})
