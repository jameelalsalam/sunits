
context("basic construction of rcrd-style units")

test_that("user-friendly constructor", {

  # int coerced to dbl by constructor
  expect_equal(sunits_rcrd(1:2, "inches"), sunits_rcrd(1L:2L, "inches"))

  # size/length is the number of elements
  expect_equal(vctrs::vec_size(sunits_rcrd(1:3, "inches")), 3)
  expect_equal(length(sunits_rcrd(1:3, "inches")), 3)
})

test_that("don't print NA units", {

  expect_equal(stringr::str_length(format(sunits_rcrd(1:2, "inches"))), c(8, 8))
  expect_equal(stringr::str_length(format(sunits_rcrd(1:2, NA_character_))), c(1, 1))

})

test_that("coercing dataframe equiv", {
  expect_equal(
    sunits_rcrd(1:2, "inches"),
    as_sunits_rcrd(
      data.frame(value = 1:2,
                 units = rep("inches", 2))))
})
