testthat::test_that("round intervals works", {
  res <- structure(c("0-2", "2-4", "2-4", "4-6", "4-6", "6-8", "6-8",
                     "8-10", "8-10", "10-12"), class = c("glue", "character"))

  testthat::expect_equal(1:10 |> rounded_interval(round=2),
                         res)
})


testthat::test_that("round intervals round down works", {
  res <- structure(c("0-5", "0-5", "0-5", "0-5", "5-10", "5-10", "5-10",
                     "5-10", "5-10", "10-15"), class = c("glue", "character"))

  testthat::expect_equal(1:10 |> rounded_interval(round=5,down = FALSE),
                         res)

})
