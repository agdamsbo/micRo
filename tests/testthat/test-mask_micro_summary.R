testthat::test_that("masks correctly", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt, missing = "no") |>
    mask_micro_summary(micro.n = 25)

  hash <- digest::digest(tbl,algo = "sha1")

  testthat::expect_equal(object = hash,
                         expected = "9d798df53d3cc4d445a56ead0d9c026b0c4689c1")
})


testthat::test_that("masks missings", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    mask_micro_summary()

  hash <- digest::digest(tbl,algo = "sha1")

  testthat::expect_equal(object = hash,
                         expected = "0b275e8e6720968728298421a6f17406eeea7532")
})


testthat::test_that("masks overall", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    gtsummary::add_overall() |>
    mask_micro_summary(micro.n = 12)

  hash <- digest::digest(tbl,algo = "sha1")

  testthat::expect_equal(object = hash,
                         expected = "e49121eb7b56a84f3dc3799781a75740dcbd4488")
})
