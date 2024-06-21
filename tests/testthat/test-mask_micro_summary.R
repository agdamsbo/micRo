testthat::test_that("masks correctly", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt, missing = "no") |>
    mask_micro_summary(micro.n = 25)

  hash <- digest::digest(tbl)

  testthat::expect_equal(object = hash,
                         expected = "93b576ae55ef2c0e1fd521de015a8d39")
})


testthat::test_that("masks missings", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    mask_micro_summary()

  hash <- digest::digest(tbl)

  testthat::expect_equal(object = hash,
                         expected = "9c572d48660de3c83dda8f8af4ae24ec")
})


testthat::test_that("masks overall", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    gtsummary::add_overall() |>
    mask_micro_summary(micro.n = 12)

  hash <- digest::digest(tbl)

  testthat::expect_equal(object = hash,
                         expected = "7ff29753912c5cf583d292f81c72e475")
})
