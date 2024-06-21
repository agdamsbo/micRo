testthat::test_that("masks correctly", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt, missing = "no") |>
    mask_micro_summary(micro.n = 25) |>
    purrr::pluck("table_body")

  testthat::expect_equal(
    tbl,
    structure(list(
      variable = c(
        "age", "marker", "stage", "stage",
        "stage", "stage", "stage", "grade", "grade", "grade", "grade",
        "response", "death", "ttdeath"
      ), var_type = c(
        "continuous", "continuous",
        "categorical", "categorical", "categorical", "categorical", "categorical",
        "categorical", "categorical", "categorical", "categorical", "dichotomous",
        "dichotomous", "continuous"
      ), var_label = c(
        "Age", "Marker Level (ng/mL)",
        "T Stage", "T Stage", "T Stage", "T Stage", "T Stage", "Grade",
        "Grade", "Grade", "Grade", "Tumor Response", "Patient Died",
        "Months to Death/Censor"
      ), row_type = c(
        "label", "label", "label",
        "level", "level", "level", "level", "label", "level", "level",
        "level", "label", "label", "label"
      ), label = c(
        "Age", "Marker Level (ng/mL)",
        "T Stage", "T1", "T2", "T3", "T4", "Grade", "I", "II", "III",
        "Tumor Response", "Patient Died", "Months to Death/Censor"
      ),
      stat_1 = c(
        "46 (37, 59)", "0.84 (0.24, 1.57)", NA, "28 (29%)",
        "25 (26%)", "<25 (<25.5%)", "<25 (<25.5%)", NA, "35 (36%)",
        "32 (33%)", "31 (32%)", "28 (29%)", "52 (53%)", "23.5 (17.4, 24.0)"
      ), stat_2 = c(
        "48 (39, 56)", "0.52 (0.19, 1.20)", NA, "25 (25%)",
        "29 (28%)", "<25 (<24.5%)", "<50 (<49%)", NA, "33 (32%)",
        "36 (35%)", "33 (32%)", "33 (34%)", "60 (59%)", "21.2 (14.6, 24.0)"
      )
    ), row.names = c(NA, -14L), class = c("tbl_df", "tbl", "data.frame"))
  )
})


testthat::test_that("masks missings", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    mask_micro_summary() |>
    purrr::pluck("table_body")

  testthat::expect_equal(
    tbl |>
      dplyr::filter(variable == "age", row_type == "missing") |>
      dplyr::select(tidyselect::starts_with("stat")),
    structure(list(stat_1 = "<10 (<10.2%)", stat_2 = "<5 (<4.9%)"), row.names = c(
      NA,
      -1L
    ), class = c("tbl_df", "tbl", "data.frame"))
  )
})


testthat::test_that("masks overall", {
  tbl <-
    gtsummary::trial |>
    gtsummary::tbl_summary(by = trt) |>
    gtsummary::add_overall() |>
    mask_micro_summary(micro.n = 12) |>
    purrr::pluck("table_body")

  testthat::expect_equal(
    tbl |>
      dplyr::select(stat_0) |> dput(),
    structure(list(stat_0 = c(
      "47 (38, 57)", "<12 (<6%)", "0.64 (0.22, 1.39)",
      "<12 (<6%)", NA, "53 (27%)", "54 (27%)", "43 (22%)", "50 (25%)",
      NA, "68 (34%)", "68 (34%)", "64 (32%)", "61 (32%)", "<12 (<6%)",
      "112 (56%)", "22.4 (16.0, 24.0)"
    )), row.names = c(NA, -17L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ))
  )
})


testthat::test_that("masks overall", {
  testthat::expect_message(
    gtsummary::trial |>
      gtsummary::tbl_summary(by = trt) |>
      gtsummary::add_overall() |>
      gtsummary::add_n() |>
      mask_micro_summary(micro.n = 12)
  )
})
