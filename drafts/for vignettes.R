#' @examples
#' require(survival)
#' with(gtsummary::trial, survival::coxph(
#'   survival::Surv(ttdeath, death) ~ strata(trt) + age
#' )) |>
#'   ggsurvfit::survfit2() |>
#'   ggsurvfit::tidy_survfit(times = c(0, 10, 15, 20, 24)) |>
#'   tidyr::pivot_wider(
#'     id_cols = strata, names_from = time,
#'     values_from = cum.event
#'   ) |>
#'   mask_micro_table(col.sel = -strata)
