#' Calculate column differences
#'
#' @param data tibble
#' @param prefix.pattern tidyselect style column selection. Default is NULL
#' @param include.first include first column. Default is TRUE
#' @param suffix.out suffix for output columns
#'
#' @return tibble
#' @export
#'
#' @examples
#' tibble::tibble(a=sample(1:10,5),b=sample(1:10,5),c=sample(1:10,5)) |>
#' (\(.x) list(.x,column_diffs(.x)))()
#'
column_diffs <- function(data, prefix.pattern = NULL, include.first = TRUE, suffix.out = "_diff") {
  if (!is.null(prefix.pattern)) {
    data <- data |>
      dplyr::select(tidyselect::starts_with(prefix.pattern))
  }

  index <- seq_along(data)[-1]

  diff <- index |>
    purrr::map(\(.y){
      abs(data[.y - 1] - data[.y])
    }) |>
    dplyr::bind_cols() |>
    (\(.x) stats::setNames(.x, paste0(names(.x), suffix.out)))()

  if (include.first) {
    out <- dplyr::bind_cols(data[1], diff)
  } else {
    out <- diff
  }
  out
}
