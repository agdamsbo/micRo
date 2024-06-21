utils::globalVariables("mask")
#' Create interval with value rounded down and up to nearest number specified
#'
#' @param data numeric vector
#' @param round number to round to
#' @param down round down on tie? Default is TRUE. Rounds up if FALSE.
#'
#' @return vector
#' @export
#'
#' @examples
#' 1:10 |> rounded_interval(round=2)
rounded_interval <- function(data, round = 5, down = TRUE) {
  # Handle "ties"
  sub <- ifelse(down, -1, 1)
  data <- ifelse(data %% round == 0 & data != 0, data + 1, data)

  c(floor, ceiling) |>
    purrr::map(\(.x) {
      plyr::round_any(x = data, accuracy = round, f = .x)
    }) |>
    dplyr::bind_cols(.name_repair = "unique_quiet") |>
    stats::setNames(c("l", "h")) |>
    dplyr::transmute(mask = glue::glue("{l}-{h}")) |>
    dplyr::pull(mask)
}
