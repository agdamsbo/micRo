#' Microdata violation evaluation
#'
#' @description
#' Evaluates if tables contains microdata. Initially supports
#' `gtsummary::tbl_summary()` tables. Colors problematic cells by two criteria:
#' 1) contains counts less than a specified threshold (default is three), or 2)
#' contains counts that are less than the specified threshold from other cells
#' in the same row.
#' Ref: https://gt.rstudio.com/reference/tab_style.html
#'
#' @param data table to evaluate
#' @param n.min count difference threshold (default is 3)
#'
#' @return gt_tbl list object
#' @export
#'
#' @examples
#' # mtcars |> gtsummary::tbl_summary(by = gear) |> check_microdata()
check_micro <- function(data, n.min) {
  UseMethod("check_microdata")
}

#' Microdata violation evaluation
#'
#' @param data table to evaluate
#' @param n.min count difference threshold (default is 3)
#'
#' @return gt_tbl list object
#'
check_micro.default <- function(data, n.min) {
  stop("This function only supports gtsummary tbl_summary class tables.")
}

# check_microdata.gt_tbl <- function(data, n.min = 3) {
#   names(ls)
#   # This will be the simple version as gt_tbl data is not as strictly formatted.
# }


#' Microdata violation evaluation
#'
#' @param data table to evaluate
#' @param n.min count difference threshold (default is 3)
#'
#' @return gt_tbl list object
#'
check_micro.tbl_summary <- function(data, n.min = 3) {
  gt <- data |> gtsummary::as_gt()

  df_ext <- gt$`_data`|>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      function(.x) project.aid::str_extract(.x, "^\\d+")
    ))

  # Defining relevant rows and cols
  rows <- which(gt$`_data`$var_type %in% c("categorical","dichotomous"))

  cols <- grep("^stat_", names(gt$`_data`))

  # Assessing cell values
  below_three <- seq_len(nrow(df_ext)) |> purrr::map(function(.y) {
    rall <- as.numeric(as.matrix(as.data.frame(df_ext))[.y, ])

    # three or below
    rall %in% seq_len(n.min)
  })


  ## Improvement: for categorical variables the same test should be
  ## applied to the column
  diff_three <- seq_len(nrow(df_ext)) |> purrr::map(function(.y) {
    rall <- as.numeric(as.matrix(as.data.frame(df_ext))[.y, ])

    # difference to any other cell in row of 3 or less
    rall |>
      purrr::map(function(.z) {
        # Limits to only check with relevant cols, while keeping all for easy indexing
        # This approach to avoid interference with levels from categorical variables
        any(abs(rall[cols] - .z) %in% seq_len(n.min))&& .z!=0
      }) |>
      purrr::list_c()
  })

  # Applying difference of three
  for (i in rows){
    gt <- gt |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#F9E3D6"),
          gt::cell_text(style = "italic")
        ),
        locations = gt::cells_body(
          columns = colnames(df_ext[cols])[which(diff_three[[i]][cols])],
          rows = i
        )
      )
  }

  # Applying three or less
  for (i in rows){
    gt <- gt |>
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#f9d6db"),
          gt::cell_text(style = "italic")
        ),
        locations = gt::cells_body(
          columns = colnames(df_ext[cols])[which(below_three[[i]][cols])],
          rows = i
        )
      )
  }

  gt
}
