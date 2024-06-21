#' Mask across tibble by column differences
#'
#' @param data tibble
#' @param micro.n n to mask at
#' @param down round down in interval
#' @param col.sel tidyselect style column selection for masking. Required.
#'
#' @return tibble
#' @export
#'
mask_micro_table <- function(data, micro.n = 5, down = TRUE, col.sel) {
  data.sel <- data |>
    dplyr::select({{ col.sel }})

  ## List of the two selection matrices
  masked <- list(
    data.sel, # The actual data
    data.sel |>
      column_diffs(include.first = TRUE) # Row differences (incl first row)
  ) |>
    purrr::map(\(.y){ # For each element in the list, do colwise test
      .y |>
        purrr::map_dfr(\(.x) .x %in% 1:(micro.n - 1))
    }) |>
    purrr::imap(\(.y, .i){ # apply minimal masking to last list object
      if (.i == 2) {
        .y |> minimal_mask(data = data.sel, micro.n = micro.n)
      } else {
        .y
      }
    }) |>
    purrr::reduce(`|`) |> # Combine matrices
    tibble::as_tibble() |> # To tibble
    purrr::map2(data.sel, \(.x, .y){ # Apply masking based on combined selection
      ifelse(.x, rounded_interval(.y, round = micro.n, down = down), .y)
    }) |>
    dplyr::bind_cols()

  ## Bind masked data to original columns
  dplyr::bind_cols(
    data |>
      dplyr::select(-{{ col.sel }}),
    masked |>
      # Converts new to character for uniform data
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x)))
  ) |>
    dplyr::select(colnames(data)) # Order columns as original input data
}

#' Small function to mask minimally
#'
#' @param mask matrix of logical masking
#' @param data tibble
#' @param micro.n n to mask at
#'
#' @return tibble
#'
#' @examples
#' ls <- list(mask = tibble::as_tibble(matrix(c(FALSE, FALSE, TRUE, TRUE), nrow = 1),
#'     .name_repair = "unique_quiet"
#'   ),
#'   data = tibble::as_tibble(matrix(c(20, 12, 8, 4), nrow = 1),
#'     .name_repair = "unique_quiet"
#'   ), micro.n = 5
#' )
#' # minimal_mask(ls$mask,ls$data,5)
minimal_mask <- function(mask, data, micro.n) {
  out <- mask

  for (i in seq_len(nrow(mask))) {
    if (i > 1) {
      for (j in seq_len(ncol(mask))) {
        if (dplyr::pull(mask[i, j])) {
          n <- max(which(which(!out[i, ]) < j))
          out[i, j] <- abs(data[i, n] - data[i, j]) %in% 1:(micro.n - 1)
        }
      }
    }
  }

  out
}
