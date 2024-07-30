utils::globalVariables(c( "N", "N_miss", "N_obs", "col_name", "n", "row_type", "stat_0", "variable"))

#' Mask micro data in gtsummary::tbl_summary() tables
#'
#' @param data gtsummary summary table
#' @param micro.n n to mask at. Passed on.
#'
#' @return list
#' @export
#'
#' @examples
#' ls <- gtsummary::trial |> gtsummary::tbl_summary(by=trt)
#' ls |> mask_micro_summary()
#' ls |> gtsummary::add_overall() |> mask_micro_summary()
mask_micro_summary <- function(data,micro.n=5){
  message("Please remember always to double-check your tables before exporting.")

  if ("n" %in% names(data$table_body) & "missing" %in% data$table_body$row_type) {
    message("You have included an 'n' column as well as counting missings.
            Please make sure you are not exposing micro data.")
  }

  data |>
    gtsummary::modify_table_body(
      ~ .x |> summary_masks(ls=data,cut.off = micro.n,dec=1)
    )
}

#' Implementation step for masking
#'
#' @param body table body
#' @param ls complete summary table list object
#' @param cut.off micro cut point
#' @param dec decimals, passed on
#'
#' @return tibble
summary_masks <- function(body,ls,cut.off=5,dec=dec){
  seq_along(unique(body$variable)) |>
    purrr::map(function(.x) {
      variable_masks(index=.x,data=ls,cut.off=cut.off,dec=dec)
    }) |>
    dplyr::bind_rows()
}

#' Two-dimensional micro data masking to handle binary and nominal data
#'
#' @param body full table body
#' @param tb filtered table body
#' @param f1 Subsets indexes of relevant columns
#' @param ns Relevant ns arranged in matrix
#' @param Ns All Ns
#' @param cut.off micro cut point
#' @param glue.mask glue string for masking
#' @param dec decimals
#' @param minimal flag to mask minimally if no overall column is present
#'
#' @return tibble
masking <- function(body,
                    tb=NULL,
                    f1,
                    ns,
                    Ns,
                    cut.off=cut.off,
                    glue.mask=glue.mask,
                    dec=dec,
                    minimal=FALSE
){

  # browser(skipCalls = 1)
  if (is.null(tb)) tb <- body

  ## Logical matrix of relevant ns to mask
  f2 <- ns |>
    purrr::map(\(.x) .x %in% 1:(cut.off - 1)) |>
    dplyr::bind_cols() |>
    as.matrix()

  ## Number of cells with zero observations in each matrix row
  n0 <- apply(ns == 0, 1, sum)

  ## Number of cells with ns to mask in each matrix row
  ns.low <- apply(f2, 1, sum)


  if (all(ns.low==0)){
    out <- body
  } else {

    ls.rows <- seq_len(nrow(f2)) |>
      purrr::map(\(.i){

        ds <- tb[.i, ]
        # Creating masked matrix to record maskings
        masked <- matrix(FALSE, ncol = ncol(f2))
        # browser()
        # Only modify in case of low, handle one col matrices
        if (apply(f2, 1, any)[.i]) {
          n <- cut.off
          N <- Ns[f2[.i,]]
          p <- round(100 * n / N, dec)

          ds[f1[f2[.i,]]] <- glue::glue(glue.mask) |>
            as.matrix() |>
            t() |>
            tibble::as_tibble(.name_repair = "unique_quiet")
          masked[f2[.i,]] <- TRUE

          if (!minimal){ # only run if minimal is not TRUE
            ## loop to add masks until satisfied
            while (sum(masked)==1 & # The case of only one low
                   nrow(masked)>1 | # But ignored in case of ncol==1
                   sum(ns[.i,][masked]) <= cut.off &
                   (sum(masked) + n0[.i]) < ncol(masked)
            ) {
              # in the case that sum of smalls is below cutoff, another field is added.

              ranked <- apply(ns[.i,], 1, rank, ties.method = "first") |> t()

              ranked.i <- ranked == sum(masked) + n0[.i] + 1
              n <- ns[.i,][ranked.i] |> plyr::round_any(accuracy = cut.off, f = ceiling)
              N <- Ns[ranked.i]
              p <- round(100 * n / N, dec)

              ds[f1[ranked.i]] <- glue::glue(glue.mask)
              masked[ranked.i] <- TRUE
            }

          }
          }

        list(
          ds = ds,
          masked = masked
        )
      })

    # The case for dichotomous
    if (nrow(tb) == 1) {
      out <- ls.rows |>
        purrr::map(purrr::pluck, "ds") |>
        dplyr::bind_rows()

      # Handling categorical data
    } else if (nrow(tb) > 1) {
      masks <- ls.rows |>
        purrr::map(purrr::pluck, "masked") |>
        purrr::reduce(rbind)

      out <- ls.rows |>
        purrr::map(purrr::pluck, "ds") |>
        dplyr::bind_rows()

      ## Indices by row
      col.i <- seq_len(nrow(masks)) |> purrr::map(\(.j){
        which(masks[.j,])
      })

      col.i.vec <- purrr::list_c(col.i) |> unique()

      if (purrr::compact(col.i) |> length() == 1){
        # As this is only the case with overall column
        # This should be reworked

        # This was the approach, to just select the first
        # which(purrr::map_lgl(col.i,is_empty))[1]

        # This will select the cell with the second lowest number
        col.i[[which(rank(ns,ties.method = "first")==2)]] <- c("")
      }

      out <- col.i |>
        purrr::map(\(.y){
          # length(.y)
          if (length(.y) > 0) {
            cols <- col.i.vec[!col.i.vec %in% .y]
            if (length(cols)==0){
              cols <- ""
            } else {
              cols
            }
          } else {
            .y
          }
        }) |>
        purrr::imap(\(.y, .i){
          ds <- out[.i, ]
          if (length(.y) > 0 & all(.y!="")) {
            n <- ns[.i, .y] |>
              purrr::map_dfr(plyr::round_any,accuracy = cut.off, f = ceiling)
            N <- Ns[.y]
            p <- round(100 * n / N, dec)
            if (n==0) glue.mask <- "{n}"

            ds[f1[.y]] <- glue::glue(glue.mask)|>
              as.matrix() |>
              t() |>
              tibble::as_tibble(.name_repair = "unique_quiet")
            ds
          } else {
            ds
          }
        }) |>
        dplyr::bind_rows()

      out <- rbind(
        body |>
          dplyr::filter(row_type=="label"),
        out
      )
    }
  }
  out

}

missing_stats <- function(index,data,glue.mask= "{N_miss} ({round(p_miss*100,dec)}%)",dec){
  index.var <- unique(data$table_body$variable)[index]

  table_body <- data$table_body |>
    dplyr::filter(variable == index.var)

  if ("missing" %in% table_body$row_type){
    f1 <- grep("stat_\\d+",names(table_body))

    ## This approach
    masks <- data$meta_data$df_stats |>
      purrr::pluck(index)  |>
      dplyr::filter(!duplicated(col_name)) |>
      dplyr::arrange(col_name) |>
      dplyr::mutate(mask=glue::glue(glue.mask))|>
      dplyr::pull(mask)

    table_body[table_body$row_type=="missing",f1] <- masks |>
      as.matrix() |>
      t() |>
      tibble::as_tibble(.name_repair = "unique_quiet")

  }
  table_body
}

missing_stats_steps <- function(body,ls,glue.mask,dec){
  seq_along(unique(body$variable)) |>
    purrr::map(function(.x) {
      missing_stats(index=.x,data=ls,glue.mask = glue.mask,dec=dec)
    }) |>
    dplyr::bind_rows()
}

add_missing_stats <- function(data,glue.mask= "{N_miss} ({round(p_miss*100,dec)}%)",dec=1){
  data |>
    gtsummary::modify_table_body(
      ~ .x |> missing_stats_steps(ls=data,glue.mask = glue.mask,dec=dec)
    )
}

variable_masks <- function(index, data, cut.off,glue.mask= "<{n} (<{p}%)",glue.mask.missing="<{n}",dec=dec) {
  index.var <- unique(data$table_body$variable)[index]

  table_body <- data$table_body |>
    dplyr::filter(variable == index.var)

  ## Filtering bu two different approaches
  if (table_body$var_type[1] %in% c("dichotomous", "categorical")) {
    if (any(grepl("^stat_[1-9]|[1-9]\\d",names(table_body)))){
      masked <- micro_n_masks(
        ## Handling nominal/binary

        body = table_body |>
          dplyr::filter(row_type!="missing"),
        n.all = data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(n),
        N.all=data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(N),
        cut.off=cut.off,
        glue.mask = glue.mask
      )
    } else {
      masked <- table_body |> dplyr::filter(row_type!="missing")
    }


    if ("stat_0" %in% names(masked)){
      body <-  masked
      tb  <-  body |> dplyr::filter(row_type!="missing",!is.na(stat_0))
      f1  <-  grep("^stat_0", names(tb))

      meta.index <- data$meta_data$df_stats |>
        purrr::pluck(index)

      if ("by" %in% names(meta.index)) {
        meta.index <- meta.index |> dplyr::filter(is.na(by))
      }

      masked <- masking(body=body,
                        tb=tb,
                        f1=f1,
                        ns =  meta.index  |>
                          dplyr::select(n)|>
                          dplyr::slice(seq_len(length(f1) * nrow(tb))) |>
                          unlist(use.names = FALSE) |>
                          matrix(ncol = length(f1), byrow = TRUE) |>
                          tibble::as_tibble(.name_repair = "unique_quiet"),
                        Ns= meta.index |>
                          dplyr::select(N_obs) |>
                          dplyr::slice(1)|>
                          unlist(use.names = FALSE),
                        cut.off=cut.off,
                        glue.mask=glue.mask,
                        dec=dec)
    }
    out <- rbind(
      masked,
      table_body |> dplyr::filter(row_type=="missing"))

  } else {
    out <- table_body
  }

  if ("missing" %in% out$row_type) {
    ## Handling missings n is N_miss, and N is N_obs

    if (!"stat_0" %in% names(out)){
      missings <- micro_n_masks(
        body = out |>
          dplyr::filter(row_type == "missing"),
        n.all = data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(N_miss),
        N.all=data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(N_obs),
        cut.off=cut.off,
        glue.mask=glue.mask.missing,
        minimal=TRUE
      )
    } else {
      missings <- micro_n_masks(
        body = out |>
          dplyr::filter(row_type == "missing"),
        n.all = data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(N_miss),
        N.all=data$meta_data$df_stats |>
          purrr::pluck(index) |>
          dplyr::select(N_obs),
        cut.off=cut.off,
        glue.mask=glue.mask.missing
      )
      ## Handling overall column
      body  <-  missings
      tb <-  body |> dplyr::filter(row_type=="missing")
      f1 <-  grep("^stat_0", names(tb))

      missings <- masking(body=body,
              tb=tb,
              f1=f1,
              ns = data$meta_data$df_stats |>
                purrr::pluck(index) |>
                dplyr::filter(is.na(by)) |>
                dplyr::select(N_miss) |>
                dplyr::slice(1) |>
                unlist(use.names = FALSE) |>
                matrix(ncol = length(f1), byrow = TRUE) |>
                tibble::as_tibble(.name_repair = "unique_quiet"),
              Ns=data$meta_data$df_stats |>
                purrr::pluck(index) |>
                dplyr::filter(is.na(by)) |>
                dplyr::select(N_obs) |>
                dplyr::slice(1) |>
                unlist(use.names = FALSE),
              cut.off=cut.off*2,
              glue.mask=glue.mask.missing,
              dec=dec)

    }

    out <- rbind(
      out |>
        dplyr::filter(row_type != "missing"),
      missings)

  }

  out
}

micro_n_masks <- function(body, n.all, N.all, cut.off, glue.mask,dec=1,minimal=FALSE) {
  if (nrow(body) > 1) {
    # First row removed in case of categorical
    # Possibly change to include in filter, to have whole df, or just rbind in the end
    tb <- body |> dplyr::filter(row_type!="label")
  } else { # last option is "dichotomous"
    tb <- body
  }

  ## Supports any number of stat columns (overkill!)
  f1 <-  grep("^stat_[1-9]|[1-9]\\d", names(tb))


  masking(body=body,
          tb=tb,
          f1 = f1,
          ns=n.all |>
            dplyr::slice(seq_len(length(f1) * nrow(tb))) |>
            unlist(use.names = FALSE) |>
            matrix(ncol = length(f1), byrow = TRUE) |>
            tibble::as_tibble(.name_repair = "unique_quiet"),
          Ns=N.all |>
            dplyr::slice(seq_len(length(f1))) |>
            unlist(use.names = FALSE),
          cut.off=cut.off,
          glue.mask=glue.mask,
          dec=dec,
          minimal=minimal
  )

  # if ("stat_0" %in% names(tb)){
  #   out <- masking(body=body,
  #                  tb=tb,
  #                  f1 = f1,
  #                  ns=n.all |>
  #                    dplyr::slice(seq_len(length(f1) * nrow(tb))) |>
  #                    unlist(use.names = FALSE) |>
  #                    matrix(ncol = length(f1), byrow = TRUE) |>
  #                    tibble::as_tibble(.name_repair = "unique_quiet"),
  #                  Ns=N.all |>
  #                    dplyr::slice(seq_len(length(f1))) |>
  #                    unlist(use.names = FALSE),
  #                  cut.off=cut.off,
  #                  glue.mask=glue.mask,
  #                  dec=dec
  #   )
  # } else {
  #   out <- tb |>
  #     dplyr::mutate(
  #     dplyr::across(f1,function(.x){
  #       num <- as.numeric(.x)
  #       if (num<cut.off){
  #         n <- plyr::round_any(x = .x,
  #                              accuracy = cut.off,
  #                              f = ceiling)
  #         glue::glue(glue.mask)
  #       } else {
  #         .x
  #       }
  #     })
  #   )
  # }
  #
  # out

}
