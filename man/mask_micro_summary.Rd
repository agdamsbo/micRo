% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mask_micro_summary.R
\name{mask_micro_summary}
\alias{mask_micro_summary}
\title{Mask micro data in gtsummary::tbl_summary() tables}
\usage{
mask_micro_summary(data, micro.n = 5)
}
\arguments{
\item{data}{gtsummary summary table}

\item{micro.n}{n to mask at. Passed on.}
}
\value{
list
}
\description{
Mask micro data in gtsummary::tbl_summary() tables
}
\examples{
ls <- gtsummary::trial |> gtsummary::tbl_summary(by=trt)
ls |> mask_micro_summary()
ls |> gtsummary::add_overall() |> mask_micro_summary()
}
