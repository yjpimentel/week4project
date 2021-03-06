% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Summarize the data selected by the fars_read_years function
This function uses the functions, group_by, summarize and spread functions from the
dplyr and tidyr packages to summarize the data from using the fars_read_years function.
The input can be one or more years (using the \code{years}).}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A vector of integers specifying the years that summarized data is wanted}
}
\value{
This function returns a list of the summarized data for one or more years
}
\description{
Summarize the data selected by the fars_read_years function
This function uses the functions, group_by, summarize and spread functions from the
dplyr and tidyr packages to summarize the data from using the fars_read_years function.
The input can be one or more years (using the \code{years}).
}
