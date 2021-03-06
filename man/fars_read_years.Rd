% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Read data from one or several years and select the month and year columns.}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of integers that will be used by the function}
}
\value{
This function returns a list with the month and year columns from the file
}
\description{
This function uses the fars_read function to read in data from a file with
filename defined by using the input (year) from the user and the function make_filename.
The function then uses lapply to go through each year given by the user.
The mutate and select function from the dplyr package is then used to select the month and year column from the file.
}
\details{
The package dplyr needs to be loaded first.
If user input is not an integet and more specifically a year for which data exists
the function will raise an error.
}
