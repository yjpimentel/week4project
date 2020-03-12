#' Read data from file into a dataframe.
#'
#' This function reads data from a file given specified by the user and puts it in a dataframe.
#' If wrong filename is given then it will result in an error.
#' The function tbl_df() from dplyr is used.
#' @param filename A string with the filename of the file to be read.
#'
#' @return This function returns a dataframe with the data read from the file.
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#'Create custom filename
#'
#' This function creates a custom filename string from the combination of year given by the user (using \code{year}) and
#' the default part of the filename of the file with data from the
#' US National Highway Traffic Safety Administration's Fatality Analysis Reporting System.
#' This string can then be used to read data from the file with data from the given year.
#' The user input needs to be an integer otherwise the function will result in an error.
#' @param year An integer respresenting the year to be used in the filename string
#'
#' @return This function returns a string with the filename customized with given year
#'
#' @examples
#' make_filename(2013)
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data/accident_%d.csv.bz2", year)
}


#' Read data from one or several years and select the month and year columns.
#'
#' This function uses the fars_read function to read in data from a file with
#' filename defined by using the input (year) from the user and the function make_filename.
#' The function then uses lapply to go through each year given by the user.
#' The mutate and select function from the dplyr package is then used to select the month and year column from the file.
#'
#' The package dplyr needs to be loaded first.
#' If user input is not an integet and more specifically a year for which data exists
#' the function will raise an error.
#' @param years A vector of integers that will be used by the function
#'
#' @return This function returns a list with the month and year columns from the file
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate select
#'
#'
#'
#' @export
fars_read_years <- function(years) {
  Month <- NULL
  YEAR <- NULL
  MONTH <- NULL
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = YEAR) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#'Summarize the data selected by the fars_read_years function
#' This function uses the functions, group_by, summarize and spread functions from the
#' dplyr and tidyr packages to summarize the data from using the fars_read_years function.
#' The input can be one or more years (using the \code{years}).
#'
#' @param years A vector of integers specifying the years that summarized data is wanted
#'
#' @return This function returns a list of the summarized data for one or more years
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#'
#' @export
fars_summarize_years <- function(years) {
  MONTH <- NULL
  n <- NULL
  year <- NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map the data points on a map over the selected state
#' This function maps the state given by the user (using the \code{state}) and the datapoints
#' for the year given by the user (using the \code{year}).
#' The function uses the map and points functions from the packages map and graphics, respectively.
#' The function uses the function make_filename to create a string of the filename and parses this
#' string to the fars_read function in order to read the data from the file.
#' The function will result in an error if either the state number is not given as an integer
#' or an incorrect state number is given (using the \code{state})
#'
#' @param state Integer specifying for what state data is to be displayed
#' @param year An integer specifying for what year data is to be displayed
#'
#' @return This function returns a map of the given state and the datapoints for that state.
#' @importFrom magrittr "%>%"
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state, year) {
  STATE <- NULL
  filename <- make_filename(year)
  data <- fars_read(filename)
  state <- as.integer(state)

  if(!(state %in% unique(data$STATE)))
    stop("invalid STATE number: ", state)
  data.sub <- dplyr::filter(data, STATE == state)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
