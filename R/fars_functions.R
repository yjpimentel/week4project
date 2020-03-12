#' Read FARS dataset
#'
#' This is a function that reads in the dataset for the Fatality Analysis Reporting System (FARS).
#' The function first checks if the file exists within the working directory and will stop if the
#' file does not exist. The user can customize the file to be read (using the 'function(filename)'
#' argument) and insert a new 'filename.'
#'
#' @param supressMessages A function to suppress any messages that may be produced while reading in
#' the dataset
#'
#' @param read_csv This function reads a csv file into the envionment for review and analysis.
#'
#' @param progress Suppresses a progress bar that shows the rate at which the file is read
#'
#' @param tbl_df This function produces a dataframe of the dataset read in by the function
#'
#' @return This function returns a dataframe from the loaded file of class attributes c("tbl_df", "tbl", "data.frame) and a
#' base type of list.
#'
#' Errors may be produced if the filename is not present in the working directory or the filename is
#' not included in quotation marks
#'

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create Filename
#'
#' This function creates the filename of the individual datasets for the FARS data. Using the year
#' specified, the function creates a new name with the corresponding date.
#'
#' @param as.integer this function converts the 'year' field into an integer value (from a character
#' value) for analysis.
#'
#'
#' @return This function is a wrapper for the the C function returning a character value with the
#' names for the individual years of the distinct datasets
#'
#' @example make_filename("2013")

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Date Extraction
#'
#' This function extracts the month and year of the loaded datasets and returns a new list with the
#' two variables. The years can be specified within the function call.
#'
#' @param lapply returns a list of the same length as the dataset and applies the ensuing function to
#' each row of the original file
#'
#' @import dplyr::mutate creates a new variable from the dataset while existing the pre-existing
#' variables
#'
#' @import dplyr::select chooses variables from an existing table
#'
#' @return The function returns a list of the year and month columns from the individual
#' datasets
#'
#' Errors may be produced if the filename is not present in the working directory. If the filename is
#' not present, a warning message will be returned indicating the invalid year and a NULL value
#'
#' @examples fars_read_years(2013)
#'

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Monthly Accident Summary
#'
#' This function provides a summary of the total monthly accidents per year. The new list returned
#' by the function provides a sum of all accidents in all twelve months per year. The years can be
#' specified within the function call.
#'
#' @import dplyr::bind_rows This argument binds the individual rows produced in the preceding
#' function into a single list.
#'
#' @import dplyr::group_by groups an existing table by the categories in a variable. Operations can
#' then be perdormed on the existing table by'groups.'
#'
#' @import dplyr::summarize This reduces multiple vaues down to a single value. In this case it reduces
#' the total number of accidents for a given month and year into a single value.
#'
#' @import tidyr::spread This function spreads a "key-value" pair across a number of different
#' columns. The function in this case pairs the individual month along with the summarized accident
#' totals for the given year.
#'
#' @return The function returns a list of the total accidents for each individual month per year.
#'
#' @examples fars_summarize_years(2013)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Accident Map Visualization
#'
#' This function provides a map of the individual states and plots each accident on its latitudue and
#' longitude coordinates. The function requires both the state number and the year to function
#' correctly.
#'
#' @import dplyr::filter This functions returns only the rows with the matching conditions specified.
#' In this case, the function returns the rows where the STATE variable is equivalent to the state
#' number
#'
#' @import maps::map This functions draws a geographical map of the state identified within the
#' function.
#'
#' @import graphics::points This function adds points to a plot. For this function, the points
#' operation plots the points at the longitude and latitude of each accident in the original dataset.
#'
#' Errors can occur if the state number or year are not specified. In addition, errors can result
#' from erroneously including quotation marks.
#'
#' @return The function returns a map of each US state (as identified by the state number in the
#' function call) and the corresponding year. The map provides an outlined image of the state with
#' accidents plotted along their longitudinal and latitudinal coordinates
#'
#' @examples fars_map_state(53,2013)
#'

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
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
