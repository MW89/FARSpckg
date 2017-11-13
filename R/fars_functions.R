#' Read in FARS data
#'
#' This functions reads in a data from the US National Highway Traffic Safety
#' Administration's Fatality Analysis Reporting System (FARS) saved as a
#' CSV file.
#'
#' @param filename A character string giving the (path and) filename of the
#' FARS data file to be loaded
#'
#' @return Returns the corresponding data frame (tibble) in R.
#'
#' @note An error is invoced if no valid filename is given.
#' The data (csv.bz2) needs to be in the working directory.
#'
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



#' Make filename from year
#'
#' This simple function creates the filename of a FARS data set given a
#' year specific
#'
#' @param year A four-digit integer specifying the year
#'
#' @return Return the filename of the FARS data set for the input year.
#'
#' @note No input checking is done, so be sure to specify a valid year.
#'
#' @examples
#' make_filename(2014)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}



#' Read in FARS data for several years
#'
#' This function reads in multiple FARS data sets for several years.
#'
#' @param years A vector or list of 4-digit integers specifying the years for
#' which the data should be loaded.
#'
#' @return Returns a list of data frames (tibbles) corresponding to the FARS
#' data sets for the input years
#'
#' @note The csv files need to be named correctly and need to be saved in the
#' working directory. If the i-th entry of the input years is invalid the i-th
#' value of the returned list will be NULL and a warning message will be given.
#' The data (csv.bz2) needs to be in the working directory.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @export
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


#' Display number of records per month and year
#'
#' This function summarizes the number of  fatal injuries suffered in motor
#' vehicle traffic crashes in each month for each year requested by the user.
#'
#' @param years A vector or list of 4-digit integers specifying the years for
#' which to calculate the number of fatal injuries.
#'
#' @return This function returns a data frame (tibble) with the number of fatal
#' injuries per month (rows) for every year (columns) input.
#'
#' @note Partial input checking is done by invoked functions. If no valid year
#' is given as input, an error results. The data (csv.bz2) needs to be in the working directory.
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Create map of fatal injuries
#'
#' This function displays a basic map of where fatal injuries suffered in motor
#' vehicle traffic crashes occured for a specific year in a specific state.
#'
#' @param state.num Integer between 1 and 56 corresponding to the state number
#' @param year 4-digit integer specifying the year
#'
#' @return None
#'
#' @note An invalid state number or your will result in an error. If no
#' accidents occured in the given state and your a message will be printed.
#' The data (csv.bz2) needs to be in the working directory.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
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
