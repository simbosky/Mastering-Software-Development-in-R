#' Read in Fatality Analysis Reporting System data file
#' 
#' \code{fars_read} reads in the US National Highway Traffic Safety 
#'   Administration's Fatality Analysis Reporting System data.
#' 
#' @param filename csv file containing data
#' 
#' @return \code{fars_read} will search within the specified path for the 
#'   filename provided. If the file exists, it will be imported and returned as 
#'   a data frame tbl.  If it does not exist an error message will be returned.
#' 
#' @examples 
#' fars_read("data/my_fars_data.csv")
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


#' Make File Name
#' 
#' \code{make_file} creates a name for the accident csv file based on the 
#'   year provided.
#' 
#' @param year the year to add to the file name
#' 
#' @return \code{make_file} will return a file name based on the year provided.
#'   For example, if 2017 is provided as the year the name that will be returned
#'   will be "accident_2017.csv.bz2".
#' 
#' @examples 
#' make_filename(2017)
#' 
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in Fatality Analysis Reporting System data files
#' 
#' \code{fars_read_years} will read in multiple Fatality Analysis Reporting 
#'   System data files based on the years provided.
#' 
#' @param years The years relating to the file names to be read in
#' 
#' @return \code{fars_read_years} will search for the file names based on the 
#'   years provided. For example, if 2016:2017 is provided \code{fars_read_years}
#'   will search for the following files:
#'   \itemize{
#'     \item "accident_2016.csv.bz2"
#'     \item "accident_2017.csv.bz2"
#'   }
#'   If the files exist a list containing the respective data will be returned.
#'   If the files do not exist an error will be returned stating the invalid year(s).
#' 
#' @seealso \code{\link{make_filename}} for naming convention
#' 
#' @examples 
#' fars_read_years(2015:2016)
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

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