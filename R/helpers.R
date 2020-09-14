#' Convert dates (year+month) to CMC
#' @param year Calendar year
#' @param month Calendar month
#' @return Date in CMC format

dates_to_cmc <- function(year, month) {
  12*(year-1900)+month
}



#' Convert CMC to dates (year+month)
#' @param cmc CMC-formatted date
#' @return Date in CMC format
#' @return A list containing: \cr
#'     * `year`: calendar year  \cr
#'     * `month`: calendar month

cmc_to_dates <- function(cmc) {
  month <- ifelse(mod(cmc, 12)!=0, mod(cmc, 12), 12)
  return(list(
    year = 1900 + (cmc-month)/12,
    month = month
  ))
}



#' Specify baseline mortality curve
#'     Currently a linear decrease from start_year to end_year
#' @param year Year at which mortality should be calculated
#' @param start_year Start year for linear interpolation
#' @param end_year End year for linear interpolation
#' @param start_u5mr U5MR at start_year
#' @param end_u5mr U5MR at end_year
#' @return U5MR at year (i.e. year specified in first argument)

baseline_u5mr <- function(year, start_year=2000, end_year=2023, start_u5mr=130, end_u5mr=80) {
  
  slope <- (end_u5mr-start_u5mr)/(end_year-start_year)
  intercept <- start_u5mr - (slope*start_year)
  return ( slope*year + intercept )
  
}
