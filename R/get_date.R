

#' Parse dates from ID_CODE columns
#'
#' Creates a vector of dates
#'
#' @param dat MRIP size, trip or catch data frame
#'
#' @return vector of dates
#'
#' @examples
#' get_date(dat)
#'
#' @export

get_date <- function(dat){

  if(is.null(dat$ID_CODE)){
    stop('ID_CODE column not available: is this an MRIP data set?')
  }

  dates <- lubridate::ymd(as.numeric(substr(dat$ID_CODE, start = 6, stop = 13)))

}




