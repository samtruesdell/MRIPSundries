


#' Calculate size composition of catch
#'
#' Returns data frame with (a function of) estimated harvest in numbers, releases in numbers,
#' harvest in KG and harvest in LBS.
#'
#' @param sdat An individual catch data frame (probably created from bindMRIP())
#'
#' @param group A vector of column names to structure output over (Year,
#'                 wave, MODE_FX, etc.).
#'
#' @param unit The output units, either 'in' for inches or 'cm' for centimeters.
#'
#' @param common Vector of common names for species of interest. Often this
#'               will be of length one, e.g., 'TAUTOG.' If length(common) > 1
#'               output will be broken down by species.
#'
#' @return A data frame with the outputs structured by the inputs to the
#'         <group> function.
#'
#' @examples
#' get_catch(cdat = boundDat,              # Catch data, probably created by
#'                                         # bindMRIP() with type = 'catch'
#'           group = c('WAVE', 'YEAR'), # Variables that structure the output
#'           agFun = sum)                  # Function to summarize the results
#'
#' @export


get_harvestSizeComp <- function(sdat, group, common = NULL,
                                unit = c('in', 'cm')){

  group <- c('COMMON', group)

  if(!all(common %in% unique(sdat$COMMON))){
    stop('get_catch: <common> not recognized -- check spelling & case')
  }

  if(!all(group %in% names(sdat))){
    w <- which(!group %in% names(sdat))
    stop(paste(group[w], 'not found in the data set column names. Check ',
               'spelling and capitalization\n'))
  }

  if(length(unit) != 1){
    stop('unit must be *in* OR *cm*')
  }

  if(unit == 'in'){
    uname <- 'L_IN_BIN'
  }else if(unit == 'cm'){
    uname <- 'L_CM_BIN'
  }else{
    stop('get_catchSizeComp: check unit name')
  }


  # Subset for species
  if(!is.null(common)){
    sdat <- filter(sdat, COMMON == common)
  }

  # Set up factor
  colLst <- as.list(sdat[group])
  names(colLst) <- group

  colLst[[length(colLst) + 1]] <- sdat[[uname]]
  names(colLst)[length(colLst)] <- uname



  ag <- group_by_at(sdat, vars(all_of(c(group, uname)))) %>%
    summarise(HN = sum(WP_SIZE),
              HKG = sum(WP_SIZE * WGT)) %>%
    ungroup()

  ag$HLB <- ag$HKG * 2.2046226218


  return(ag)




}





