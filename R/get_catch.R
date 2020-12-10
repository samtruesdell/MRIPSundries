


#' Calculate catch-related quantities
#'
#' Returns data frame with (a function of) estimated harvest in numbers, releases in numbers,
#' harvest in KG and harvest in LBS.
#'
#' @param cdat A catch data frame (probably created from bindMRIP())
#' @param group A vector of column names to structure output over (Year,
#'                 wave, MODE_FX, etc.).
#' @param agFun The function to summarize the results by. For example, if
#'              <group> was "YEAR" and the function was sum it would return
#'              annual totals. If the function was max it would return the
#'              maximum catch records in each year. Defaults to sum().
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


get_catch <- function(cdat, group, common, agFun = sum){

  cdat_typ <- substr(attributes(cdat)$bindMRIPFrame, 6, 10)
  if(cdat_typ != 'catch'){
    stop(paste0('please provide effort data to argument cdat.\n',
                'type provided was ', cdat_typ, '.'))
  }

  if(is.null(common)){
    common <- unique(cdat$COMMON)
  }

  group <- c('COMMON', group)

  if(!all(common %in% unique(cdat$COMMON))){
    stop('get_catch: <common> not recognized -- check spelling & case')
  }

  if(!all(group %in% names(cdat))){
    w <- which(!group %in% names(cdat))
    stop(paste(group[w], 'not found in the data set column names. Check ',
               'spelling and capitalization\n'))
  }

  # Define group order for sensical output order
  groupRef <- c('COMMON', 'YEAR', 'ST', 'WAVE', 'MODE_FX', 'AREA_X')
  group <- group[order(match(group, groupRef))]


  # colLst <- as.list(cdat[group])
  # names(colLst) <- group

  # Subset for species
  if(!is.null(common)){
    odat <- filter(cdat, COMMON %in% common)
  }else{
    odat <- cdat
  }

  ag <- group_by_at(odat, vars(all_of(group))) %>%
    summarise(HN = agFun(LANDING * WP_CATCH),
              RN = agFun(RELEASE * WP_CATCH),
              HKG = agFun(WGT_AB1 * WP_CATCH),
              .groups = 'drop')

  ag$HLB <- ag$HKG * 2.2046226218


  return(ag)


# old version
#   ag <- with(cdat, aggregate(x = list(HN = LANDING * WP_CATCH,
#                                       RN = RELEASE * WP_CATCH,
#                                       HKG = WGT_AB1 * WP_CATCH),
#                              by = colLst,
#                              FUN = agFun))



}








