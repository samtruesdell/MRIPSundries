

#' Calculate catch-related quantities for *estimate* data
#'
#' Returns data frame with (a function of) estimated harvest in numbers, releases in numbers,
#' harvest in KG, harvest in LBS, release numbers and total catch, grouped according
#' to user-specified grouping argument.
#'
#' @param cdatEst A catch data frame (probably created from bindMRIPEstimates())
#' @param group A vector of column names to structure output over (Year,
#'                 wave, MODE_FX, etc.).
#'
#' @param common Vector of common names for species of interest. Typically this
#'               will be of length one, e.g., 'TAUTOG.' If length(common) > 1
#'               output will be broken down by species.
#'
#' @return A data frame with the outputs structured by the inputs to the
#'         <group> function.
#'
#' @examples
#' get_catchEstimates(
#'   cdat = boundEstimateDat,      # Catch data, probably created by
#'                                 # bindMRIPEstimates() with type = 'catch'
#'   group = c('WAVE', 'YEAR'),    # Variables that structure the output
#'   common = 'SCUP')              # Function to summarize the results
#'
#' @export

get_catchEstimates <- function(cEstDat, group, common){
  out <- cEstDat %>%
    rename(common2 = common) %>% # rename so we can use common as argument
    filter(common2 == common) %>%
    rename(common = common2) %>%
    group_by_at(c('common', group)) %>%
    summarize(HN = sum(landing),
              varHN = sum(land_var),
              RN = sum(estrel),
              varRN = sum(estrlvar),
              HLB = sum(lbs_ab1),
              varHLB = sum(var_lbs),
              HKG = sum(wgt_ab1),
              varHKG = sum(var_wab1)) %>%
    mutate(CN = HN + RN,
           varCN = NA, #varHN + varRN, # not sure why this doesn't work
           PSEHN = sqrt(varHN) / HN * 100,
           PSERN = sqrt(varRN) / RN * 100,
           PSEHLB = sqrt(varHLB) / HLB * 100,
           PSEHKG = sqrt(varHKG) / HKG * 100,
           PSECN = sqrt(varCN) / CN * 100)
  return(out)
}


