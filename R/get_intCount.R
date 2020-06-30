
#' Calculate the number of intercepts and the number of interviews
#'
#' Returns data frame containing intercepts and interview counts
#'
#' @param cdat A catch data frame created from bindMRIP().
#'
#' @param tdat A trip data frame created from bindMRIP().
#'
#' @param group A string of column names to structure output over (YEAR,
#'              COMMON, WAVE, MODE_FX, etc.).
#'
#' @return A data frame with the outputs structured by the inputs to the
#'         <group> argument.
#'
#' @examples
#' int <- get_intCount(cdat = cdat,    # Catch data frame
#'                     tdat = tdat,    # Trip data frame
#'                     group = group)  # Grouping vector
#'
#' @export



get_intCount <- function(cdat, tdat, group){

  # cdat_typ <- substr(attributes(cdat)$bindMRIPFrame, 6, 10)
  # if(cdat_typ != 'catch'){
  #   stop(paste0('please provide catch data to argument tdat.\n',
  #               'type provided was ', cdat_typ, '.'))
  # }
  #
  # tdat_typ <- substr(attributes(tdat)$bindMRIPFrame, 6, 9)
  # if(tdat_typ != 'trip'){
  #   stop(paste0('please provide effort data to argument tdat.\n',
  #               'type provided was ', tdat_typ, '.'))
  # }

  # #ensure that the same frames were used for the data queries
  # frameT <- attributes(tdat)$bindMRIPFrame
  # frameC <- attributes(cdat)$bindMRIPFrame
  #
  # frameTEl <- str_split(frameT, pattern = '_')[[1]]
  # frameCEl <- str_split(frameC, pattern = '_')[[1]]
  #
  # if(frameTEl[2] != frameCEl[2] | frameTEl[3] != frameCEl[3]){
  #   stop(paste0('Wave and year domains for input data should be equal. ',
  #               'try re-running bindMRIP() with type = \'catch\' and ',
  #               'type = \'trip\' using the same year and wave parameters.\n',
  #               'trip frame: ', frameT, '\n',
  #               'catch frame: ', frameC))
  # }

  ### Ensure that the catch frame is equal to or a subset of the trip frame
  # Common columns between catch and trip data frames
  commonCol <- c('YEAR', 'ST', 'MODE_FX', 'WAVE', 'AREA_X', 'MONTH')

  # Get unique values for catch
  cintCol <-  cdat  %>%
      select(all_of(commonCol)) %>%
      distinct()

  # Get unique values for trip
  tintCol <- tdat %>%
    select(all_of(commonCol)) %>%
    distinct()

  # new data frame with all rows in unique catch data frame not contained in
  # the unique trip data frame (hopefully there are none)
  aj <- anti_join(cintCol, tintCol,
                  by = c('YEAR', 'ST', 'MODE_FX', 'WAVE', 'AREA_X', 'MONTH'))

  if(nrow(aj) > 0){
    stop(paste('catch data set has elements not contained in the trip data',
               'set. Check that any groupings or subsetting used during or',
               'after a call to bind_MRIP() haven\'t offset the values. The',
               'values checked were:', paste(commonCol, collapse = ', ')))
  }

  # Join the catch and trip data frames, retaining all the catch rows. Had to
  # remove PSU_ID from list of joining columns because it somehow leads to
  # differences between the trip and catch data sets.
  jcols <- c('STRAT_ID', 'YEAR', 'ST', 'MODE_FX', 'AREA_X',
             'ID_CODE', 'SUB_REG', 'WAVE', 'KOD', 'MONTH',
             'WP_INT', 'VAR_ID', 'ARX_METHOD', 'ALT_FLAG')
  ct <- left_join(cdat, tdat, by = jcols)


  grpName <- sapply(group, as.name)

  out <- ct %>%
    group_by_at(group) %>%
    summarize(nIntercept = length(unique(LEADER)),
              nInterview = n(), .groups = 'drop') %>%
    ungroup() %>%
    complete(!!!grpName, fill = list(nIntercept = 0, nInterview = 0))

  return(out)

}



