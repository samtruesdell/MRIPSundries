


#' Calculate fishing effort
#'
#' Returns data frame of fishing effort by group. Type of effort is defined
#' in the function
#'
#' @param tdat A trip data frame created from bindMRIP()
#'
#' @param cdat (optional) A trip data frame created from bindMRIP(). Must be
#'             provided if argument type includes 'A', 'B1', or 'B2'.
#'
#' @param common Common name for species of interest, e.g., 'TAUTOG.' May not
#'               be a vector.
#'
#' @param group A vector of column names to structure output over (Year,
#'                 wave, MODE_FX, etc.).
#'
#' @param type A character vector of types of effort to include in the effort
#'             definition. Options are:
#'               PRIM1_COMMON -- primary target
#'               PRIM2_COMMON -- secondary target
#'               A -- type A catch (claim, observed landings)
#'               B1 -- type B1 catch (unobserved landings)
#'               B2 -- type B2 catch (unobserved releases)
#'
#' @param agFun The function to summarize the results by. For example, if
#'              <group> was "YEAR" and the function was sum it would return
#'              annual totals. If the function was max it would return the
#'              maximum catch records in each year. Defaults to sum().
#'
#'
#' @return A data frame with the outputs structured by the inputs to the
#'         <group> function.
#'
#' @examples
#' eff <- get_effort(tdat = mDatT,                              # Trip data set
#'                   common = 'BLUEFISH',                       # species of interest
#'                   group = c('YEAR', 'ST'),                   # grouping over
#'                   type = c('PRIM1_COMMON', 'PRIM2_COMMON'))  # effort definition
#'
#' @export







get_effort <- function(tdat, cdat = NULL, common, group,
                       type = c('PRIM1_COMMON', 'PRIM2_COMMON',
                                'A', 'B1', 'B2'), agFun = sum){

  if('COMMON' %in% group){
    stop('for now, don\'t include common in group...')
  }

  tdat_typ <- substr(attributes(tdat)$bindMRIPFrame, 6, 9)
  if(tdat_typ != 'trip'){
    stop(paste0('please provide effort data to argument tdat.\n',
                'type provided was ', tdat_typ, '.'))
  }

  if(is.null(cdat)){
     if(any(c('A', 'B1', 'B2') %in% type)){
      stop(paste('types A, B1 and B2 require a catch data set',
                 'typically provided by bindMRIP() with type = \'catch\'',
                 'be provided to argument cdat'))
     }
  }

  if(!is.null(cdat)){

    # if(length(unique(cdat$COMMON)) > 1){
    #   stop(paste('Please provide a catch data set (argument cdat)',
    #              'that contains only one species'))
    # }

    # if(common != unique(cdat$COMMON)){
    #   stop(paste('argument common should be the same as the species',
    #              'catch data set (argument cdat)'))
    # }

    if(!all(group %in% names(cdat))){
      w <- which(!group %in% names(cdat))
      stop(paste(group[w], 'not found in <cdat> column names. Check ',
                 'spelling and capitalization\n'))
    }

    if(!all(common %in% unique(cdat$COMMON))){
      w <- which(!common %in% unique(cdat$COMMON))
      stop(paste(paste(common[w], collapse = ', '), 'not found in <cdat>'))
    }

  }

  if(!all(group %in% names(tdat))){
    w <- which(!group %in% names(tdat))
    stop(paste(group[w], 'not found in <tdat> column names. Check ',
               'spelling and capitalization\n'))
  }

  # Define group order for sensical output order
  groupRef <- c('YEAR', 'ST', 'WAVE', 'MODE_FX', 'AREA_X')
  group <- group[order(match(group, groupRef))]


  # If catch data are going to be used (i.e., if type is A, B1 or B2),
  # then should ensure that the same frames were used for the data
  # queries
  if(!is.null(cdat)){

    frameT <- attributes(tdat)$bindMRIPFrame
    frameC <- attributes(cdat)$bindMRIPFrame

    frameTEl <- str_split(frameT, pattern = '_')[[1]]
    frameCEl <- str_split(frameC, pattern = '_')[[1]]

    if(frameTEl[2] != frameCEl[2] | frameTEl[3] != frameCEl[3]){
      stop(paste0('Wave and year domains for input data should be equal. ',
                  'try re-running bindMRIP() with type = \'catch\' and ',
                  'type = \'trip\' using the same year and wave parameters.\n',
                  'trip frame: ', frameT, '\n',
                  'catch frame: ', frameC))
    }

  }

  agLst <- list()
  for(i in 1:length(common)){


    # Distribute claim among all trip members
    if(is.null(cdat)){
      dat <- tdat
    }else{

      cdatTemp <- filter(cdat, COMMON == common[i])

      dat <- left_join(x = tdat, y = select(cdatTemp, COMMON, ID_CODE, LANDING,
                                            CLAIM, RELEASE, HARVEST))

      # For trips with multiple participants where not all could be interviewed
      # or where the catch could not be separated out by individual the entire
      # claim (i.e., number of type A fish) may be attributed to just the leader.
      # This means that there are records in the database for individuals that
      # may have caught fish but where claim is zero or NA. The CLAIM2 column
      # is just the maximum claim spread out among all participants in a fishing
      # party. It should be considered binary.
      grClaim <- dat %>%
        group_by(LEADER) %>%
        summarize(claimMax = sum(CLAIM, na.rm=TRUE))

      dat$CLAIM2 <- grClaim$claimMax[match(dat$LEADER, grClaim$LEADER)]
    }


    p1c <- which(dat$PRIM1_COMMON == common[i])
    p2c <- which(dat$PRIM2_COMMON == common[i])

    if(is.null(cdat)){
      typA <- NA
      typB1 <- NA
      typB2 <- NA
    }else{
      typA <- which(dat$CLAIM2 > 0)
      typB1 <- which(dat$HARVEST > 0)
      typB2 <- which(dat$RELEASE > 0)
    }


    idxLst <- list(p1c, p2c, typA, typB1, typB2)
    incl <- c('PRIM1_COMMON' %in% type,
              'PRIM2_COMMON' %in% type,
              'A' %in% type,
              'B1' %in% type,
              'B2' %in% type)

    idx <- unique(unlist(idxLst[incl]))

    # ensure common name is recorded
    dat$COMMON <- common[i]


    # Define group order for sensical output order. Always include Common
    groupRef <- c('COMMON', 'YEAR', 'ST', 'WAVE', 'MODE_FX', 'AREA_X')
    if(!'COMMON' %in% group){
      group <- c('COMMON', group)
    }
    group <- group[order(match(group, groupRef))]

    agLst[[i]] <- group_by_at(dat[idx,], vars(all_of(group))) %>%
      summarise(E = agFun(WP_INT, na.rm = TRUE)) %>%
      ungroup()

  }

  ag <- bind_rows(agLst)

  return(ag)

}






