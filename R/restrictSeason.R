


#' Print variable definitions
#'
#' Print definitions for submited arguments if they exist. Arguments are
#' (case-insensitive) column names from the trip, catch or size data frames.
#' Documentation based on EXCEL workbook that can be found here:
#' <https://www.st.nmfs.noaa.gov/st1/recreational/MRIP Survey Data/>
#' under MRIP survey variables.xlsx
#'
#' @param cDat Catch data base containing harvest data (typically from
#'             bindMRIP).
#'
#' @param openDates Vectors indicating season length in the form of
#'                  season open to season close (MM-DD), and *not* including
#'                  year. For example, a season running from Jul 1 to Aug 15
#'                  would be c('07-01', '07-15'). Complex seasons with multiple
#'                  closures are possilbe and in these cases openDates must be
#'                  provided as a list, e.g.,
#'                  list(c('MM-DD', 'MM-DD'), c('MM-DD', 'MM-DD'))
#'
#' @param group Desired grouping for catch data such as MODE_FX, STATE, etc.
#'              If NULL, the default, only YEAR is used.
#'
#' @param common Common name of species, as defined in MRIP data. Case-sensiive.
#'               Common name may be a vector if multiple species are to be
#'               evaluated at the same time (for the same seasons).
#'
#' @param FUN Summarization function to be applied over multiple years of data.
#'            for example, if the average harvest is desired then FUN would be
#'            mean.
#'
#' @return printed output to the console defining the variable
#'
#' @examples
#'
#' # Print definition for single argument
#' printColDef('MODE_FX')
#'
#' # Print definition for multiple arguments (case-insensitive)
#' printColDef(c('Year', 'reLEase'))
#'
#' # Return when argument not found
#' printColDef('dummyArg')
#'
#' @export


restrictSeason <- function(cDat, openDates, group = NULL, common, fun){

  # Check to see whether each year in the data is longer than
  # dateStart to dateEnd

  if(class(openDates) == 'character'){
    if(length(openDates) != 2){
      stop(paste('restrictSeason: vector openDates should',
                 'be of length 2'))
    }else{
      openDates <- list(openDates)
    }
  }else if(class(openDates) == 'list'){
    lenCheck <- sapply(openDates, length)
    if(!all(lenCheck == 2)){
      stop(paste('restrictSeason: each element of openDates list should',
                 'be a vector of length 2'))
    }
  }else{
    stop('openDates should be of class list or a vector of length 2')
  }


  # If year is already included make sure it is 1st, otherwise add year
  if('YEAR' %in% group){
    group <- group[-which(group == 'YEAR')]
  }
  group <- c('YEAR', group)

  # Check to make sure only a single species is provided



  cDat <- mutate(cDat, DATE = get_date(dat = cDat))
                 # plotDateStart = as.Date(paste(YEAR, dateStart, sep='-')),
                 # plotDateEnd = as.Date(paste(YEAR, dateEnd, sep='-')))


  yrs <- sort(unique(cDat$YEAR))

  openLst <- lapply(openDates, function(x) data.frame(YEAR = yrs,
                                                      st0 = x[1],
                                                      nd0 = x[2]))
  openDF <- do.call(rbind, openLst)
  openDF$st <- with(openDF, as.Date(paste(YEAR, st0, sep='-')))
  openDF$nd <- with(openDF, as.Date(paste(YEAR, nd0, sep='-')))

  uCom <- unique(cDat$COMMON)
  plotList <- list()
  for(i in seq_along(uCom)){
    p <- cDat %>%
      filter(COMMON == uCom[i]) %>%
      group_by(YEAR) %>%
      ggplot(aes(DATE, fill = MODE_FX)) +
        geom_bar(stat = 'count', position = 'stack') +
        facet_wrap(~YEAR, scales = 'free', nc = 2) +
        geom_rect(data = openDF,
                  aes(xmin = st, xmax = nd,
                      fill = 'Open'), alpha = 0.75,
                  ymin=-Inf, ymax = 0,
                  inherit.aes = FALSE) +
        scale_fill_manual('Proposed Season',
                          values = 'cornflowerblue') +
      labs(title = uCom[i]) +
      theme_bw()

    print(p)
    plotList[[i]] <- p
  }



  dAsLst <- list()
  for(i in 1:length(openDates)){
    tmp <- list()
    for(j in 1:length(yrs)){
      tmp[[j]] <- as.character(paste(yrs[j], openDates[[i]], sep='-'))
    }
    dAsLst[[i]] <- do.call(rbind, tmp)
  }
  dAsDf <- do.call(rbind, dAsLst)

  ### Argument for release mortality


  get_subDate <- function(x) cDat$DATE > as.Date(x[1]) &
                               as.Date(cDat$DATE) < x[2]
  cond <- apply(dAsDf, 1, get_subDate)
  cDatAlt <- cDat[apply(cond, 1, sum) > 0,]


  # Get the estimates
  gc <- get_catch(cdat = cDat,
                  group = group,
                  common = common)

  gcAlt <- get_catch(cdat = cDatAlt,
                     group = group,
                     common = common)


  est <- gc %>%
    group_by_at(c('COMMON', group[-which(group == 'YEAR')])) %>%
    summarise_at(c('HN', 'RN', 'HKG', 'HLB'), mean)

  estAlt <- gcAlt %>%
    group_by_at(c('COMMON', group[-which(group == 'YEAR')])) %>%
    summarise_at(c('HN', 'RN', 'HKG', 'HLB'), mean)

  return(list(plot = p,
              data = list(data = gc,
                          altSeason = gcAlt),
              call = match.call(),
              est = list(data = est,
                         altSeason = estAlt)))

}







