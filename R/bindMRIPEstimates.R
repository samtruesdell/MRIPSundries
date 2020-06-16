


#' Bind together precompiled MRIP data estimates
#'
#' Reads MRIP *estimate* csv files from a directory and binds them together into a single
#' data frame.
#'
#' @param dir Directory containing mrip estimate files. Note that these are not
#'            the raw data but precompiled estimates. This function is
#'            recursive; in other words it will search through all sub-folders
#'            in the given directory and try to merge all .csv files. Function
#'            is designed to work only with a directory downloaded directly
#'            from the MRIP website (i.e., csv files from
#'            <https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/>.
#' @param type Type of data to bind together: one of "trip", "catch" or "size."
#' @param years Vector of years to include in the output data frame.
#'
#' @return Data frame derived from compiled MRIP catch and/or effort estimates
#'
#' @examples
#' bindMRIPEstimates(dir = './data',             # path to folder containing .csv files
#'                   type = 'catch',             # specifies catch data sets
#'                   years = 2014:2017)          # years to include
#'
#' @export



bindMRIPEstimates <- function(dir, type = c('catch', 'effort'),
                     years = NULL){


  # List all files in the given directory
  fl <- list.files(dir, full.names = TRUE, recursive = TRUE, pattern = '.csv')
  flName <- basename(fl)

  if(!all(str_detect(flName, 'mrip_catch_wave|mrip_catch_year|mrip_effort'))){
    stop(paste('some files in your directory do not follow the named syntax',
               'mrip_catch_wave, mrip_catch_year or mrip_effort. Please only',
               'include those types of files. You can download the directory',
               'from the MRIP website and point to it as-is.'))
  }

  if(is.null(years) & 'catch' %in% type){

    idC <- grep(pattern = 'catch', x = fl, value = FALSE)
    matC <- str_split(string = flName[idC], pattern = '_|.csv', simplify = TRUE)
    yC1 <- as.numeric(matC[,ncol(matC)-2])
    yC2 <- as.numeric(matC[,ncol(matC)-1])

    yearsC <- min(yC1, yC2, na.rm = TRUE):max(yC1, yC2, na.rm = TRUE)

    if(all(type == 'catch')){
      years <- yearsC
    }

  }

  if(is.null(years) & 'effort' %in% type){

    idE <- grep(pattern = 'effort', x = fl, value = FALSE)
    matE <- str_split(string = flName[idE], pattern = '_|.csv', simplify = TRUE)
    yE1 <- as.numeric(matE[,ncol(matE)-2])
    yE2 <- as.numeric(matE[,ncol(matE)-1])

    yearsE <- min(yE1, yE2, na.rm = TRUE):max(yE1, yE2, na.rm = TRUE)

    if(all(type == 'effort')){
      years <- yearsE
    }

  }

  if(is.null(years) & all(c('catch', 'effort') %in% type)){
    if(!all(yearsC == yearsE)){
      stop(paste('check that year ranges in catch and effort data in the',
                 'data files are the same'))
    }
  }

  if(is.null(years) & all(c('catch', 'effort') %in% type)){
    years <- yearsC  # yearsC == yearsE in this scenario
  }


  getFileIdx <- function(pat){

    id <- grep(pattern = pat, x = fl, value = FALSE)
    mat <- str_split(string = flName[id], pattern = '_|.csv', simplify = TRUE)
    blank <- which(mat[,ncol(mat)-1] == '')
    mat[blank,ncol(mat)-1] <- mat[blank,ncol(mat)-2]
    minYr <- as.numeric(mat[,ncol(mat)-2])
    maxYr <- as.numeric(mat[,ncol(mat)-1])
    wmin <- which(min(years) >= minYr)
    wmax <- which(max(years) <= maxYr)
    flIdx <- max(wmin):min(wmax)
    return(fl[id][flIdx])

  }



  if('catch' %in% type){
    catFlIdx <- getFileIdx(pat = 'catch_wave')
    cat <- catFlIdx %>%
      map_dfr(read_csv, col_types = cols()) %>%
      filter(year %in% years)
  }

  if('effort' %in% type){
    effFlIdx <- getFileIdx('effort')
    eff <- effFlIdx %>%
      map_dfr(read_csv, col_types = cols(status = 'c')) %>%
      filter(year %in% years)
  }

  if(all(type == 'catch')){

    ret <- cat

  }else if(all(type == 'effort')){

    ret <- eff

  }else if(all(type %in% c('catch', 'effort'))){

    ret <- left_join(eff, cat, by = c('year', 'wave', 'sub_reg', 'st',
                                      'mode_fx', 'area_x'),
                     suffix = c('_effort', '_catch'))

  }else{

    stop('type argument should be one or more of: c(\'catch\', \'effort\')')

  }


  att <- paste(paste0('type[', type, ']'),
               paste0('years[', paste(sort(years), collapse = '-'), ']'),
               sep = '_')

  return(ret)


}


