

#' Bind together MRIP data
#'
#' Reads MRIP csv files from a directory and binds them together into a single
#' data frame.
#'
#' @param dir Directory containing mrip files. This function is recursive; in
#'            other words it will search through all sub-folders in the given
#'            directory and try to merge all .csv files. Function is designed
#'            to work only with a directory downloaded directly from the MRIP
#'            website (i.e., csv files from
#'            <https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/>.
#' @param type Type of data to bind together: one of "trip", "catch" or "size."
#' @param years Vector of years to include in the output data frame.
#' @param waves Vector of waves to include in the output data frame (1-6).
#' @param common Vector of common names to include in the output data frame.
#'
#' @return Data frame derived from trip, catch or size .csv files
#'
#' @examples
#' bindMRIP(dir = './data',             # path to folder containing .csv files
#'          type = 'catch',             # specifies catch data sets
#'          years = 2014:2017,          # years to include
#'          waves = NULL,               # NULL includes all waves
#'          common = 'BLACK SEA BASS')  # species to include (watch spelling)
#'
#' @export



bindMRIP <- function(dir, type = c('trip', 'catch', 'size'),
                     years = NULL, waves = NULL, common = NULL){

  if(!type %in% c('trip', 'catch', 'size')){
    stop('type must be one of trip, catch or size')
  }

  if(length(type) != 1){
    stop('select *1* of trip, catch or size for <type>')
  }

  if(type == 'trip' & !is.null(common)){
    stop(paste('trip data do not contain species-specific information:',
               '<common> argument must be left blank'))
  }

  # save the function arguments to return
  funArgs <- list(dir = dir,
                  type = type,
                  years = years,
                  waves = waves,
                  common = common)

  # List all files in the given directory
  fl <- list.files(dir, full.names = TRUE, recursive = TRUE, pattern = '.csv')
  flName <- basename(fl)

  # Exclude 1981 as there was no information during Wave 1
  w1981 <- grep(pattern = '1981', x = flName)
  if(length(w1981) > 0){
    fl <- fl[-w1981]
    flName <- flName[-w1981]
  }

  # identify file names that match pattern
  setPath <- grep(pattern = type, x = fl, value = TRUE)

  wUnder <- regexpr('_', flName)
  fYear <- substr(x = flName,
                  start = wUnder + 1,
                  stop = wUnder + 4)
  fWave <- substr(x = flName,
                  start = wUnder + 5,
                  stop = wUnder + 5)
  fType <- substr(x = flName,
                  start = 1,
                  stop = wUnder-1)

  if(is.null(years)){
    years <- fYear
  }

  if(is.null(waves)){
    waves <- fWave
  }

  fIdx <- (fYear %in% years) & (fWave %in% waves) & (fType %in% type)

    # read in the data
  # csvList <- lapply(fl[fIdx], read.csv,
  #                   header = TRUE, stringsAsFactors = FALSE)
  csvList <- lapply(fl[fIdx], read_csv, col_types = cols(), progress = FALSE)


  # structure of names changed over time ... capitilize names
  # for consistency
  for(i in 1:length(csvList)){
    names(csvList[[i]]) <- toupper(names(csvList[[i]]))
  }

  # Determine the column names in common for all csv files
  allNames <- Reduce(intersect, lapply(csvList, names))

  nc <- sapply(csvList, ncol)
  if(!all(nc == length(allNames))){
    warning('note: some columns removed due to incompatible names')
  }

  # Extract common columns
  csvListInt <- lapply(csvList, '[', allNames)

  # Bind together the files
  df <- do.call(rbind, csvListInt)

  # If common name specified, subset by those species
  if(!is.null(common)){

    df <- subset(df, COMMON %in% common)

  }

  # some columns should be character
  spCurrent <- options('scipen')
  options(scipen = 999)

  df$ID_CODE <- as.character(df$ID_CODE)
  df$PSU_ID <- as.character(df$PSU_ID)
  if(type == 'trip'){
    df$ASG_CODE <- as.character(df$ASG_CODE)
  }

  options(scipen = spCurrent$scipen)

  # Set the attributes of the return to represent what was in the call
  funArgs <- list(dir = dir,
                  type = type,
                  years = years,
                  waves = waves,
                  common = common)

  att <- paste(paste0('type[', type, ']'),
               paste0('years[', paste(sort(years), collapse = '-'), ']'),
               paste0('waves[', paste(sort(waves), collapse = '-'), ']'),
               paste0('common[', paste(sort(common), collapse = '-'), ']'),
               sep = '_')

  attributes(df)$bindMRIPFrame <- att

  return(df)

}






