


#' Print data set information
#'
#' Prints information about the MRIP data set given in the argument
#'
#' @param mripData Data set created with a call to bindMRIP().
#'
#' @return Console output describing years, species, modes and waves contained
#'         in the data set.
#'
#' @examples
#' get_dataStats(mdat)  # where mdat was created from bindMRIP()
#'
#' @export




get_dataStats <- function(mripData){

  y <- sort(unique(mripData$YEAR))
  w <- sort(unique(mripData$WAVE))
  m <- sort(unique(mripData$MODE_FX))
  st <- sort(unique(mripData$ST))

  ns <- length(unique(mripData$COMMON))

  if(ns == 1){
    spOut <- paste0('(', unique(mripData$COMMON), ')')
  }else{
    spOut <- NULL
  }

  cat('MRIP data set contains:\n')
  cat('Species\n', ns, 'distinct species', spOut, '\n')
  cat('States\n', st, '\n')
  cat('Mode_FX\n', m, '\n')

  u <- unique(mripData[,c('YEAR', 'WAVE')])

  p <- pivot_wider(u, names_from = WAVE, values_from = WAVE,
                   names_prefix = 'wave')
  p <- as.data.frame(p)
  p[,2:ncol(p)][!is.na(p[,2:ncol(p)])] <- 'x'
  p[is.na(p)] <- '-'
  p <- p[,c(1, order(names(p)[2:ncol(p)] )+1 )]

  cat('Waves in MRIP data set')
  print(knitr::kable(p, align = 'c'))

}





