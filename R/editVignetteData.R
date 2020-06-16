


mripPth <- 'C:/Users/samuel.truesdell/Downloads/ps_2015_2017_csv'

fl <- list.files(mripPth, full.names = TRUE, pattern = '.csv')

dir.create(file.path(mripPth, 'newCSVs'))


red <- function(x){

  d <- read.csv(file = x, header = TRUE, stringsAsFactors = FALSE)

  flName <- basename(x)
  wUnder <- regexpr('_', flName)
  typ <- substr(flName, start = 1, stop = wUnder-1)

  if(typ %in% c('catch', 'size')){

    upd <- subset(d, common %in% c('ATLANTIC COD', 'BLUEFISH') &
                     ST %in% c(25, 44))

  }else{

    upd <- subset(d, ST %in% c(25, 44))

  }

  write.csv(upd, file = file.path(mripPth, 'newCSVs', flName),
            row.names = FALSE)

}


lapply(fl, red)

