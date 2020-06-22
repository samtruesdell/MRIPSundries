
library(MRIPSundries)
library(msm)


IDCodeFun <- function(y, w){
  x1 <- paste(sample(0:9, 5, replace = TRUE), collapse = "")
  m <- sprintf("%02d", sample(c(w*2, w*2-1), 1))

  switch <- 0
  while(switch == 0){
    d <- sprintf("%02d", sample(1:31, 1))
    testDate <- suppressWarnings(lubridate::ymd(paste(y, m, d, sep = '-')))
    if(!is.na(testDate)){
      switch <- 1
    }
  }
  x2 <- paste0(y, m, d)
  x3 <- paste(sample(0:9, 3, replace = TRUE), collapse = "")
  return(paste0(x1, x2, x3))
}



yr <- 2050:2052
wv <- 4:5
cm <- c('BLUEFISH', 'BLACK SEA BASS')
st <- c(25, 44)
md <- c(3,4,5,7)

n <- 5
targ <- c(cm, 'SCUP', 'SUMMER FLOUNDER')

csvLst <- list()
cnt <- 1

for(i in seq_along(yr)){
  for(j in seq_along(wv)){
    tempLstCat <- list()
    tempLstTrp <- list()
    tempLstSiz <- list()
    tempCnt <- 1
    for(k in seq_along(cm)){
      for(l in seq_along(st)){
        for(m in seq_along(md)){

          ctemp <- data.frame(
            COMMON = cm[k],
            YEAR = yr[i],
            ST = st[l],
            MODE_FX = md[m],
            ID_CODE = replicate(n, IDCodeFun(yr[i], wv[j])),
            PSU_ID = paste0(yr[i], wv[j], st[l], 'X', md[m], md[m]),
            WAVE = wv[j],
            CLAIM = rpois(n, 3),
            RELEASE = rpois(n, 4),
            HARVEST = rpois(n, 2),
            WP_CATCH = rlnorm(n, 5, 1)
          )
          ctemp$LANDING <- with(ctemp, CLAIM + HARVEST)
          ctemp$WGT_AB1 <- with(ctemp, CLAIM + HARVEST) * 1.25

          tempLstCat[[tempCnt]] <- ctemp

          # Trip data
          ttemp <- data.frame(
            PRIM2_COMMON = sample(targ, size = n, replace = TRUE),
            PRIM1_COMMON = sample(targ, size = n, replace = TRUE),
            YEAR = yr[i],
            ST = st[l],
            MODE_FX = md[m],
            ID_CODE = ctemp$ID_CODE,
            ASG_CODE = substr(ctemp$ID_CODE, start = 1, stop = 13),
            PSU_ID = ctemp$PSU_ID,
            LEADER = replicate(n, paste(sample(0:9, 16, replace = TRUE),
                                     collapse = "")),
            WAVE = wv[j])
          ttemp$WP_INT <- ctemp$WP_CATCH

          tempLstTrp[[tempCnt]] <- ttemp

          # Size data
          idx <- rep(1:nrow(ctemp), times = ctemp$CLAIM)
          cls <- c('COMMON', 'YEAR', 'ST', 'MODE_FX', 'ID_CODE',
                   'PSU_ID', 'WAVE')
          stemp <- ctemp[idx,cls]
          stemp$WP_SIZE <- ctemp$WP_CATCH[idx] / ctemp$CLAIM[idx]
          stemp$LENGTH <- runif(1:nrow(stemp), 25, 65)
          stemp$WGT <- 0.00001 * stemp$LENGTH^3
          stemp$L_IN_BIN <- floor(stemp$LENGTH / 2.54)

          tempLstSiz[[tempCnt]] <- stemp
          tempCnt <- tempCnt + 1
        }
      }
    }
    csvLst[[cnt]] <- do.call(rbind, tempLstCat)
      names(csvLst)[cnt] <- paste0('catch_', yr[i], wv[j])
    csvLst[[cnt+1]] <- do.call(rbind, tempLstTrp)
      names(csvLst)[cnt+1] <- paste0('trip_', yr[i], wv[j])
    csvLst[[cnt+2]] <- do.call(rbind, tempLstSiz)
      names(csvLst)[cnt+2] <- paste0('size_', yr[i], wv[j])
    cnt <- cnt + 3
  }
}



cv <- 0.3

catDat <- csvLst[seq(1, length(csvLst), by = 3)]

catEstLst <- list()
for(i in 1:length(catDat)){
  catDatTmp <- catDat[[i]]
  attributes(catDatTmp)$bindMRIPFrame <- 'xxxxxcatch'
  catEstLst[[i]] <- get_catch(catDatTmp,
            group = c('YEAR', 'ST', 'WAVE', 'MODE_FX'),
            common = 'BLACK SEA BASS') %>%
    rename(year = YEAR, st = ST, wave = WAVE, mode_fx = MODE_FX,
           landing = HN, estrel = RN, common = COMMON) %>%
    mutate(status = 'FINAL',
           # Something for the variance that is at least on the right scale
           land_var = rtnorm(n(), mean = landing, sd = cv * landing)^2,
           estrelvar = rtnorm(n(), mean = estrel, sd = cv * estrel)^2,
           tot_cat = landing + estrel) %>%
    select(status, year, wave, st, common, mode_fx, landing, land_var, estrel,
           estrelvar, tot_cat)
}
catEst <- bind_rows(catEstLst)





# MRIP estimate trip data
tripDat <- csvLst[seq(2, length(csvLst), by = 3)]

tripEstLst <- list()
for(i in 1:length(tripDat)){
  tripDatTmp <- tripDat[[i]]
  attributes(tripDatTmp)$bindMRIPFrame <- 'type[trip]_x_x'
  catDatTmp <- catDat[[i]]
  attributes(catDatTmp)$bindMRIPFrame <- 'type[catch]_x_x'
  tripEstLst[[i]] <- get_effort(tdat = tripDatTmp,
                                cdat = catDatTmp,
                                group = c('YEAR', 'ST', 'WAVE', 'MODE_FX'),
                                common = 'BLACK SEA BASS',
                                type = c("PRIM1_COMMON", "PRIM2_COMMON", "A",
                                         "B1", "B2")) %>%
    rename(year = YEAR, st = ST, wave = WAVE, mode_fx = MODE_FX) %>%
    mutate(status = 'FINAL',
           estrips = rlnorm(n(), mean = log(2e3), sd = 2),
           # Something for the variance that is at least on the right scale
           NUMVAR = rtnorm(n(), mean = estrips, sd = cv * estrips)^2) %>%
    select(status, mode_fx, year, wave, st, estrips, NUMVAR)
}
tripEst <- bind_rows(catEstLst)



dir.create('vignettes/vinData')
dir.create('vignettes/vinData/mrip_est')
dir.create('vignettes/vinData/mrip_raw')
mapply(write.csv, x = csvLst,
       file = file.path('vignettes/vinData/mrip_raw',
                        paste0(names(csvLst), '.csv')),
       row.names = FALSE)
write_csv(catEst, path = file.path('vignettes/vinData/mrip_est',
                                   paste0('mrip_catch_wave_',
                                   min(yr), '_', max(yr), '.csv')))
write_csv(tripEst, path = file.path('vignettes/vinData',
                                    paste0('mrip_effort_',
                                    min(yr), '_', max(yr), '.csv')))



