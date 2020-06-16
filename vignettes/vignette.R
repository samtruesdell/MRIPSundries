## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(MRIPSundries)

## -----------------------------------------------------------------------------
fl <- list.files('mripDat', full.names = TRUE)
print(fl)

## ----message=FALSE------------------------------------------------------------
# Bind together catch data
mDatC <- bindMRIP(dir = 'mripDat',  # Directory holding the .csv files
                 type = 'catch')    # Type of .csv files to bind together

## -----------------------------------------------------------------------------
get_dataStats(mDatC)

## -----------------------------------------------------------------------------
names(mDatC)

## -----------------------------------------------------------------------------
printColDef(colName = c('Release', 'MODE_FX'))

## -----------------------------------------------------------------------------
catY <- get_catch(mDatC,                # data set from bindMRIP()
                  group = c('YEAR'),    # structure of output
                  common = 'BLUEFISH')  # species
knitr::kable(catY, digits=0)            # print a nice table

## -----------------------------------------------------------------------------
catYM <- get_catch(mDatC,                         # Data from bindMRIP()
                   group = c('YEAR', 'MODE_FX'),  # columns to group by
                   common = 'BLUEFISH')           # species
knitr::kable(catYM, digits=0)                     # print a nice table

## -----------------------------------------------------------------------------
# Bind together catch data
mDatS <- bindMRIP(dir = 'mripDat',  # Directory holding the .csv files
                 type = 'size')     # Type of file to bind together

