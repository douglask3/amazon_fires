################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
library(raster)
library(rasterExtras)
source("make_inputs/fuel_moisture_equilibrium.r")
source("libs/writeInput.r")

## paths and parameters
files = paste0('data/', c('precip.mon.mean.nc', 'rhum.mon.mean.nc', 'air.mon.mean.nc'))
mask_file = 'data/climate/climate_mask.nc'

dir_out   = 'outputs/climate/'
fname_out = 'emc-'

Start_yr = c(2001,1948)

################################################################################
## load data                                                                  ##
################################################################################
makeData4Start_year <- function(syr) {
    loadDat <- function(file) {
        dat = brick(file)
        index =  which(sapply(strsplit(names(dat), '.', fixed = TRUE), substr, 2, 5)[1,] >= syr)
        return(dat[[index]])
    }

    dat = lapply(files, loadDat)
    


    ################################################################################
    ##   make emc                                                                 ##
    ################################################################################

    make_emc <- function(i)
        fuel_moisture_equilibrium(0, dat[[2]][[i]], dat[[3]][[i]])   


    emc = layer.apply(1:nlayers(dat[[1]]), make_emc)
    mask = raster(mask_file)

    emc = raster::resample(emc, mask)
    ################################################################################
    ## output                                                                     ##
    ################################################################################
    years =  sapply(strsplit(names(dat[[1]]), '.', fixed = TRUE), substr, 2, 5)[1,]
    
    writeInput(emc, dir_out, fname_out, years)
    
}#

lapply(Start_yr, makeData4Start_year)
