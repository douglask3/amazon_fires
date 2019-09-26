################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
library(raster)
source("moisture_inputs/fuel_moisture_equilibrium.r")
source("libs/writeRaster.Standard.r")

## paths and parameters
files = paste0('data/', c('precip.mon.mean.nc', 'rhum.mon.mean.nc', 'air.mon.mean.nc'))
mask_file = 'data/climate/climate_mask.nc'

fname_out = 'outputs/emc_'

################################################################################
## load data                                                                  ##
################################################################################
loadDat <- function(file) {
    dat = brick(file)
    index =  which(sapply(strsplit(names(dat), '.', fixed = TRUE), substr, 2, 5)[1,] >= 2001)
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
fname = paste0(fname_out, paste(range(years), collapse = '-'), '.nc')
writeRaster.Standard(emc, fname)
