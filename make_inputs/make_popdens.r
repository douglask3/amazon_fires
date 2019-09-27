library(raster)
library(rasterExtras)
source("libs/writeRaster.Standard.r")
source("libs/interpoloateAnnual2Monthly.r")

dir = 'data/HYDE_PD/'

mask_file = 'data/climate/climate_mask.nc'

mask = raster(mask_file)

open_and_regrid_file <- function(file) {
    dat = raster(file)
    dat = raster::resample(dat, mask)
    return(dat)
}

files = list.files(dir, full.names = TRUE)
dat = lapply(files, open_and_regrid_file)
dat = interpolateAnnual2Monthly(dat)
dat = dat[[-(1:6)]]
writeRaster.Standard(dat, 'outputs/human/PD_HYDEv3.2_2001-2018.nc')
