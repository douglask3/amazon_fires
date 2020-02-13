################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
library(raster)
library(rasterExtras)
source("make_inputs/fuel_moisture_equilibrium.r")
source("libs/writeInput.r")

## paths and parameters
dir = 'data/precip_daily/'
mask_file = 'data/climate/climate_mask.nc'

dir_out   = 'outputs/climate/'
fname_out = c('wetdays-', 'precip-')

Start_yr = c(2001,1979)

subArea = extent(c(100, 170, -50, 0))

################################################################################
## load data                                                                  ##
################################################################################

files = list.files(dir, full.names = TRUE)

findMnthWD <- function(mn, dat, mnths) {
    print(mn)
    mean(dat[[which(mn == mnths)]] >= 2.8)
}

findMnthPR <- function(mn, dat, mnths) {
    print(mn)
    mean(dat[[which(mn == mnths)]])
}
mask = raster(mask_file)
makeData4Start_year <- function(syr) {
    findVar <- function(file, vFUN = findMnthWD) {
        dat = brick(file)
        dat = raster::crop(dat, subArea)
        mnths = sapply(names(dat), function(i) strsplit(i, '.', fixed = TRUE)[[1]][2])
        
        out = layer.apply(unique(mnths), vFUN, dat, mnths)
        out = raster::resample(out, mask)
        return(out)
    }
    years = sapply(files, function(i) tail(strsplit(i, '.', fixed = TRUE)[[1]],2)[1])
    test = years >= syr
    files = files[test]; years = years[test]

    wd = layer.apply(files, findVar)
    pr = layer.apply(files, findVar, vFUN = findMnthPR)

    ################################################################################
    ## output                                                                     ##
    ################################################################################
    
    writeInput(wd, dir_out, fname_out[1], years)
    writeInput(pr, dir_out, fname_out[2], years)
}#

lapply(Start_yr, makeData4Start_year)
