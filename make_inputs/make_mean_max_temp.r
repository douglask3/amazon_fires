library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/filename.noPath.r")
source("libs/writeRaster.Standard.r")

files = list.files('data/temp_4_daily/', full.names=TRUE)
mask_file = 'data/climate/climate_mask.nc'

file_out = "outputs/climate/tasMax_2001-2019.nc"

makeMonthlyTas <- function(file) {
    dat = brick(file)
    month = as.numeric(sapply(names(dat), function(i) strsplit(i, '.', fixed = TRUE)[[1]][2]))

    meanMonthMax <- function(mnth) {
        file_temp = paste0('temp/', filename.noPath(file, noExtension = TRUE),
                           '-meanMax-', mnth, '.nc')
        if (file.exists(file_temp)) out = raster(file_temp)
        else {
            index = which(month == mnth)
            if (length(index) == 0) {
                if (file == tail(files, 1)) return(NULL)
                else {
                    print("missing months")
                    browser()
                } 
            } 
            dindex = matrix(index, nrow = 4)
            out = layer.apply(apply(dindex, 2, function(i) max(dat[[i]])), function(i) i)
            out = mean(out)
            out = writeRaster(out, file_temp, overwrite = TRUE)
        }
        return(out)
    }
    layer.apply(1:12, meanMonthMax)
}

tmax = layer.apply(files, makeMonthlyTas)

mask = raster(mask_file)
tmax = raster::resample(tmax, mask)


writeRaster.Standard(tmax    , file_out)

