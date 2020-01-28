library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/filename.noPath.r")
source("libs/writeRaster.Standard.r")

dat = brick('data/precip.mon.mean.nc')

dir = 'outputs/climate/from_2001/'

startLayer = "X2001.01.01"

calLag <- function(lag, dat, dat0, dir, fname) {
    memSafeFile.initialise('temp/')
        cal <- function(i) {
            print(i)
            out = sum(dat[[(i-lag+1):i]])
            out = writeRaster(out, filename = memSafeFile(), overwrite = TRUE)
            return(out)
        }

        spoint = which(startLayer == names(dat))
        out = layer.apply(spoint:nlayers(dat), cal)
        if(!is.null(dat0)) out = out/dat0

        fname = paste0(dir, fname)
        out = writeRaster.Standard(out, fname)
    memSafeFile.remove()
    return(out)
}

dat120 = calLag(120, dat, NULL, dir, 'precip_tenyrLag.2001-2019.nc')
dat12 = calLag(12, dat, dat120, dir, 'precip_yrLag.2001-2019.nc')
dat36 = calLag(36, dat, dat120,dir, 'precip_threeyrLag.2001-2019.nc')
