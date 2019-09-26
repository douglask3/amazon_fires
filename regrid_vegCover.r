library(raster)
library(rasterExtras)
source("libs/writeRaster.Standard.r")

mask_file = 'data/climate/climate_mask.nc'

dirs = c(treecover = 'data/treecover/',
         nonetreecover = 'data/nontree/')

output_dir = 'outputs/'

mask = raster(mask_file)
regrid <- function(dir, nm) {
    files = list.files(dir, full.names = TRUE) 
    openMaskFile <- function(file) {
        dat = raster(file)
        dat = convert_regular_2_pacific_centric(dat)
        dat = raster::resample(dat, mask)
    }
    dat = lapply(files, openMaskFile)
    
    interpolate <- function(d1, d2) {
        interpolateMonth <- function(mn)
            (d1 * (12-mn) + d1 * mn)/12
        
        dat = layer.apply(1:12, interpolateMonth)
    }
    dat = mapply(interpolate, head(dat, -1), dat[-1])
    dat = layer.apply(dat, function(i) i)
    dat = dat[[-(1:6)]]
    
    yrs = sapply(files, function(file) tail(strsplit(file, '.', fixed = TRUE)[[1]], 2)[1])
    fname = paste0(output_dir, nm, '-', min(as.numeric(yrs))+1, '-', 'June', max(yrs), '.nc')
    writeRaster.Standard(dat, fname)
    return(fname)
}

files = mapply(regrid, dirs, names(dirs))

dat = brick(files[1]) + brick(files[2])
fname = paste0(output_dir, 'vegCover-', paste0(tail(strsplit(files[1], '-')[[1]], 2), collapse = '-'))
writeRaster.Standard(dat, fname)
