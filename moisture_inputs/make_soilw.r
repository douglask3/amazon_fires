library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/filename.noPath.r")

files = list.files('data/soilw/', full.names=TRUE)

makeMonthlySoilW <- function(file) {
    dat = brick(file)
    month = as.numeric(sapply(names(dat), function(i) strsplit(i, '.', fixed = TRUE)[[1]][2]))

    meanMonth <- function(mnth) {
        file_temp = paste0('temp/', filename.noPath(file, noExtension = TRUE), '-', mnth, '.nc')
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
            out = mean(dat[[index]])
            out = writeRaster(out, file_temp, overwrite = TRUE)
        }
        return(out)
    }
    layer.apply(1:12, meanMonth)
}
files = files
soilw = layer.apply(files, makeMonthlySoilW)


seaCy12 <- function(r) {	
    Cy12 <- function(mnth) {
	subr = r[[(mnth-11):mnth]]
	return(max(subr) / mean(subr))
    }
    return(layer.apply(13:nlayers(r), Cy12))
}

soilw_max = seaCy12(soilw)
soilw = soilw[[13:nlayers(soilw)]]

filesN = sapply(files, filename.noPath, noExtension=TRUE)
yrs = sapply(filesN, function(file) tail(strsplit(file, '.', fixed = TRUE)[[1]],1))
yrs = max(yrs)

file_out = paste0('outputs/',c('', 'MaxOverMean_'),filesN[1], '-', yrs, '.nc')

writeRaster.gitInfo(soilw    , file_out[1], overwrite = TRUE)
writeRaster.gitInfo(soilw_max, file_out[2], overwrite = TRUE)
