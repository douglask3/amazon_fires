library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/filename.noPath.r")
source("libs/writeRaster.Standard.r")

files = list.files('data/soilw/', full.names=TRUE)
mask_file = 'data/climate/climate_mask.nc'

soil_property_file = 'data/qrparm.soil.nc'

firstYr = 2000

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


MakeByStartYr <- function(firstYr = NULL) {
    if (!is.null(firstYr)){
        yrs = sapply(strsplit(files, '.', fixed = TRUE), function(i) tail(i, 2)[1])
        files = files[yrs >= firstYr]
    }
    
    soilw = layer.apply(files, makeMonthlySoilW)

    mask = raster(mask_file)
    soilw = raster::resample(soilw, mask)

    soil_porosity = raster(soil_property_file, varname = "soil_porosity")
    soil_porosity[soil_porosity > 9E9] = NaN
    soil_porosity = raster::resample(soil_porosity, mask)

    soil_wilting_point = raster(soil_property_file,
                                varname = "volume_fraction_of_condensed_water_in_soil_at_wilting_point")
    soil_wilting_point[soil_wilting_point > 9E9] = NaN
    soil_wilting_point = raster::resample(soil_wilting_point, mask)


    soilw = (soilw - soil_wilting_point)/(soil_porosity - soil_wilting_point)
    soilw[soilw>1] = 1
    soilw[soilw<0] = 0
    min_soilw = min.raster(soilw, na.rm = TRUE)
    soilw = (soilw - min_soilw)/(1 - min_soilw)

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

    file_out = paste0('outputs/',c('climate/', 'vegetation/MaxOverMean_'),
                        filesN[2], '-', yrs, '.nc')
    
    writeRaster.Standard(soilw    , file_out[1])
    writeRaster.Standard(soilw_max, file_out[2])
}

MakeByStartYr(firstYr)
MakeByStartYr()
