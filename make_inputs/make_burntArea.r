################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
library(raster)
source("libs/convert_pacific_centric_2_regular.r")
library(rhdf5)
source("libs/writeRaster.Standard.r")

dir = 'data/GFED4s/'

mask_file = 'data/climate/climate_mask.nc'

out_file = 'outputs/GFED4s_2.5degree_2000-2016.nc'

files = list.files(dir, full.names = TRUE)
files = files[grepl('GFED4.1s_', files)]
fileDate = c()

mask = raster(mask_file)

openHDFandConvert2Nc <- function(month, fname) {
    if (month < 10) month = paste('0', month, sep = '')
    print(fname)
    layer = paste('burned_area', month, 'burned_fraction', sep = '/')
    dat = h5read(fname, layer)
    dat = raster(t(dat))

    extent(dat) = extent(c(-180, 180, -90, 90))
    dat = convert_regular_2_pacific_centric(dat)
    dat = raster::resample(dat, mask)
   
    H5close()
	
    writeRaster(dat, file = memSafeFile(), overwrite = TRUE)
    
    fDate = file.info(fname)$ctime
    fDate = as.character(fDate)
    names(fDate) = fname	
    fileDate <<- c(fileDate, fDate)
	
    return(dat)
}

memSafeFile.initialise('temp/')
dat = layer.apply(files, function(...)
		  layer.apply(1:12, openHDFandConvert2Nc, ...))

				 
names(fileDate) = paste(names(fileDate), 'obtained on')

comment = list('Data from GFEDv4.1s' = 'see https://www.globalfiredata.org/data.html',
	       'Data obtained on' = fileDate)
   
writeRaster.Standard(dat, out_file, comment = comment)
					
memSafeFile.remove()
