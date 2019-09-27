library(raster)
source("libs/writeRaster.Standard.r")
source("libs/convert_pacific_centric_2_regular.r")

extent = extent(c(-85, -30, -30, 13))

dirs = list.dirs('outputs/')
dirs = dirs[!grepl('_region', dirs)]

regridFiles <- function(dir) {
    files_in  = list.files(dir, full.names = TRUE)
    files_in  = files_in[grepl('.nc', files_in)]

    files_out = strsplit(files_in, '//', fixed = TRUE)
    files_out = sapply(files_out, paste0, collapse = "/amazon_region/")
    
    regridFile <- function(file_in, file_out) { 
        print(file_in)
        print(file_out)
        
        r = r0 = brick(file_in)
        r = convert_pacific_centric_2_regular(r)
        r = raster::crop(r, extent)
        
        rmask = all(is.na(r)) | any(r > 9E9)
        if (is.null(mask)) mask <<- rmask
        else mask <<- mask | rmask
        writeRaster.Standard(r, file_out)
    }
    mapply(regridFile, files_in, files_out)
    return(files_out)
}

mask = NULL
files_out = unlist(sapply(dirs, regridFiles))

addMask <- function(file) {
    print(file)
    r = brick(file) 
    r[mask] = NaN
    writeRaster.Standard(r, file)
}

lapply(files_out, addMask)
