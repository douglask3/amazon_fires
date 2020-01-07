library("raster")
library(snow)
library(ncdf4)
source("libs/writeRaster.Standard.r")

fileName = "data/DL_FIRE_M6_95447/fire_archive_M6_95447.csv"
years = 2001:2019
nlines_per_step = 10000

file_grid = "data/air.mon.mean.nc"
mask_file = "data/climate/climate_mask.nc"

extent = extent(c(360-85, 360-30, -33, 15))

processFile <- function(file, nlines_per_step, rv, lonlat2.5) {
    print(file)   
    
    nlines = as.numeric(strsplit(system(paste('wc -l', file), TRUE), ' data')[[1]][1])
    cnames = colnames(read.csv(file, nrows = 1, skip = 0))
    
    readLines <- function(i, nlines_per_step, lonlat2.5, rv, years) {
        source("libs/filename.noPath.r")
        print(i/ceiling(nlines/nlines_per_step))     
        skip = nlines_per_step * (i-1)
        
        if ((skip + nlines_per_step) > nlines) nrows = nlines - skip
        else nrows = nlines_per_step
        
        temp_file = paste0('temp/', filename.noPath(file, noExtension = TRUE),
                           '_lines-', nlines_per_step, '-', nlines, '-', skip, '-', nrows,
                           '.Rd')
        
        if (file.exists(temp_file)) load(temp_file)
        else {
            dat   = read.csv(file, skip = skip, nrows = nrows, stringsAsFactors = FALSE)
            
            dates = strsplit(dat[, cnames == "acq_date"], '-', fixed = TRUE)
            mnths = as.numeric(sapply(dates, function(i) i[[2]]))
            yrs   = as.numeric(sapply(dates, function(i) i[[1]])) - min(years) + 1
            
            if (any(mnths > 12)) browser()
            lons0 = lons = dat[, cnames == "longitude"]
            lons[lons < 0] = 360 + lons[lons<0]
            lons = round(lons/2.5)        
            lats = round(dat[, cnames == "latitude" ]/2.5)

            conf = dat[, cnames == "confidence"]
        
            add2grid <- function(lon, lat, yr, mnth, conf, rv) {  
                cell = which(lonlat2.5[,1] == lon & lonlat2.5[,2] == lat)
                #layer = which(months == mnth)
                
                if (length(cell) == 0) return(rv)
                else if (length(cell) > 1) {
                    print("more than one")
                }                
                rv[[yr]][cell, mnth] = rv[[yr]][cell, mnth] + conf         
                return(rv)
            }
        
            for (i in 1:length(lons)) 
                rv = add2grid(lons[i], lats[i], yrs[i], mnths[i], conf[i], rv)
            
            save(rv, file = temp_file)
        }            
        gc()          
        return(rv)
    }
    #rout = lapply(1:10, readLines, nlines_per_step, lonlat2.5, rv)
    
    cl = makeSOCKcluster(c("localhost", "localhost", "localhost", "localhost", "localhost", "localhost", "localhost"))
        rout = parLapply(cl, 1:ceiling(nlines/nlines_per_step), readLines,
                         nlines_per_step, lonlat2.5, rv, years)
    stopCluster(cl)
    
    for (ri in rout) for (yr in 1:length(rv))
        rv[[yr]] = rv[[yr]] + ri[[yr]]
    
    gc()
    return(rv)
}


r = brick(file_grid)
r = r[[637:nlayers(r)]]

r[] = 0

rv = rep(list(values(r[[1:12]])), length(years))

lonlat2.5 = xyFromCell(r[[1]], 1:length(r[[1]]))/2.5

#files = list.files(dir_nr, full.names = TRUE)
#files = files[grepl('.csv', files)]
#years = sapply(files, function(file) tail(strsplit(file, 'Focos_')[[1]],1))
#years = sapply(years, function(year) strsplit(year, '-')[[1]][1])
#years = as.numeric(years)


rv = processFile(fileName, nlines_per_step, rv, lonlat2.5)

#sats = unique(unlist(lapply(rv, names)))[-1]
r[] = NaN
rr = r
#names(rr) = sats

addYear2rr <- function(ri, yr) {
    print(yr)
    mnths = ((yr - 2001)*12+1):((yr-2001 + 1)*12)
    
    #for (i in 2:length(ri)) {
    #    index = which(sats == names(ri)[i])
    for (mn in 1:12) 
        if (mnths[mn] <= nlayers(rr))
            rr[[mnths[mn]]][] = ri[,mn]
        
    
    rr <<- rr
} 
mapply(addYear2rr, rv, years)

mask = raster(mask_file)
rr_rs = raster::resample(rr,  mask)


writeRaster.Standard(rr_rs, 'outputs/firecount_SE_Aus_2001_onwards.nc')
#writeRaster(r, file = "MODIS_fire_count.nc",overwrite=TRUE)

