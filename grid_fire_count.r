library("raster")
library(snow)
library(ncdf4)
source("libs/writeRaster.Standard.r")

dir_nr = "data/fireCounts/"
nlines_per_step = 10000

file_grid = "data/air.mon.mean.nc"
mask_file = "data/climate/climate_mask.nc"

extent = extent(c(360-85, 360-30, -33, 15))

processFile <- function(file, nlines_per_step, rv, lonlat2.5) {
    print(file)
    source("libs/filename.noPath.r")
    nlines = as.numeric(strsplit(system(paste('wc -l', file), TRUE), ' data')[[1]][1])
    cnames = colnames(read.csv(file, nrows = 1, skip = 0))
    
    readLines <- function(i) {
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
        
            dates = strsplit(dat[, cnames == "datahora"], '/', fixed = TRUE)
            mnths = as.numeric(sapply(dates, function(i) i[[2]]))

            satalites =  dat[, cnames == "satelite"]
        
            lons0 = lons = dat[, cnames == "longitude"]
            lons[lons < 0] = 360 + lons[lons<0]
            lons = round(lons/2.5)        
            lats = round(dat[, cnames == "latitude" ]/2.5)
        
            add2grid <- function(lon, lat, mnth, satalite, rv) {  
                cell = which(lonlat2.5[,1] == lon & lonlat2.5[,2] == lat)
                #layer = which(months == mnth)
            
                if (length(cell) == 0) return(rv)
                else if (length(cell) > 1) {
                    print("more than one")
                }
            
                item = which(names(rv) == satalite)
            
                if (length(item) == 0) {
                    rnames = names(rv)
                    rv[[length(rv)+1]] = rv[[1]]                
                    names(rv) = c(rnames, satalite)
                    item = length(rv)
                }
                rv[[item]][cell, mnth] = rv[[item]][cell, mnth] + 1            
                return(rv)
            }
        
            for (i in 1:length(lons)) 
                rv = add2grid(lons[i], lats[i], mnths[i], satalites[i], rv)
            
            save(rv, file = temp_file)
        }
            
        gc()  
        
        return(rv)
    }
    rout = lapply(1:ceiling(nlines/nlines_per_step), readLines)
    
    sats = unique(unlist(lapply(rout, names)))
    rv = rep(rv, length(sats))
    names(rv) = sats
    
    for (ri in rout) for (i in 1:length(ri)) {
        index = which(sats == names(ri)[i])
        rv[[index]] = rv[[index]] + ri[[i]]
    } 
    
    gc()
    return(rv)
}


r = brick(file_grid)
r = r[[637:nlayers(r)]]

r[] = 0

rv = values(r[[1:12]])
rv = list(blank = rv)

lonlat2.5 = xyFromCell(r[[1]], 1:length(r[[1]]))/2.5

files = list.files(dir_nr, full.names = TRUE)
files = files[grepl('.csv', files)]

years = sapply(files, function(file) tail(strsplit(file, 'Focos_')[[1]],1))
years = sapply(years, function(year) strsplit(year, '-')[[1]][1])
years = as.numeric(years)

cl = makeSOCKcluster(c("localhost", "localhost", "localhost", "localhost"))
    #rv = lapply(files, processFile, nlines_per_step, rv, lonlat2.5)
    rv = parLapply(cl, files, processFile, nlines_per_step, rv, lonlat2.5)
stopCluster(cl)

sats = unique(unlist(lapply(rv, names)))[-1]
r[] = NaN
rr = rep(c(r), length(sats))
names(rr) = sats

addYear2rr <- function(ri, yr) {
    print(yr)
    mnths = ((yr - 2001)*12+1):((yr-2001 + 1)*12)
    
    for (i in 2:length(ri)) {
        index = which(sats == names(ri)[i])
        for (mn in 1:12) 
            if (mnths[mn] <= nlayers(rr[[index]]))
                if (sum(ri[[i]][,mn]) > 0)
                    rr[[index]][[mnths[mn]]][] = ri[[i]][,mn]
        
    }
    rr <<- rr
} 
mapply(addYear2rr, rv, years)

mask = raster(mask_file)
rr_rs = lapply(rr, raster::resample, mask)

fnames = paste0('outputs/fireCount-', sats, '.nc')
mapply(writeRaster.Standard, rr_rs, fnames)
#writeRaster(r, file = "MODIS_fire_count.nc",overwrite=TRUE)

