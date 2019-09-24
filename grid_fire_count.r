library("raster")

dir_nr = "data/fireCounts/"
nlines_per_step = 50

file_grid = "data/air.mon.mean.nc"

extent = extent(c(360-85, 360-30, -33, 15))

processFile <- function(file) {
    print(file)
    
    nlines = as.numeric(strsplit(system(paste('wc -l', file), TRUE), ' data')[[1]][1])
    nlines = 654
    cnames = colnames(read.csv(file, nrows = 1, skip = 0))
    readLines <- function(i) {
        print(i/ceiling(nlines/nlines_per_step))     
        skip = nlines_per_step * (i-1)
        if ((skip + nlines_per_step) > nlines) nrows = nlines - skip
        else nrows = nlines_per_step
        
        dat   = read.csv(file, skip = skip, nrows = nrows, stringsAsFactors = FALSE)
        dates = strsplit(dat[, cnames == "datahora"], '/', fixed = TRUE)
        yrs   = as.numeric(sapply(dates, function(i) i[[1]]))
        mnths = as.numeric(sapply(dates, function(i) i[[2]]))

        satalites =  dat[, cnames == "satelite"]
        
        if (max(yrs) < 2001) return()
        
        lons0 = lons = dat[, cnames == "longitude"]
        lons[lons < 0] = 360 + lons[lons<0]
        lons = round(lons/2.5)        
        lats = round(dat[, cnames == "latitude" ]/2.5)

        add2grid <- function(lon, lat, yr, mnth, satalite) {            
            cell = which(lonlat2.5[,1] == lon & lonlat2.5[,2] == lat)
            layer = which(years == yr &  months == mnth)
            
            if (length(layer) == 0 || length(cell) == 0) return()
            else if (length(layer) > 1 || length(cell) > 1) {
                print("more than one")
                browser()
            }
            item = which(names(r) == satalite)
            if (length(item) == 0) {
                rnames = names(r)
                r = c(r, r[[1]])
                names(r) = c(rnames, satalite)
                item = length(r)
            }
            
            r[[item]][[layer]][cell] = r[[item]][[layer]][cell] + 1
            r <<- r
        }
        
        mapply(add2grid, lons, lats, yrs, mnths, satalites)
        gc()
    }
    sapply(1:ceiling(nlines/nlines_per_step), readLines)
    browser()
}

files = list.files(dir_nr, full.names = TRUE)
files = files[grepl('.csv', files)][2:1]

r = brick(file_grid)
r = r[[637:nlayers(r)]]

date = names(r)
date = strsplit(date, '.', fixed = TRUE)
years = as.numeric(sapply(date, function(i) substr(i[[1]], 2, 5)))
months = as.numeric(sapply(date, function(i) i[[2]]))

r[] = 0
r = list(blank = r)
#r = convert_pacific_centric_2_regular(r)
lonlat2.5 = xyFromCell(r[[1]], 1:length(r[[1]]))/2.5

sapply(files, processFile)

#writeRaster(r, file = "MODIS_fire_count.nc",overwrite=TRUE)

