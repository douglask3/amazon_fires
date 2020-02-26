library("raster")
library(rasterExtras)
source("libs/convert_pacific_centric_2_regular.r")
library(snow)
library(ncdf4)
source("libs/writeRaster.Standard.r")

dat = brick('data/MCD64A1.006_500m_aid0001.nc')
ncuts = 5

mask = raster('data/climate/climate_mask.nc')
mask = convert_pacific_centric_2_regular(mask)

muliCore = FALSE

regridLayer <- function(r, xcut, ycut, mask) {   
    
    if (is.list(r)) {
        r = r[[1]]
        library("raster")
        library(rasterExtras)
        source("libs/convert_pacific_centric_2_regular.r")
    }    
    tempFile = paste("temp/", "xMCD64A1.006", names(r), ".nc", sep = "_") 
    if (file.exists(tempFile)) return(raster(tempFile))
     
    fileConn = paste0("debugging/debug-", names(r), ".txt")
    #if (file.exists(fileConn)) return(NULL)
    
    fileConn =  file(fileConn)
    writeLines("configing", fileConn)

    out = mask
    out[] = 0

    regridByXY <- function(x, y, area = FALSE) {
        writeLines(paste(c(x, y), collpase = '-'), fileConn)
        
        ri = crop(r, extent(x, y))
        maski = disaggregate(crop(mask, extent(x, y)), 600)
        aggRes <- function(rj) {
            rj = raster::resample(rj, maski)
            rj = aggregate(rj, 600)
            rj = raster::resample(rj, mask)
        }
        
        if (area) ri[ri >= 0] = 1 else ri[ri >0 ] = 1
        ri[ri < 0] = 0   
        
        ri = aggRes(ri)
        ri[is.na(ri)] = 0.0
        ri[ri<0] = 0
        return(ri)
    }  
    
    
    gridDat <- function(...)
        lapply(ycut, function(yi) lapply(xcut, regridByXY, yi, ...))
    plates = gridDat()
    areas  = gridDat(area = TRUE)
    
    
    addUp <- function(r) {
        for (px in r) for (py in px) out = out + py
        return(out)
    }
    writeLines("Adding BA", fileConn)
    BA = addUp(plates)
    writeLines("Adding AR", fileConn)
    AR = addUp(areas)
    writeLines("Normalising", fileConn)
    BA = BA/AR
    writeLines("masking", fileConn)
    BA[AR == 0] = NaN
    writeLines("outputting", fileConn)
    BA = writeRaster(BA, file = tempFile, overwrite = TRUE)
    
    return(BA)
}

cutCords <- function(id, xs) {
    xcut = diff(extent(dat)[id])/ncuts
    xcut = seq(extent(dat)[id[1]], extent(dat)[id[2]], by = xcut)
    xcut = sapply(xcut, function(i) which.min(abs(i - xs)))
    out = mapply(c,xs[head(xcut, -1)], xs[xcut[-1]] , SIMPLIFY = FALSE) 
    #out = mapply(function(i,j) c(i + 1.1*diff/2, j - 1.1*diff/2),xs[head(xcut, -1)], xs[xcut[-1]] , SIMPLIFY = FALSE) 
    return(out)
}
xcut = cutCords(1:2, xFromCol(dat, 1:ncol(dat)))
ycut = cutCords(3:4, yFromRow(dat, 1:nrow(dat)))

#browser()
if (muliCore) {
    cl = makeSOCKcluster(rep("localhost", 4))
        #lapply(layer.apply(dat, function(i) c(i)),
        #           regridLayer, xcut = xcut, ycut = ycut, mask = mask)
        parLapply(cl, layer.apply(dat, function(i) c(i)),
                  regridLayer, xcut = xcut, ycut = ycut, mask = mask)
    stopCluster(cl)
}
out = layer.apply(dat, regridLayer, xcut = xcut, ycut = ycut, mask = mask)
out = writeRaster.Standard(out, file = 'outputs/fire_counts/burnt_area_MCD64A1.006.nc')
