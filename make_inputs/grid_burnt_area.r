library("raster")
library(rasterExtras)
source("libs/convert_pacific_centric_2_regular.r")

dat = brick('data/MCD64A1.006_500m_aid0001.nc')
ncuts = 10

mask = raster('data/climate/climate_mask.nc')
mask = convert_pacific_centric_2_regular(mask)

out = mask
out[] = 0
regridLayer <- function(r) {
    tempFile = paste("temp/", "MCD64A1.006", names(r), ".nc", sep = "_")
    if (file.exists(tempFile)) return(raster(tempFile))
    regridByXY <- function(x, y, area = FALSE) {
        print(x)
        ri = crop(r, extent(x, y))

        aggRes <- function(rj) {
            rj = aggregate(rj, 200)
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
        lapply(ycut, function(y) lapply(xcut, regridByXY, y, ...))
    plates = gridDat()
    areas  = gridDat(area = TRUE)
    
    
    addUp <- function(r) {
        for (px in r) for (py in px) out = out + py
        return(out)
    }
    BA = addUp(plates)
    AR = addUp(areas)
    BA = BA/AR
    BA[AR == 0] = NaN
    BA = writeRaster(BA, file = tempFile)
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

out = layer.apply(dat, regridLayer)
