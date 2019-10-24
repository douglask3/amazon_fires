library(raster)
library(greenbrown)

files = paste0("outputs/amazon_region/",
               c("vegetation/treecover-2001-June2018.nc",
                 "human/cropland2001-2019.nc", "human/pasture2001-2019.nc"))
                
names(files)  = c("Tree Cover", "Cropland Cover", "Pasture Cover")

cols_trend_ag = rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e'))

cols_trends   = list(c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
                       '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
                     cols_trend_ag, cols_trend_ag)
limits_trends = list(c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15),
                     c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15),#c(-50, -20, -10, -5, -1, 1, 5, 10, 20, 50),
                     c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15))
cols_ag       = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
                     
colss         = list(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'),
                     cols_ag, cols_ag)
limitss       = list(c(0   , 1, 10, 20, 40, 60, 80),
                     c(0, 0.1, 1, 2, 5, 10, 20, 50),
                     c(0, 0.1, 1, 2, 5, 10, 20, 50))
                     
scales = c(1/0.8, 1, 1)

plotStandardMap <- function(r, cols, limits, e, add_legend = FALSE, ...) {
    plot_raster_from_raster(r, e = e,
                            cols = cols, limits = limits, add_legend = FALSE,
                            quick = TRUE, ePatternRes = 5, ePatternThick = 0.67, limits_error = c(0.05, 0.1))
    if (add_legend) {
        add_raster_legend2(cols, limits, dat = trend[[2]],
                           transpose = FALSE, srt = 0, oneSideLabels= FALSE,
                           plot_loc = c(0.35, 0.99, 0.09, 0.12),  ylabposScling=0.8, ...)
    }
}

logistic <- function(r) 1/(1+exp(-r))

plotVariable <- function(file, title, scale, cols, cols_trend, limits, limits_trend) {
    
    dat = brick(file)
    dat = dat[[seq(1, nlayers(dat), by = 12)]]*scale
    ldat = logit(dat, ns = 9E9)   
    nl = nlayers(dat)

    trend = TrendRaster(ldat, freq = 1)
    pValue = trend[[3]]
    
    ddat = ldat[[nl]] - nl*trend[[2]] 
    ddat = logistic(ddat)
    
    trend = dat[[nl]] - ddat
    trend = trend * 100
    dat = dat*100
    
    plotStandardMap(dat[[nlayers(dat)]], cols, limits_tree, NULL, TRUE, maxLab = 100)
    mtext(side = 3, title)
    plotStandardMap(trend, cols_trend, limits_trend, pValue, TRUE, extend_max = TRUE, extend_min = TRUE)  
    return(trend)
}

par(mfcol = c(2,3), mar = rep(0,4), oma = c(0,2,2,0))

mapply(plotVariable, files, names(files), scales, colss, cols_trends, limitss, limits_trends) 