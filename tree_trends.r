library(raster)
library(greenbrown)
source("libs/plotStandardMap.r")

files = paste0("outputs/amazon_region/",
               c("vegetation/treecover-2001-June2018.nc",
                 "human/cropland2001-2019.nc", "human/pasture2001-2019.nc", "human/fract_agr2001-2019.nc"))
                
names(files)  = c("Tree", "Cropland", "Pasture", "Total Agriculture")

cols_trend_ag = rev(c('#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e'))

cols_trends   = list(c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
                       '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
                     cols_trend_ag, cols_trend_ag, cols_trend_ag)
limits_trends = list(c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15),
                     c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15),#c(-50, -20, -10, -5, -1, 1, 5, 10, 20, 50),
                     c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15),
                     c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15))
cols_ag       = c('#ffffe5','#fff7bc','#fee391','#fec44f','#fe9929','#ec7014','#cc4c02','#993404','#662506')
                     
colss         = list(c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529'),
                     cols_ag, cols_ag, cols_ag)
limitss       = list(c(0   , 1, 10, 20, 40, 60, 80),
                     c(0, 0.1, 1, 2, 5, 10, 20, 50),
                     c(0, 0.1, 1, 2, 5, 10, 20, 50),
                     c(0, 0.1, 1, 2, 5, 10, 20, 50))
                     
scales = c(1/0.8, 1, 1, 1)

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
    return(addLayer(trend, pValue))
}

layout(rbind(c(1, 3, 5, 7), c(2, 4, 6, 8), c(9, 10, 10, 10)))
par(mar = rep(0,4), oma = c(0,2,2,2))

trends = mapply(plotVariable, files, names(files), scales, colss, cols_trends, limitss, limits_trends) 
mtext(outer = TRUE, side = 2, adj = 1 - 0.33/2, 'Cover (%)')
mtext(outer = TRUE, side = 2, adj = 0.5, 'Trend in Cover\nover period (%)', line = -1)

summ = trends[[1]][[1]]
summ[] = NaN

p = c(0, 0, 0)

makeTests <- function(trend) 
    list(trend[[2]] >- 0.05 | trend[[1]] == 0, trend[[2]] < 0.05 & trend[[1]] < 0, trend[[2]] < 0.05 & trend[[1]] > 0)

labs = c("no change in", "decreased", "increased")
labs_out = c()
for (tree in makeTests(trends[[1]]))  {
    p[1] = p[1] + 1
    p[2] = 0
    for (agri in makeTests(trends[[4]])) {
        p[2] = p[2] + 1
        p[3] = p[3] + 1
        mask = tree & agri
        
        summ[mask] = p[3]
        labs_out = c(labs_out, paste(labs[p[1]], "tree cover,", labs[p[2]], "agriculature"))
    }
}

cols = c("white", "cyan", "orange", "#2B5000", "pink", "red", "#99CC00", "#000066", "magenta")
plotStandardMap(summ, cols = cols, limits = (2:length(labs_out))-0.5)
lines(c(-180, 180), c(-23.5, -23.5), lty = 2, lwd = 1.5)
mtext(side = 2, "Trend based regions")
plot.new()
legend('left', legend = labs_out, pch = 15, pt.cex = 3, col = cols)

comment = as.list(1:length(labs_out))
names(comment) = labs_out
writeRaster.gitInfo(summ, 'outputs/amazon_region/treeCoverTrendRegions.nc', comment = comment, overwrite = TRUE)