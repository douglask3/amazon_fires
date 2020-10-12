library(raster)
library(greenbrown)
source("libs/plotStandardMap.r")
library(gitBasedProjects)
library(rasterExtras)
library(ncdf4)
graphics.off()

files = paste0("inputs/amazon_region/",
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

regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75),
               E = -c(61.25, 53.75, 23.75, 18.75))
regions = list("A" = -c(76.25, 66.25, 11.25, 6.25),
               "B" = -c(66.25, 58.75, 11.25,  6.25),               
               "C" = -c(58.75, 51.25, 8.75,  3.75),  
               "D" = -c(51.25, 46.25,  8.75,  1.25),
               "E" = -c(61.25, 56.25, 21.25, 13.75))
limits_tree = seq(0, 0.9, 0.1) * 100
                     
scales = c(1/0.8, 1, 1, 1)

logit <- function(r, ns) {
    r[r<0.000001    ] = 0.000001    
    r[r>(1-0.000001)] = 1-0.000001
    r = log(r/(1+r*(-1)))
    return(r)
}
logistic <- function(r) 1/(1+exp(r*(-1)))

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
    dat = dat * 100
    
    plotStandardMap(dat[[nlayers(dat)]], cols, limits_tree, NULL, TRUE, maxLab = 100)
    mtext(side = 3, title)
    
    plotStandardMap(trend, cols_trend, limits_trend, pValue, TRUE, extend_max = TRUE, extend_min = TRUE)  
    return(addLayer(trend, pValue))
}
png("figs/treeCoverTrends.png", height = 170, width = 183, units = 'mm', res = 300)
    layout(rbind(c(1, 3, 5, 7), c(2, 4, 6, 8), c(9, 10, 10, 10)))
    par(mar = rep(0,4), oma = c(0,2,2,2))

    trends = mapply(plotVariable, files, names(files), scales, colss, cols_trends,
                    limitss, limits_trends) 
    mtext(outer = TRUE, side = 2, adj = 1 - 0.33/2, 'Cover (%)')
    mtext(outer = TRUE, side = 2, adj = 0.5, 'Trend in Cover\nover period (%)', line = -1)

    summ = trends[[1]][[1]]
    summ[] = NaN

    p = c(0, 0, 0)

    makeTests <- function(trend) 
        list(trend[[2]] >- 0.1 | trend[[1]] == 0, trend[[2]] < 0.1 & trend[[1]] < 0,
             trend[[2]] < 0.1 & trend[[1]] > 0)

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
    #dev.new()
    cols = c("white", "cyan", "orange", "#2B5000", "pink", "red",
             "#99CC00", "#000066", "magenta")
    plotStandardMap(summ, cols = cols, limits = (2:length(labs_out))-0.5)

    addBox <- function(xs, ys = NULL, name = NULL, ...) {  
        if (is.null(ys)) ys = xs[3:4]
        lines(c(xs[1:2], xs[2:1], xs[1]), c(ys[1], ys[1:2], ys[2:1]), ...)
        if (!is.null(name)) {
            FUN <- function(col, cex) text(x = mean(xs[1:2]), y = mean(ys[1:2]), name,
                                            col = make.transparent(col, 0.67), cex = cex, font = 2)
            FUN("black", 2.0)   
            FUN("black", 1.6) 
            FUN("white", 1.8)
        }
    }
    
    #browser() 
    lines(c(-180, 180), c(-23.5, -23.5), lty = 2, lwd = 1.5)
    mtext(side = 2, "Trend based regions")
    #plot.new()
    #legend('left', legend = labs_out, pch = 15, pt.cex = 3, col = cols)
#dev.off()

comment = as.list(1:length(labs_out))
names(comment) = labs_out
writeRaster.gitInfo(summ, 'inputs/amazon_region/treeCoverTrendRegions.nc', comment = comment, overwrite = TRUE)

deforestMask = summ
deforestMask[summ!= 6] = 0
deforestMask[summ== 6] = 1

lat = yFromCell(summ,1:length(summ))
lat = lat > (-23.5) & lat < 0
deforestMask[!lat] = 0
deforestMask[is.na(summ)] = NaN

writeRaster.gitInfo(deforestMask, 'inputs/amazon_region/treeCoverTrendRegions-F.nc', comment = comment, overwrite = TRUE)


deforestMaskG = deforestMaskH = deforestMaskI = deforestMask
t3 = !is.na(deforestMask[])
t1 = xFromCell(deforestMask, 1:length(deforestMask)) > -51.25
t2 = yFromCell(deforestMask, 1:length(deforestMask)) < -11.25
t4 = yFromCell(deforestMask, 1:length(deforestMask)) > (-11.25+5)


t5 = yFromCell(deforestMask, 1:length(deforestMask)) > (-11.25-2.5)
t6 = xFromCell(deforestMask, 1:length(deforestMask)) < (-51.25-15)

deforestMaskG[t1 & t3] = 0
deforestMaskG[t2 & t3 & !(t5 & t6)] = 0
#deforestMaskG[] = 0
#deforestMaskG[t4 & t3] = 0

deforestMaskH[!t1 & t3] = 0
deforestMaskI[!t2 & t3] = 0
deforestMaskI[ t1 & t3] = 0
deforestMaskI[ t5 & t6] = 0
writeRaster.gitInfo(deforestMaskG, 'inputs/amazon_region/treeCoverTrendRegions-G.nc', comment = comment, overwrite = TRUE)
writeRaster.gitInfo(deforestMaskH, 'inputs/amazon_region/treeCoverTrendRegions-H.nc', comment = comment, overwrite = TRUE)
writeRaster.gitInfo(deforestMaskI, 'inputs/amazon_region/treeCoverTrendRegions-I.nc', comment = comment, overwrite = TRUE)

addPnts <- function(r, pch, ...) {
    xy = xyFromCell(r, which(r[]==1))
    points(xy[,1], xy[,2], pch = pch, col = make.transparent("black", 0.5), ...)
}
addPnts(deforestMaskG, pch = 19, cex = 0.67)
addPnts(deforestMaskH, pch = 4)
addPnts(deforestMaskI, pch = 8)
mapply(addBox, regions, name = names(regions), lwd = 2)
dev.off()

png("figs/regionsMap.png", height = 100*0.5, width = 140*0.5, units = 'mm', res = 300) 
par(mar = c(0, 0, 0, 0))
out = deforestMask + deforestMaskH + 2*deforestMaskI
#out[out == 0] = NaN
cols = c("#F0F0F0", '#1b9e77','#d95f02', '#7570b3')#c("green", "red")
plotStandardMap(out, ylim = c(-28, 8),
                cols = cols, limits = 0.5:2.5)
mapply(addBox, regions, name = names(regions), lwd = 2)
legend('bottomleft', horiz = TRUE, col = cols[-1], pch = 15, pt.cex = 1, legend = c("G - Humid forest", "H - Cerrado/\nCattinga savanna", "I - Chiquitano/\nGran Chaco dry forest"), x.intersp = 1,cex = 0.5, text.width=13.5, box.col = "white")
dev.off()

regions = list("A" = -c(71.25, 63.75, 11.25,  6.25),
               "B" = -c(61.25-2.5, 53.75, 11.25-2.5,  6.25),  
               "C" = -c(48.25, 43.25,  8.75,  1.25),
               "D" = -c(66.25, 58.75, 18.75, 13.75),   
               "E" = -c( 61.25, 53.75, 23.75, 18.75))

regions = list("A" = -c(66.25, 58.75, 11.25,  6.25),
               "B" = -c(56.25, 51.25, 8.75,  3.75),  
               "C" = -c(48.75, 43.75,  8.75,  1.25),
               "D" = -c(61.25, 58.75, 18.75, 13.75),   
               "E" = -c( 61.25, 56.25, 21.25, 18.75))

regions = list("A" = -c(76.25, 71.25, 11.26, 6.25),
               "B" = -c(71.25, 66.25, 11.25, 6.25),
               "C" = -c(66.25, 61.25, 11.25,  6.25),
               "D" = -c(61.25, 56.25, 11.25, 3.75),
               "E" = -c(56.25, 51.25, 8.75,  3.75),  
               "F" = -c(51.25, 46.25,  8.75,  1.25),
               "G" = -c(61.25, 58.75, 18.75, 13.75),   
               "H" = -c( 61.25, 56.25, 21.25, 18.75))
regions = list("A" = -c(76.25, 66.25, 11.25, 6.25),
               "B" = -c(66.25, 58.75, 11.25,  6.25),               
               "C" = -c(58.75, 51.25, 8.75,  3.75),  
               "D" = -c(51.25, 46.25,  8.75,  1.25),
               "E" = -c(61.25, 56.25, 21.25, 13.75))
 
rmasks = summ
rmasks[!is.na(rmasks)] = 0

makeMask <- function(name, extent) {
    X = (extent[1]+0.1):(extent[2]-0.1)
    Y = rep((extent[3]+0.1):(extent[4]-0.1), each = length(X))
    rmasks[unique(cellFromXY(rmasks, cbind(X,Y)))] = 1
    plot(rmasks)
    browser()
    writeRaster.gitInfo(rmasks,
                        paste0('inputs/amazon_region/treeCoverTrendRegions-', name, '.nc'),
                        comment = comment, overwrite = TRUE)
}

mapply(makeMask, names(regions), regions)

BA =  brick('inputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc')

fireMonths = 6:8
months = lapply(fireMonths, seq, nlayers(BA), by = 12)
monthsByYr = lapply(1:min(sapply(months, length)),
                    function(yr) sapply(months, function(m) m[yr]))

x = trends[[4]][[1]][]
y = trends[[1]][[1]][]
z =  mean(BA[[tail(monthsByYr,1)[[1]]]])/mean(BA[[unlist(monthsByYr)]]); z = z[]

mask = !is.na(x + y + z)

x = x[mask]
y = y[mask]
z = z[mask]

cols = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
limits = 0.5:4.5
limits = c(-rev(limits), limits)
limits = 1.2^(limits)

z =  cut_results(z, limits)
cols = cols[z]

axisLogNeg <- function(x) {
    x0 = x
    x[x0==0] = 0.5*min(x0[x0>0])
    x[x0>0] = log(x[x0>0])
    x[x0<0] = -log(-x0[x0<0]+1)
    return(x)
}

x = axisLogNeg(x)
y = axisLogNeg(y)

plot(x, y, pch = 19, cex = 3, col = cols)




