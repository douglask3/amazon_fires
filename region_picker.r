## region picker
library(raster)
library(rasterExtras)
source("libs/plotStandardMap.r")
source("libs/standardGrid.r")
#library(gitBasedProjects)
library(ncdf4)

graphics.off()
options(scipen = 999)

treeCover = 'outputs/Australia_region/vegetation/from_2001/treecover-2001-June2018.nc'
fireCount = 'outputs/Australia_region/firecount-SE_Aus_2001_onwards.nc'

limits_fc = c(0, 1000, 2000, 5000, 10000, 20000, 50000, 100000)/ 100000
cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
            '#e31a1c','#bd0026','#800026')

limits_dfc = c(-200000, -100000, -50000, -10000, -5000, -1000, -100,  100, 1000, 2000, 10000, 50000, 100000, 200000)/ 100000
dcols_fc =rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))

limits_tree = c(0, 1, 2, 5, 10, 20, 30)
cols_tree = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

fireSeason = 11:12

regions = list('Gold Coast/Northern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi & Blue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25),
               'Nadgee/Wallagaraugh River' = c(146.25, 148.75, -38.75, -36.25),
               'Kangaroo Island/Adelaide' = c(136.25, 138.75, -36.25, -33.75)) 

variables = paste0("outputs/Australia_region/climate/from_2001/",
                   c( "emc-2001-2019.nc", "precip2001-2019.nc", "rhumMaxMax.2001-2019.nc", "soilw.0-10cm.gauss.2001-2019.nc", "precip_yrLag.2001-2019.nc"))
vnames     = c("emc", "precip", "rh", "soilw", "precip12")
units = c("%", "mm ~yr-1~", "%", "%", "%")

line_cols = c("purple", "blue", "grey", "green", "blue")
line_lty  = c(1, 1, 1, 1, 2)
map_cols = list(rev(c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')),
                c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'),
                c('#ffffff','#f5e0f5','#e3a9e3','#cc03cc','#969696','#737373','#525252','#252525','#000000'),
                c('#f7fcf5','#e5f5e0','#c7e9c0','#a1d99b','#74c476','#41ab5d','#238b45','#006d2c','#00441b'),
                c('#fff7fb','#ece2f0','#d0d1e6','#a6bddb','#67a9cf','#3690c0','#02818a','#016c59','#014636'))
                
maps_dcols = list(c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419'),
                  c('#543005','#8c510a','#bf812d','#dfc27d','#f6e8c3','#f5f5f5','#c7eae5','#80cdc1','#35978f','#01665e','#003c30'),
                  c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'),
                  c('#40004b','#762a83','#9970ab','#c2a5cf','#e7d4e8','#f7f7f7','#d9f0d3','#a6dba0','#5aae61','#1b7837','#00441b'),
                  c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
                  
maps_limits = list(c(0, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2, 0.22, 0.24) *100,
                   c(0, 1, 2, 3, 4, 5, 6),
                   c(0, 10, 20, 30, 40, 50, 60, 70),
                   c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6) *100,
                   c(0, 1, 2, 5, 10, 20, 30))
 
maps_dlimits = list(c(-1, -0.5, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.5, 1)*10,
                    c(-4, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5),
                    c(-20, -10, -5, -2, -1, 1, 2, 5, 10, 20),
                    c(-0.2, -0.1, -0.05, -0.02, -0.01, 0.01, 0.02, 0.05, 0.1, 0.2)*100,
                    c(-2, -1, -0.5, -0.2, -0.1, 0.1, 0.2, 0.5, 1, 2))
                    
scaling = c(100, 1, 1, 100, 100)
                    
                    
                   
fireSeasons = lapply(2:19, function(i) (i-1) * 12 + fireSeason)

treeCover = mean(brick(treeCover))  * 100
treeCoverR = treeCover
ll = xyFromCell(treeCoverR, 1:length(treeCoverR[]))
test = ll[,1] > 140 & ll[,2] < -22 & treeCoverR[] > 10
test[is.na(test)] = FALSE
treeCoverR[!test] = NaN
writeRaster.gitInfo(treeCoverR, 'outputs/Australia_region/SE_TempBLRegion.nc', overwrite = TRUE)

fireCount = brick(fireCount) 

plotVariable <- function(variable, name, limits, cols, dlimits, dcols, units, scaling = 1, diff = TRUE) {
    fname = paste0("figs/yearly_", name, ".png")
    
    #fireCount_mean = mean(variable)

    variable_seasons = lapply(fireSeasons, function(i) mean(variable[[i]]) * scaling)
    variable_seasons_mean = mean(variable[[unlist(fireSeasons)]]) * scaling
    
    if (diff) {
        variable_seasons = lapply(variable_seasons, function(i) i - variable_seasons_mean)
        extend_min = TRUE
    } else {
        dlimits = limits
        dcols = cols
        extend_min = FALSE
    }
    png(fname, height = 8.5, width = 7, units = 'in', res = 300)
        layout(t(matrix(c(1:22, 0, 0, 0, 23, 23, 0), nrow = 4)), heights = c(rep(1, 6), 0.3))
        par(mar = rep(0, 4), oma = c(2, 1, 1, 1))

        plotStandardMap(treeCover, title2 = 'Tree cover', cols = cols_tree, limits = limits_tree,
                        left_text_adj = 0.1)

        StandardLegend(cols_tree, limits_tree, treeCover, 0.9, transpose = TRUE,
                       plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, units = '%', maxLab = 100)

        plotStandardMap(variable_seasons_mean, title2 = '     Average\n   fire season', cols = cols,
                        limits = limits, left_text_adj = 0.1, left_text_adj_line = -1.0)
        StandardLegend(cols, limits, variable_seasons_mean, 0.9, transpose = TRUE,
                       plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, extend_max = TRUE)

        text.units(units, x = 0.33, y = 0.5, srt = 90)

        mapply(plotStandardMap, variable_seasons, title2 = 2002:2019,
               MoreArgs = list(cols = dcols, limits = dlimits, left_text_adj = 0.1, regions = regions))

        StandardLegend(dcols, dlimits, variable_seasons[[1]], 0.9, oneSideLabels = FALSE,
                       extend_min = extend_min, units = units)
    dev.off.gitWatermark()
}
variables = lapply(variables, brick)

plotVariable(fireCount, "fireCount", limits_fc, cols_fc, limits_dfc, dcols_fc, 'counts/k~m2~')

mapply(plotVariable, variables, paste0(vnames, "-dff"), maps_limits, map_cols, maps_dlimits, maps_dcols, units, scaling)
mapply(plotVariable, variables, vnames, maps_limits, map_cols, maps_dlimits, maps_dcols, units, scaling, diff = FALSE)

browser()

plotRegion <- function(region, name) {
    plot(c(2001, 2020), c(0, 1), axes = FALSE, xlab = '', ylab = '', type = 'n')
    standardGrid()
    plotVar <- function(var, col, line = 0, side = 2, ...) {
        y = unlist(layer.apply(raster::crop(var, extent(region)), sum.raster, na.rm = TRUE))
        rangeY = range(y)
        
        scale0 <- function(x, rx) (x - rx[1]) / diff(rx)
        
        y = scale0(y, rangeY)
        lines(seq(2001, by = 1/12, length.out = length(y)), y, col = col, ...)
        
        labels = unique(signif(seq(rangeY[1], rangeY[2], length.out = 7), 1))
        if (sum(labels <= rangeY[2]) < 4) labels = unique(signif(seq(rangeY[1], rangeY[2], length.out = 7), 2))
        
        at = scale0(labels, rangeY)
        axis(side = side, at = at, labels = labels, line = line, col = col)
    }
   
    mapply(plotVar, variables, line_cols, line = c(0, 1, 0, 1, 2),
            side = c(2, 2, 4, 4, 4), lty = line_lty, lwd = 2)
    plotVar(fireCount, 'red', lwd = 2.5, line = 2)
    mtext(name, side = 3, line = -1)
}

png("figs/region_INPUT_TS.png", height = 10, width = 21, units = 'in', res = 300)
par(mfrow = c(4, 1), mar = c(0, 4, 1, 4), oma = c(4, 0, 1, 0))
mapply(plotRegion, regions, names(regions))
axis(1, at = 2001:2020)
mtext(side = 1, 'Year', line = 2.5)
dev.off.gitWatermark()
