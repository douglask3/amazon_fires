## region picker
library(raster)
library(rasterExtras)
source("libs/plotStandardMap.r")
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

variables = paste0("outputs/Australia_region/climate/", c("air2001-2019.nc", "emc-2001-2019.nc", "precip2001-2019.nc", "relative_humidity2001-2019.nc", "soilw.0-10cm.gauss.2001-2019.nc", "tasMax_2001-2019.nc"))

line_cols = c("brown", "purple", "blue", "grey", "cyan", "red")

variables = c("outputs/Australia_region/climate/from_2001/tmaxMax.2001-2019.nc",
              "outputs/Australia_region/climate/from_2001/emc-2001-2019.nc",
              "outputs/Australia_region/climate/from_2001/soilw.0-10cm.gauss.2001-2019.nc",
               "outputs/Australia_region/vegetation/from_2001/MeanAnnual_soilw.0-10cm.gauss.2001-2019.nc")

line_cols = c("brown", "purple", "blue", "green")


fireSeasons = lapply(2:19, function(i) (i-1) * 12 + fireSeason)


treeCover = mean(brick(treeCover))  * 100
fireCount = brick(fireCount) 

fireCount_mean = mean(fireCount)

fireCount_seasons = lapply(fireSeasons, function(i) mean(fireCount[[i]]))
fireCount_season_mean = mean(fireCount[[unlist(fireSeasons)]])

fireCount_seasons = lapply(fireCount_seasons, '-', fireCount_season_mean)

png("figs/yearly_fires_map.png", height = 8.5, width = 7, units = 'in', res = 300)
    layout(t(matrix(c(1:22, 0, 0, 0, 23, 23, 0), nrow = 4)), heights = c(rep(1, 6), 0.3))
    par(mar = rep(0, 4), oma = c(2, 1, 1, 1))

    plotStandardMap(treeCover, title2 = 'Tree cover', cols = cols_tree, limits = limits_tree,
                    left_text_adj = 0.1)

    StandardLegend(cols_tree, limits_tree, fireCount_seasons[[1]], 0.9, transpose = TRUE,
                   plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, units = '%', maxLab = 100)

    plotStandardMap(fireCount_season_mean, title2 = '     Average\n   fire season', cols = cols_fc,
                    limits = limits_fc, left_text_adj = 0.1, left_text_adj_line = -1.0)
    StandardLegend(cols_fc, limits_fc, fireCount_seasons[[1]], 0.9, transpose = TRUE,
                   plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, extend_max = TRUE)

    text.units('counts/k~m2~', x = 0.33, y = 0.5, srt = 90)

    mapply(plotStandardMap, fireCount_seasons, title2 = 2002:2019,
           MoreArgs = list(cols = dcols_fc, limits = limits_dfc, left_text_adj = 0.1, regions = regions))

    StandardLegend(dcols_fc, limits_dfc, fireCount_seasons[[1]], 0.9, oneSideLabels = FALSE,
                   extend_min = TRUE, units = 'counts/k~m2~')
dev.off.gitWatermark()

variables = lapply(variables, brick)
#variables[[3]] = layer.apply(2:nlayers(variables[[3]]), function(i) variables[[3]][[i]] - variables[[3]][[i-1]])

#variables[[3]] = addLayer(variables[[3]][[1]], variables[[3]])

plotRegion <- function(region, name) {
    plot(c(2001, 2020), c(0, 1), axes = FALSE, xlab = '', ylab = '', type = 'n')
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
   
    mapply(plotVar, variables, line_cols, line = c(0, 0, 1, 2),
            side = c(2, 4, 4, 4), lty = c(1, 2, 1, 1), lwd = 2)
    plotVar(fireCount, 'red', lwd = 2.5, line = 2)
    mtext(name, side = 3, line = -1)
}

png("figs/region_INPUT_TS.png", height = 10, width = 21, units = 'in', res = 300)
par(mfrow = c(4, 1), mar = c(0, 4, 1, 4), oma = c(4, 0, 1, 0))
mapply(plotRegion, regions, names(regions))
axis(1, at = 2001:2020)
mtext(side = 1, 'Year', line = 2.5)
dev.off.gitWatermark()
