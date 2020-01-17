## region picker
library(raster)
library(rasterExtras)
source("libs/plotStandardMap.r")
graphics.off()
options(scipen = 999)

treeCover = 'outputs/Australia_region/vegetation/treecover-2001-June2018.nc'
fireCount = 'outputs/Australia_region/firecount_SE_Aus_2001_onwards.nc'

limits_fc = c(0, 100, 1000, 5000, 10000, 50000, 100000)/ 100000
cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
            '#e31a1c','#bd0026','#800026')

limits_dfc = c(-100000, -50000, -10000, -5000, -1000, -100, -10, 10, 100, 1000, 2000, 10000, 50000, 100000)/ 100000
dcols_fc =rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))

limits_tree = c(0, 1, 2, 5, 10, 20, 30)
cols_tree = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')


fireSeason = 11:12

regions = list('Wollemi & Blue Mountains' = c(149.75, 151.25, -36.25, -31.25),
               'Nadgee/Wallagaraugh River' = c(146.25, 151.25, -38.75, -36.25),
               'Kangaroo Island/Adelaide' = c(136.25, 138.75, -36.25, -33.75)) 


fireSeasons = lapply(2:19, function(i) (i-1) * 12 + fireSeason)


treeCover = mean(brick(treeCover))  * 100
fireCount = brick(fireCount) 

fireCount_mean = mean(fireCount)

fireCount_seasons = lapply(fireSeasons, function(i) mean(fireCount[[i]]))
fireCount_season_mean = mean(fireCount[[unlist(fireSeasons)]])

fireCount_seasons = lapply(fireCount_seasons, '-', fireCount_season_mean)
fireCount_seasons = lapply(fireCount_seasons, '/', raster::area(fireCount[[1]]))

png("figs/yearly_fires_map.png", height = 7, width = 7, units = 'in', res = 300)
    layout(t(matrix(c(1:22, 0, 0, 0, 23, 23, 0), nrow = 4)), heights = c(rep(1, 6), 0.3))
    par(mar = rep(0, 4), oma = c(2, 1, 1, 1))

    plotStandardMap(treeCover, title2 = 'Tree\ncover', cols = cols_tree, limits = limits_tree,
                    left_text_adj = 0.1, left_text_adj_line = -2.5)

    StandardLegend(cols_tree, limits_tree, fireCount_seasons[[1]], 0.9, transpose = TRUE,
                   plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, units = '%', maxLab = 100)

    plotStandardMap(fireCount_season_mean, title2 = 'Average\nfire\nseason', cols = cols_fc,
                    limits = limits_fc, left_text_adj = 0.1, left_text_adj_line = -3.5)
    StandardLegend(cols_fc, limits_fc, fireCount_seasons[[1]], 0.9, transpose = TRUE,
                   plot_loc = c(0.01, 0.95, 0.2, 0.25), srt =- -90, extend_max = TRUE)

    text.units('counts/k~m2~', x = 0.33, y = 0.5, srt = 90)

    mapply(plotStandardMap, fireCount_seasons, title2 = 2002:2019,
           MoreArgs = list(cols = dcols_fc, limits = limits_dfc, left_text_adj = 0.1, regions = regions))

    StandardLegend(dcols_fc, limits_dfc, fireCount_seasons[[1]], 0.9, oneSideLabels = FALSE,
                   extend_min = TRUE, units = 'counts/k~m2~')
dev.off()

plotRegion <- function(region, name) {
    plot(c(2001, 2020), c(0, 1), axes = FALSE, xlab = '', ylab = '', type = 'n')
    plotVar <- function(var, col, line = 0, side = 2) {
        y = unlist(layer.apply(raster::crop(fireCount, extent(region)), sum.raster))
        rangeY = range(y)
        
        scale0 <- function(x, rx) (x - rx[1]) / diff(rx)
        
        y = scale0(y, rangeY)
        lines(seq(2001, by = 1/12, length.out = length(y)), y, col = col, lwd = 1.5)
        
        labels = unique(signif(seq(rangeY[1], rangeY[2], length.out = 7), 1))
        if (sum(labels <= rangeY[2]) < 4) labels = unique(signif(seq(rangeY[1], rangeY[2], length.out = 7), 2))
        
        at = scale0(labels, rangeY)
        axis(side = side, at = at, labels = labels, line = line, col = col)
    }
    plotVar(fireCount, 'red')
    mtext(name, side = 3, line = -1)
}

par(mfrow = c(3, 1), mar = c(0, 4, 1, 4), oma = c(4, 0, 1, 0))
mapply(plotRegion, regions, names(regions))
axis(1, at = 2001:2020)
mtext(side = 1, 'Year', line = 2.5)