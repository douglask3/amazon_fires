## region picker
library(raster)
source("libs/plotStandardMap.r")

treeCover = 'outputs/Australia_region/vegetation/treecover-2001-June2018.nc'
fireCount = 'outputs/Australia_region/firecount_SE_Aus_2001_onwards.nc'

limits_fc = c(0, 100, 500, 1000, 2000, 5000, 10000, 20000)
cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
            '#e31a1c','#bd0026','#800026')

limits_dfc = c(-50000, -10000, -5000, -1000, -100, -10, -1,
                1, 10, 100, 1000, 2000, 10000, 50000)
dcols_fc =rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))

limits_tree = c(0, 1, 2, 5, 10, 20, 30)
cols_tree = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')


fireSeason = 11:12

fireSeasons = lapply(2:19, function(i) (i-1) * 12 + fireSeason)


treeCover = mean(brick(treeCover))  * 100
fireCount = brick(fireCount)

fireCount_mean = mean(fireCount)

fireCount_seasons = lapply(fireSeasons, function(i) mean(fireCount[[i]]))
fireCount_season_mean = mean(fireCount[[unlist(fireSeasons)]])

fireCount_seasons = lapply(fireCount_seasons, '-', fireCount_season_mean)

par(mfrow = c(5, 4), mar = rep(0, 4))

plotStandardMap(treeCover, title2 = 'Tree Cover', cols = cols_tree, limits = limits_tree)

plotStandardMap(fireCount_season_mean, title2 = 'Average fire season', cols = cols_fc, limits = limits_fc)

mapply(plotStandardMap, fireCount_seasons, title2 = 2002:2019,
      MoreArgs = list(cols = dcols_fc, limits = limits_dfc))


