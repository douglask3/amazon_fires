library(raster)
library(rasterExtras)
source("libs/plotStandardMap.r")
source("libs/sd.raster.r")

file_obs = 'outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc'
dir_sim  = 'D:/amazon_fires/outputs/sampled_posterior_ConFire_solutions-firecount/constant_post_2018/'
fireMonths = 7:8

qs = c(0.25, 0.5, 0.75)

cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
limits = c(0, 1, 10, 100, 200, 400, 600, 800)

summeryFile <- function(file, ...) {
    dat = brick(file, ...)

    months = lapply(fireMonths, seq, nlayers(dat), by = 12)

    months_last = sapply(months, tail, 1)
    months_last = months_last[1:which.max(months_last)]

    months = sort(unlist(months))

              
    #maps = list(mean(dat) * 12, mean(dat[[months]]) * length(fireMonths),mean(dat[[months_last]]) * length(fireMonths))
    maps = list(mean(dat), mean(dat[[months]]),mean(dat[[months_last]]))
                
    names(maps) = c("Annual", "Fire Season", "Fire Season 2019")  
    maps = lapply(maps, function(i) {i[i>9E9] = NaN; i})
    
    return(maps)
}

obs = summeryFile(file_obs)

files_sim = list.files(dir_sim, full.names = TRUE)
sims = lapply(files_sim[1:5], summeryFile, varname = "burnt_area")

select_item <- function(i) layer.apply(sims, function(r) r[[i]])
#quantile.raster <- function(x) quantile(x, probs = qs)
#simqs = lapply(1:3, function(i) raster::calc(select_item(i), quantile.raster))
simrs =  lapply(1:3, select_item)

par(mfrow = c(2, 3), mar = rep(0, 4))
mapply(plotStandardMap, obs, MoreArgs = list(cols = cols, limits = limits))
mapply(plotStandardMap, simrs, MoreArgs = list(cols = cols, limits = limits))
