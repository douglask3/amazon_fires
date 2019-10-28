library(raster)
library(rasterExtras)
source("libs/plotStandardMap.r")
source("libs/sd.raster.r")
source("libs/filename.noPath.r")

file_obs = 'outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc'
dir_sim  = 'D:/amazon_fires/outputs/sampled_posterior_ConFire_solutions-firecount/constant_post_2018/'
fireMonths = 7:8

qs =seq(0, 1, 0.1)

cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
limits_fc = c(0, 1, 10, 100, 200, 400, 600, 800)

cols_pc = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
limits_pc = seq(0, 100, 10)

grab_cache = TRUE

months = lapply(fireMonths, seq, nlayers(dat), by = 12)
monthsByYr = lapply(1:min(sapply(months, length)), function(yr) sapply(months, function(m) m[yr]))


summeryFile <- function(file, addError = FALSE, ...) {
    dat = brick(file, ...)
    tfile = paste0('temp/', filename.noPath(file, noExtension = TRUE), 'fireSeason.nc')
    if (file.exists(tfile) && grab_cache) return(brick(tfile))
    fireSeasonYr <- function(mnths) dat = mean(dat[[mnths]])
    
    maps = layer.apply(monthsByYr, fireSeasonYr)
    maps = writeRaster(maps, file = tfile, overwrite = TRUE)
    return(maps)
}

obs = summeryFile(file_obs)

files_sim = list.files(dir_sim, full.names = TRUE)
sims = lapply(files_sim, summeryFile, varname = "burnt_area")

select_item <- function(i) layer.apply(sims, function(r) r[[i]])
quantile.raster <- function(x) quantile(x, probs = qs, na.rm = FALSE)

simrs =  lapply(1:nlayers(obs), select_item)

par(mfrow = c(2, 3), mar = rep(0, 4))

obs_last = obs[[nlayers(obs)]]
obs_mean = mean(obs)

obs_qs = obs
obs_qs[is.na(obs_qs)] = 10E9
obs_qs = raster::calc(obs_qs, quantile.raster)
obs_qs[obs_qs > 9E9] = NaN
obs_maps = list(obs_mean, obs_last, 100* sum(obs_last > obs_qs[[-1]])/(length(qs)-1))

sim_mean = do.call(mean, simrs) 
sim_last = tail(simrs, 1)[[1]]

sim_qs = lapply(sims, raster::calc, quantile.raster)
sim_qs = layer.apply(1:length(sim_qs), function(i) 100* sum(sim_last[[i]] >= sim_qs[[i]][[-1]])/(length(qs)-1))
sim_qs[sim_mean[[1]]>9E9] = NaN
sim_maps = list(sim_mean, sim_last, sim_qs)
cols = list(cols_fc, cols_fc,  cols_pc)
limits = list(limits_fc, limits_fc, limits_pc)
mapply(plotStandardMap, obs_maps, cols = cols, limits = limits)
mapply(plotStandardMap, sim_maps, cols = cols, limits = limits)