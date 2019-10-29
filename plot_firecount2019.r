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


summeryFile <- function(file, addError = FALSE, tnameExt = 'asIs',  ...) {
    tfile = paste0('temp/', filename.noPath(file, noExtension = TRUE), tnameExt, 'fireSeason.nc')
    if (file.exists(tfile) && grab_cache) return(brick(tfile))
    
    dat = brick(file, ...)
    if (addError) {
        dat0 = dat
        mask = dat[[1]]<9E9
        for (i in 1:nlayers(dat))
            dat[[i]][mask] = dat[[i]][mask] + sample(-5000:5000, sum(mask[]), replace = TRUE, prob = dnorm(-5000:5000, 0, 132)) 
        
    }
    fireSeasonYr <- function(mnths) dat = mean(dat[[mnths]])
    
    maps = layer.apply(monthsByYr, fireSeasonYr)
    maps = writeRaster(maps, file = tfile, overwrite = TRUE)
    return(maps)
}

obs = summeryFile(file_obs)

files_sim = list.files(dir_sim, full.names = TRUE)
sims = lapply(files_sim, summeryFile, varname = "burnt_area")

select_item <- function(i, rs = sims) layer.apply(rs, function(r) r[[i]])
quantile.raster <- function(x) quantile(x, probs = qs, na.rm = FALSE)

#simrs =  lapply(1:nlayers(obs), select_item)

obs_last = obs[[nlayers(obs)]]
obs_mean = mean(obs)

obs_qs = obs
obs_qs[is.na(obs_qs)] = 10E9
obs_qs = raster::calc(obs_qs, quantile.raster)
obs_qs[obs_qs > 9E9] = NaN
obs_maps = list(obs_mean, obs_last, 100* sum(obs_last > obs_qs[[-1]])/(length(qs)-1))

sim_mean = simrs[[1]]
for (r in simrs[-1]) sim_mean = sim_mean + r#do.call(sum, simrs)
sim_mean = sim_mean/length(simrs) 
sim_last = tail(simrs, 1)[[1]]

sim_qs = lapply(sims, raster::calc, quantile.raster)
sim_qs = layer.apply(1:length(sim_qs), function(i) 100* sum(sim_last[[i]] >= sim_qs[[i]][[-1]])/(length(qs)-1))
sim_qs[sim_mean[[1]]>9E9] = NaN
sim_maps = list(sim_mean, sim_last, sim_qs)
cols = list(cols_fc, cols_fc,  cols_pc)
limits = list(limits_fc, limits_fc, limits_pc)
graphics.off()
pdf("figs/fireSeasonComaprison.pdf", height = 7, width = 5)#, res = 300, units = 'in')
    layout(rbind(c(1, 2, 2, 3), c(4, 5, 5, 6), c(7, 7, 8, 8), c(9, 10, 10, 11), 12),
           heights = c(1, 1, 0.3, 1, 0.3), widths = c(1, 0.9, 0.1, 1))
    mar = c(0, 0, 0, 0)
    par(mar = mar, oma = c(0, 1.2, 1.2, 0))
    mapply(plotStandardMap, obs_maps, cols = cols, limits = limits,
           title2 = c("Observations", "", ""),
           title3 = c("Average 2001-2019", "2019", "2019 Qunatile"))
    mapply(plotStandardMap, sim_maps, cols = cols, limits = limits, 
           title2 = c("Simulated", "", ""))

    StandardLegend <- function(cols, limits, dat, rightx = 0.99) 
        add_raster_legend2(cols, limits, dat = dat, add = FALSE,
                           transpose = FALSE, srt = 0, oneSideLabels= FALSE,
                           plot_loc = c(0.01, rightx, 0.3, 0.7),
                           ylabposScling = 1, extend_max = TRUE)
    par(mar = c(3, 0, 0, 0))                  
    StandardLegend(cols_fc, limits_fc, obs_maps[[1]], 0.9)
    StandardLegend(cols_pc, limits_pc, obs_maps[[3]])
    par(mar = mar)
    sims_e = lapply(files_sim, summeryFile, varname = "burnt_area", addError = TRUE, tnameExt = 'plusError')
    simrs_e =  lapply(1:nlayers(obs), select_item, sims_e)
    #sims_e = sims
    #simrs_e = simrs
    ppoint = layer.apply(1:nlayers(obs), function(i) 100*mean(obs[[i]] > simrs_e[[i]]))
    ppoint_maps = list(mean(ppoint), ppoint[[nlayers(ppoint)-1]], ppoint[[nlayers(ppoint)]])

    cols = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
    mapply(plotStandardMap, ppoint_maps,
           title2 = c("Probability of\nfire season", "", ""),
           title3 = c("Average 2001-2019", "2018", "2019"),
            MoreArgs = list(limits_error = c(0.25, 0.5), cols = cols, limits = limits_pc))

    par(mar = c(3, 0, 0, 0)) 
    StandardLegend(cols, limits_pc, obs_maps[[3]])
dev.off()

cell = 300#

plot(2001:2019, obs[cell], type = 'l', ylim = range(c(0, obs[cell])))
for (sim in sims_e) 
    lines(2001:2019, sim[cell], col = 'red')
    
lines(2001:2019, obs[cell], lty = 2)