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

cols_qs = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
limits_qs = 1:18

cols_pc = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15','#67000d')
cols_pc = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
limits_pc = c(0, 1, 5, 10, 20, 40, 60, 80, 90, 95, 99, 100)

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

qrs <- function(rs)
    sum(rs[[nlayers(rs)]] > rs[[-nlayers(obs)]])   

mean.listedRasters <- function(rs) {
    #browser()
    r_mean = rs[[1]]
    for (r in rs[-1]) r_mean = r_mean + r
    r_mean = r_mean/length(rs) 
    return(r_mean)
}

select_item <- function(i, rs = sims) layer.apply(rs, function(r) r[[i]])    

obs = summeryFile(file_obs)
obs_last = obs[[nlayers(obs)]]
obs_mean = mean(obs)
obs_maps = list(obs_mean, obs_last, qrs(obs))


files_sim = list.files(dir_sim, full.names = TRUE)
sims = lapply(files_sim, summeryFile, varname = "burnt_area")
#simrs =  lapply(1:nlayers(obs), select_item
sim_mean = mean.listedRasters(simrs)
sim_last = tail(simrs, 1)[[1]]
sim_qrs = layer.apply(sims, qrs)
sim_qrs[[1]][is.na(obs[[1]])] = NaN
sim_maps = list(sim_mean, sim_last, sim_qrs)

cols = list(cols_fc, cols_fc,  cols_qs)
limits = list(limits_fc, limits_fc, limits_qs-0.5)
graphics.off()
png("figs/fireSeasonComaprison.png", height = 7, width = 5, res = 300, units = 'in')
    layout(rbind(c(1, 2, 2, 3), c(4, 5, 5, 6), c(7, 7, 8, 8), c(9, 10, 10, 11), 12),
           heights = c(1, 1, 0.3, 1, 0.3), widths = c(1, 0.9, 0.1, 1))
    mar = c(0, 0, 0, 0)
    par(mar = mar, oma = c(0, 1.2, 1.2, 0))
    mapply(plotStandardMap, obs_maps, cols = cols, limits = limits,
           title2 = c("Observations", "", ""),
           title3 = c("Average 2001-2019", "2019", "No. years >"))
    mapply(plotStandardMap, sim_maps, cols = cols, limits = limits, 
           title2 = c("Simulated", "", ""))

    par(mar = c(3, 0, 0, 0))                  
    StandardLegend(cols_fc, limits_fc, obs_maps[[1]], 0.9)
    mtext('fire counts', side = 1, line = -2.5)
    labelss = limits_qs
    labelss[seq(1, length(labelss), 2)] = ''
    StandardLegend(cols_qs, limits_qs, obs_maps[[3]],
                   labelss = c('0', labelss),
                   adj = 0,extend_max = FALSE)
    mtext('No. years', side = 1, line = -2.5, adj = 0.75)
    par(mar = mar)
    grab_cache = FALSE
    sims_e = lapply(files_sim, summeryFile, varname = "burnt_area", addError = TRUE, tnameExt = 'plusError')
    #simrs_e =  lapply(1:nlayers(obs), select_item, sims_e)
    
    ppoint = layer.apply(1:nlayers(obs), function(i) 100*mean(obs[[i]] > simrs_e[[i]])-50)
    ppoint_maps = list(mean(ppoint), ppoint[[nlayers(ppoint)]], ppoint[[nlayers(ppoint)-1]])
    
    mapply(plotStandardMap, ppoint_maps,
           title2 = c("Probability of\nfire season", "", ""),
           title3 = c("Average 2001-2019", "2019", "2018"),
            MoreArgs = list(limits_error = c(0.25, 0.5), cols = cols_pc, limits = limits_pc-50))

    par(mar = c(3, 0, 0, 0)) 
    limits_pci = limits_pc
    limits_pci[1] = limits_pci[1] + 0.0001; limits_pci[length(limits_pci)] = limits_pci[length(limits_pci)] - 0.0001
    StandardLegend(cols_pc, limits_pc-50, ppoint_maps[[3]], labelss = limits_pc,#c(0, limits_pc, 100)
                   rightx = 0.9, extend_max = TRUE, extend_min = TRUE, units = '%')
dev.off()

writeRaster(ppoint_maps[[2]], 'outputs/quantilePosition_2019_firecount.nc', overwrite = TRUE)
