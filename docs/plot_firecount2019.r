library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/plotStandardMap.r")
source("libs/sd.raster.r")
source("libs/filename.noPath.r")
graphics.off()

file_obs = 'outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc'
dir_sim  = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018_2/'

file_obs = "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc" 
dir_sim  = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/'

fireMonths = 6:8
#fireMonths = 9

qs =seq(0, 1, 0.1)

cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
            '#e31a1c','#bd0026','#800026')
#cols_fc = c('#ffff99','#fed976','#fd8d3c','#e31a1c','#800026')
limits_fc = c(0, 1, 10, 50, 100, 200, 400, 600)
limits_fc = c(0, 10, 50, 100, 200, 500, 750, 1000)
limits_fc = c(0, 0.001, 0.01, 0.1, 0.2, 0.5, 1, 2)

dcols_fc =rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
dlimits_fc = c(-500, -200, -100,  -50, -10, 10, 50, 100, 200, 500)
dlimits_fc = c(-1, -0.5,  -0.1, -0.01, -0.001, 0.001, 0.01, 0.1, 0.2,  1)

cols_qs = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e',
            '#7a0177','#49006a')
limits_qs = 1:17

cols_pc = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c',
            '#cb181d','#a50f15','#67000d')
cols_pc = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                '#ddffdd','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
limits_pc = c(1, 5, 10, 20, 40, 60, 80, 90, 95, 99)

grab_cache = F

range.single <- function(x) if (length(x)==1) return(x) else return(range(x))
title3 = paste('Average',
               paste(substr(month.name[range.single(fireMonths)], 1, 3), collapse = ' - '),
               '2002-2019')

mod_summ_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area-Tnorm/constant_post_2018_full_2002-attempt3//model_summary.nc'

dat = brick(file_obs)
months = lapply(fireMonths, seq, nlayers(dat), by = 12)
monthsByYr = lapply(1:min(sapply(months, length)),
                    function(yr) sapply(months, function(m) m[yr]))

monthsByYr = monthsByYr[-1]

summeryFile <- function(file, meanMonths = TRUE, ...) {
    
    tfile = paste0('temp/', filename.noPath(file, noExtension = TRUE),
                   meanMonths,  'fireSeason.Rd')
    print(tfile)
    if (file.exists(tfile) && grab_cache) {
        load(tfile)
        return(maps)
    }
    
    dat = brick(file, ...)
    fireSeasonYr_mean <- function(mnths) sum(dat[[mnths]])
    fireSeasonYr      <- function(mnths)     (dat[[mnths]])
    
    if (meanMonths)  maps = layer.apply(monthsByYr, fireSeasonYr_mean)
        else maps = lapply(monthsByYr, fireSeasonYr)

    save(maps, file = tfile)
    #maps = writeRaster(maps, file = tfile, overwrite = TRUE)
    return(maps)
}

qrs <- function(rs)
    sum(rs[[nlayers(rs)]] > rs[[-nlayers(obs)]])   

mean.listedRasters <- function(rs) {
    r_mean = rs[[1]]
    for (r in rs[-1]) r_mean = r_mean + r
    r_mean = r_mean/length(rs) 
    return(r_mean)
}

select_item <- function(i, rs = sims) layer.apply(rs, function(r) r[[i]])    

obs = summeryFile(file_obs)
obs_last = obs[[nlayers(obs)]]
obs_mean = mean(obs)
obs_maps = list(obs_mean*100, (obs_last - obs_mean)*100, qrs(obs))


files_sim = list.files(dir_sim, full.names = TRUE)
files_sim = files_sim[grepl('sample_', files_sim)][1:10]

sims = lapply(files_sim, summeryFile, varname = "burnt_area")

tfile = paste0('temp/simsConv2Yrs_summary-', length(sims), '.Rd')
if (file.exists(tfile) && grab_cache) {
    load(tfile)
} else {
    
    simrs = lapply(monthsByYr, function(mns) {
        openMnPc <- function(mn, pc)
            brick(mod_summ_file, varname = "burnt_area", level = mn)[[pc]]
        
        openPc <- function(pc) sum( layer.apply(mns, openMnPc, pc))
        out = layer.apply(c(5, 50, 95), openPc)
    })
        #brick(mod_summ_file, varname = "burnt_area", level = mns)[[c(5, 50, 95)]])
    
    sim_mean = mean.listedRasters(simrs)
    sim_mean[is.na(obs_mean)] = NaN
    
    sim_last = tail(simrs, 1)[[1]]

    sim_qrs = layer.apply(1:3, function(i)                  
                sum(layer.apply(simrs, function(r) sim_last[[i]] > r[[i]])))
    
    sim_qrs[[1]][is.na(obs[[1]])] = NaN

    #browser()
    sim_qrs[is.na(sim_mean)] = NaN
    sim_maps = list(sim_mean*100, (sim_last - sim_mean)*100, sim_qrs)
    save(simrs, sim_mean, sim_last, sim_qrs, sim_maps, file = tfile)
}

cols = list(cols_fc, dcols_fc,  cols_qs)
limits = list(limits_fc, dlimits_fc, limits_qs-0.5)
graphics.off()
png(paste0("figs/fireSeasonComaprison", paste(fireMonths, collapse = "-"), ".png"),
    height = 200 * 2.33/4.67, width = 183, res = 300, units = 'mm')

    #layout(rbind(c(1, 2, 2, 3),
    #             c(4, 5, 5, 6),
    #             c(7, 7, 8, 8),
    #             c(9, 10, 10, 11),
    #             12,
    #             c(13, 14, 14, 15),
    #             16),
    #       heights = c(1, 1, 0.3, 1, 0.3, 1, 0.3), widths = c(1, 0.9, 0.1, 1))
    layout(rbind(c(1, 2, 2, 3),
                 c(4, 5, 5, 6),
                 c(7, 8, 8, 9)),       
                 heights = c(1, 1, 0.55), widths = c(1, 0.9, 0.1, 1))
                 #c(10, 11, 11, 12),
                 #13,
                 #c(14, 15, 15, 16),
                 #17),
           #heights = c(1, 1, 0.33, 1, 0.33, 1, 0.33), widths = c(1, 0.9, 0.1, 1))
    mar = c(1, 0, 0, 0)
    par(mar = mar, oma = c(0, 1.2, 1.2, 3))
    plotMapFun <- function(..., xaxt = TRUE) {
        plotStandardMap(..., ylim = c(-23.5, 8))
        if (xaxt) {
            axis(1)
            axis(1, at = c(-80, 180))
        }
    }

    mapply(plotMapFun, obs_maps, cols = cols, limits = limits,
           title2 = c("Observations", "", ""),
           title3 = c(title3, "     Difference in 2019", "No. years exceeded"), xaxt = FALSE)
    
    axis(4)
    axis(4, at = c(-180, 180))
    mapply(plotMapFun, sim_maps, cols = cols, limits = limits, 
           title2 = c("Simulated", "", ""))
    axis(4) 
    axis(4, at = c(-180, 180))
    par(mar = c(1.5, 0, 1.5, 0))                  
    StandardLegend(cols_fc, limits_fc, obs_maps[[1]], 0.9, oneSideLabels = FALSE, units = '%')
    #mtext('fire counts', side = 1, line = -2.5)
    
    StandardLegend(dcols_fc, dlimits_fc, obs_maps[[1]], 0.9, extend_min = TRUE, oneSideLabels = FALSE, units = '%')
    #mtext('fire counts', side = 1, line = -3.8, adj = -0.65, xpd = TRUE)
    mtext('Burnt area (%)', side = 1, line = 1.5, adj = -0.85, xpd = TRUE, cex = 0.8)

    labelss = limits_qs
    labelss[seq(1, length(labelss), 2)] = ''
    StandardLegend(cols_qs, limits_qs, obs_maps[[3]],
                   labelss = c('0', labelss),
                   adj = 0,extend_max = FALSE)
    mtext('No. years', side = 1, line = 1.5, adj = 0.65, cex = 0.8)
    par(mar = mar)
    
    obs_slt = summeryFile(file_obs, FALSE)
    
    open_simqs <- function(yrs) {       
        openYr <- function(yr){            
            out = brick(paste0(dir_sim, "/fire_summary_frequancy_of_counts.nc"), level = yr)
            nms = names(out)
            out[[1]][out[[1]] > 9E9] = NaN
            out = out / sum(out)
            names(out) = nms
            return(out)
        }        
        out = lapply(yrs, openYr)
        return(out)
    }
    tfile = 'temp/fire_summary_precentile.nc'
    if (file.exists(tfile) & grab_cache) {
        load(tfile)
    } else {
        sim_qrs = lapply(monthsByYr, open_simqs)
        save(sim_qrs, file = tfile)
    }
    
    counts = floor(as.numeric(sapply(names(sim_qrs[[1]][[1]]),
                   function(i) strsplit(i, "X")[[1]][2])))
    diffC = diff(counts[1:2])

    YearBeat <- function(yr, obs, sim, sumDir = 'X') {
        obs = diffC*round(obs/diffC)
        tObs <- function(i) {
            ob = obs[[i]]
            ob[ob < 1] = 1
            sm = sim[[i]]
            vtest <- function(ov, sv) {
                if (is.na(ov)) return(NaN)
                index = which(counts == ov)+1 
                
                if (sumDir == 'X') return(sum(sv[1:index]))    
                sum(sv[sv>sv[index]])        
            }
            
            out = ob
            out[]  = mapply(vtest, ob[],data.frame(t(values(sm))))
            return(out)
            #function(i) 100*mean(obs[[i]] > (sim[[i]]))-50
        }

        transalteUnits <- function(i) (mean(i) * 100) -50
        tfile = paste('temp/BeatYear', yr, sumDir, '.nc', sep = '-')
        if (file.exists(tfile) && FALSE) return(transalteUnits(brick(tfile)))
        print(tfile)
        out = layer.apply(1:nlayers(obs), tObs )
        out = writeRaster(out, tfile, overwrite = TRUE)
        return(transalteUnits(out))
    }

    plot_ppoints <- function(sumDir = 'X', limits, labelss, cols) {
        ppoint = mapply(YearBeat, 1:length(obs_slt), obs_slt, sim_qrs, sumDir = sumDir)
        ppoint = layer.apply(ppoint, function(i) i)
        
        ppoint_maps = list(mean(ppoint), ppoint[[nlayers(ppoint)]], ppoint[[11]])
    
        if (sumDir == 'X') {
            title2 = c("Quantile of\nfire season", "", "")
            title3 = c(title3, "2019", "2011") 
        } else {
            title2 = c("Probability of\nfire season", "", "")
            title3 = c("", "", "")      
        }
        mapply(plotStandardMap, ppoint_maps,
               title2 = title2,
               title3 = title3,
                MoreArgs = list(limits_error = c(0.25, 0.5),
                                cols = cols, limits = limits))

        
        par(mar = c(3, 0, 0, 0)) 
        StandardLegend(cols, limits, ppoint_maps[[3]], extend_max = FALSE,
                       labelss = labelss,
                       rightx = 0.9, units = '%')
        par(mar = mar) 
        return(ppoint_maps)
    }
    dev.off()
    ppoint_maps = plot_ppoints('X', limits_pc-50, c(-50, limits_pc-50, 50), cols_pc)
    limits_pr = c(20, 40, 60, 80, 90, 95, 99)
    cols_pr = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
    plot_ppoints('Y', limits_pr - 50, c(0, limits_pr, 100), cols_pr)
    
dev.off()

writeRaster.gitInfo(ppoint_maps[[2]], 'outputs/quantilePosition_2019_firecount.nc', overwrite = TRUE)


