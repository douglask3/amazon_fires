library(raster)
library(rasterExtras)
library(gitBasedProjects)
library(ncdf4)
source("libs/plotStandardMap.r")
source("libs/sd.raster.r")
source("libs/filename.noPath.r")
graphics.off()

file_obs = 'outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc'
dir_sim  = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018/'
fireMonths = 9

qs =seq(0, 1, 0.1)

cols_fc = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
            '#e31a1c','#bd0026','#800026')
limits_fc = c(0, 1, 10, 100, 200, 400, 600, 800)

cols_qs = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e',
            '#7a0177','#49006a')
limits_qs = 1:18

cols_pc = c('#fff5f0','#fee0d2','#fcbba1','#fc9272','#fb6a4a','#ef3b2c',
            '#cb181d','#a50f15','#67000d')
cols_pc = rev(c('#a50026','#d73027','#f46d43','#fdae61','#fee090',
                '#ddffdd','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'))
limits_pc = c(1, 5, 10, 20, 40, 60, 80, 90, 95, 99)

grab_cache = FALSE

dat = brick(file_obs)
months = lapply(fireMonths, seq, nlayers(dat), by = 12)
monthsByYr = lapply(1:min(sapply(months, length)),
                    function(yr) sapply(months, function(m) m[yr]))


summeryFile <- function(file, meanMonths = TRUE, ...) {
    
    tfile = paste0('temp/', filename.noPath(file, noExtension = TRUE),
                   meanMonths,  'fireSeason.Rd')
    print(tfile)
    if (file.exists(tfile) && grab_cache) {
        load(tfile)
        return(maps)
    }
    
    dat = brick(file, ...)
    fireSeasonYr_mean <- function(mnths) mean(dat[[mnths]])
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
obs_maps = list(obs_mean, obs_last, qrs(obs))


files_sim = list.files(dir_sim, full.names = TRUE)
files_sim = files_sim[grepl('sample_', files_sim)][1:10]

sims = lapply(files_sim, summeryFile, varname = "burnt_area")

tfile = paste0('temp/simsConv2Yrs_summary-', length(sims), '.Rd')
if (file.exists(tfile) && grab_cache) {
    load(tfile)
} else {
    simrs =  lapply(1:nlayers(obs), select_item, sims)
    sim_mean = mean.listedRasters(simrs)
    sim_last = tail(simrs, 1)[[1]]
    sim_qrs = layer.apply(sims, qrs)
    sim_qrs[[1]][is.na(obs[[1]])] = NaN
    sim_maps = list(sim_mean, sim_last, sim_qrs)
    save(simrs, sim_mean, sim_last, sim_qrs, sim_maps, file = tfile)
}

cols = list(cols_fc, cols_fc,  cols_qs)
limits = list(limits_fc, limits_fc, limits_qs-0.5)
graphics.off()
png(paste0("figs/fireSeasonComaprison", paste(fireMonths, collapse = "-"), ".png"),
    height = 9.8, width = 5, res = 300, units = 'in')

    layout(rbind(c(1, 2, 2, 3),
                 c(4, 5, 5, 6),
                 c(7, 7, 8, 8),
                 c(9, 10, 10, 11),
                 12,
                 c(13, 14, 14, 15),
                 16),
           heights = c(1, 1, 0.3, 1, 0.3, 1, 0.3), widths = c(1, 0.9, 0.1, 1))
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
     
        ppoint_maps = list(mean(ppoint), ppoint[[nlayers(ppoint)]], ppoint[[nlayers(ppoint)-1]])
    
        if (sumDir == 'X') {
            title2 = c("Quantile of\nfire season", "", "")
            title3 = c("Average 2001-2019", "2019", "2018")
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
    }
    plot_ppoints('X', limits_pc-50, c(-50, limits_pc-50, 50), cols_pc)
    limits_pr = c(20, 40, 60, 80, 90, 95, 99)
    cols_pr = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
    plot_ppoints('Y', limits_pr - 50, c(0, limits_pr, 100), cols_pr)
    
dev.off()

writeRaster.gitInfo(ppoint_maps[[2]], 'outputs/quantilePosition_2019_firecount.nc', overwrite = TRUE)
