library(raster)
library(rasterExtras)
source("libs/make_transparent.r")
source("libs/plotStandardMap.r")
source("libs/dev.off.gitInfo.r")
library(gitBasedProjects)
graphics.off()
layers = c(5, 95)
cols = c("tan", "#DDDD00", "red")

error_file = 'outputs/sampled_posterior_ConFire_solutions-RoSfirecount-Tnorm/constant_post_2018_full_2002_maxMaxT-MaxW/fire_summary_precentile.nc'

uncert_file = 'outputs/sampled_posterior_ConFire_solutions-RoSfirecount-Tnorm/constant_post_2018_full_2002_maxMaxT-MaxW/model_summary.nc'

obs = "outputs/Australia_region/firecount-SE_Aus_2001_onwards.nc"

liklihood = 'outputs/sampled_posterior_ConFire_solutions-RoSfirecount-Tnorm/constant_post_2018_full_2002_maxMaxT-MaxW/fire_summary_observed_liklihood.nc'


mnths = 1:228

fire_season = 12

fire_seasons = mnths[seq(fire_season, max(mnths), by = 12)]


temp_file_rs = paste0(c('temp/time_sries_fire_vars-ROS-TMAX-WMAX', layers), collapse = '-')
temp_file = paste0(temp_file_rs, '-Open.Rd')

fname = "figs/test_time_series-ROS-TNAX-WMAX.png"

grab.cache = FALSE

openPost <- function(mnth, file, layer, ...) {
    print(mnth)
    brick(file, level = mnth, ...)[[layer]]
}

openPosts <- function(file, ...)
    lapply(layers, function(i) layer.apply(mnths, openPost, file, i, ...))


regions = list('Gold Coast, Northern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi,  Blue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25),
               'Nadgee, Wallagaraugh River' = c(146.25, 148.75, -38.75, -36.25),
               'Kangaroo Island, Adelaide' = c(136.25, 138.75, -36.25, -33.75))


regions = list('SE Aus temperate BL woodland' = 'outputs/SE_TempBLRegion.nc',
               'Gold Coast, Northern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi,  Blue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25))

if (file.exists(temp_file) && grab.cache) {
    load(temp_file) 
} else {
    error = openPosts(error_file)
    uncert = openPosts(uncert_file, varname = "burnt_area")
    save(error, uncert, file = temp_file)
}

obs = brick(obs)
p_value = brick(liklihood, varname = "variable")
liklihood = brick(liklihood, varname = "variable_0")

cropMean <- function(r, extent, nme1, nme2, ...) {
    tfile = paste0(temp_file_rs, nme1, nme2, '.Rd')
    if (file.exists(tfile) && grab.cache) {
        load(tfile)
    } else {
        cropMeani <- function(ri) {
            if (is.character(extent)) {#
                r0 = ri
                mask = is.na(raster(extent) )
                ri[mask] = 0
                
            } else {
                ri = crop(ri, extent(extent, ...))                
            }
            ri[ri>9E9] = 0
            ri[is.na(ri)] = 0 
            ri[ri<0] = 0
            
            unlist(layer.apply(ri, mean.raster))
        }
        r = sapply(r, cropMeani)     
        save(r, file = tfile)
    }
    return(r)
}

polygonCoords <- function(x, y, col, border = col, ...) {
    #y[y<0.00001]=0.00001
    polygon(c(x, rev(x)), c(y[,1], rev(y[,2])), col = col, border = border, ...)
}

plotRegion <- function(extent, name, last) {
    tFile = paste(temp_file_rs, name, '.Rd')
    if (file.exists(tFile) && grab.cache) {
        load(tFile) 
    } else {
        obs_r = cropMean(list(obs, obs), extent, name, 'obs')
        obs_p = cropMean(list(p_value), extent, name, 'obs_p1')
        obs_l = cropMean(list(liklihood), extent, name, 'obs_l')
        error_r  = cropMean(error, extent, name, 'error')
        uncert_r  = cropMean(uncert, extent, name, 'uncert')
    }
    
    par(mar = c(1, 3, 0, 2))    
    maxY = max(error_r, uncert_r, obs_r)
    
    plot(c(12, max(mnths) + 2), c(0.0, maxY), xaxt = 'n', type = 'n', xaxs = 'i',
         xlab = '', ylab = '')
    if (last)  axis(1, at = seq(1, length(mnths), 12),
                    labels = seq(2001, length.out = floor(length(mnths)/12)))
    mtext(name, adj = 0.1, line = -1.5)
   
    polygonCoords(mnths, error_r, cols[1], lwd = 2)
    polygonCoords(mnths, uncert_r, cols[2], lwd = 2)
    polygonCoords(mnths, obs_r, cols[3], lwd = 2)

    labels = seq(0, 1, 0.2)
    obs_l = (1 + obs_l)*maxY/2
    

    lines(mnths, obs_l, col = make.transparent("black", 0.67), lwd = 1.5)
    #lines(mnths, obs_l, col = make.transparent("black", 1 - obs_p))
    
    test2 = obs_p > 0.99
    test1 = obs_p > 0.95
    points(mnths[test1], obs_l[test1], pch = 19, cex = 2)
    points(mnths[test2], obs_l[test2], pch = 4 , cex = 2)
    axis(4, at = (1 + labels)*maxY/2, labels = labels)
    
    if (name == 'D') {
         at = seq(1, max(mnths), by = 12)
        axis(1, at = at, labels = 2001 + round(at/12))

        legend('top', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                text.col = cols, horiz = TRUE, bty = 'n')
    }

    ratio = apply(uncert_r, 2, function(i) obs_r[,1]/i)[,c(2,1)][fire_seasons,]
    #polygonCoords(mnths[fire_seasons], ratio fire_seasons,
    #              make.transparent("black", 0.7), lwd = 2)

    ratioS = ratio + max(ratio)
    ratioS = cbind(fire_seasons, ratioS * maxY / max(ratioS))
    arrowFun <- function(x) arrows(x[1], x[2], x[1], x[3], code = 3, angle = 90)
    #apply(ratioS, 1,  arrowFun)
    
    
    climScale <- function(r) {
        
        for (mn in 1:12) {
            index = seq(mn, max(mnths), by = 12)
            r[index,] = r[index,] / mean(r[index,])
        }
        r
    }   
    obs_r = climScale(obs_r+0.0001)
    uncert_r = climScale(uncert_r+ 0.0001)

    obs_r = obs_r[fire_seasons,]
    uncert_r = uncert_r[fire_seasons,]

    cols_years = make_col_vector(c("#161843", "#FFFF00", "#a50026"), ncols = 19)
    par(mar = c(3, 0.5, 0, 3))  
    plot(obs_r, uncert_r,
         xlim = range(obs_r,uncert_r, obs_r*1.3), ylim = range(obs_r,uncert_r, obs_r*1.3),
         xlab = '', ylab = '', yaxt = 'n', log = 'xy')
    axis(4)
    lines(x = c(0.0001, 9E9), y = c(0.0001,9E9), col = "black", lty = 2)
    
    
    points(obs_r[,1], uncert_r[,1], pch = 19, col = cols_years)
    points(obs_r[,2], uncert_r[,2], pch = 19, col = cols_years)

    apply(cbind(obs_r[,], uncert_r[,]), 1,
          function(i) lines(c(i[1], i[2]), c(i[3], i[4])))
    
    for (i in c(5, 8, 10, 19)) {
        text(obs_r[i,1], mean(uncert_r[i,], 1), 2000 + i, srt = -90, adj = c(0.5, -0.5))
    }
}

png(fname, height = 7.3, width = 12, res = 300, units = 'in')


layout(rbind(t(matrix(1:8, nrow = 2)), 9), widths = c(.75, 0.25), heights = c(1, 1, 1, 1, 0.1))
par(oma = c(3, 1.2, 1, 1.2))

last = c(rep(FALSE, length(regions)-1), TRUE)
mapply(plotRegion, regions, names(regions), last)

mtext.units(outer = TRUE, side = 2, 'Fire counts (k~m-2~)', line = -1)
mtext(outer = TRUE, side = 4, 'Modelled anomolie')
mtext(side = 1, 'Observed anomolie', line = 2.5)

par(mar = rep(0, 4))
plot(0, axes = FALSE, type = 'n')
dev.off.gitWatermark(comment = 'Note - meteorological input data for 2019/2020 fire season \nso recycled from 2018/2019')

