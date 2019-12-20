library(raster)
source("libs/make_transparent.r")
graphics.off()
layers = c(5, 95)
cols = c("tan", "#DDDD00", "red")

error_file = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018/fire_summary_precentile.nc'

uncert_file = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018/model_summary.nc'

obs = "outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc"

mnths = 1:227

fire_season = 8

fire_seasons = mnths[seq(fire_season, max(mnths), by = 12)]


temp_file_rs = paste0(c('temp/time_sries_fire_vars', layers), collapse = '-')
temp_file = paste0(temp_file_rs, '-Open.Rd')

openPost <- function(mnth, file, layer) {
    print(mnth)
    brick(file, level = mnth)[[layer]]
}

openPosts <- function(file)
    lapply(layers, function(i) layer.apply(mnths, openPost, file, i))

regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75),
               Central = -c(66.25, 53.75, 11.25, 6.25),
               Deep    = -c(71.25, 58.75, 6.25, 1.25),
               "All Deforested" = 'outputs/amazon_region/treeCoverTrendRegions.nc')
regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75))
if (file.exists(temp_file)) {
    load(temp_file) 
} else {
    error = openPosts(error_file)
    uncert = openPosts(uncert_file)
   
    save(error, uncert, file = temp_file)
}

obs = brick(obs)

cropMean <- function(r, extent, nme1, nme2, ...) {
    tfile = paste0(temp_file_rs, nme1, nme2, '.Rd')
    if (file.exists(tfile)) {
        load(tfile)
    } else {
        cropMeani <- function(ri) {
            if (is.character(extent)) {#
                r0 = ri
                mask = raster(extent) == 6
                ri[!mask] = 0
                ri = crop(ri, extent( -83.75, -50, -23.3, 0))
                
            } else {
                ri = crop(ri, extent(extent, ...))                
            }
            ri[ri>9E9] = 0
            ri[is.na(ri)] = 0 
            ri[ri<0] = 0
            
            unlist(layer.apply(ri, sum.raster))
        }
        r = sapply(r, cropMeani)     
        save(r, file = tfile)
    }
    return(r)
}

polygonCoords <- function(x, y, col, border = col, ...) {
    polygon(c(x, rev(x)), c(y[,1], rev(y[,2])), col = col, border = border, ...)
}

plotRegion <- function(extent, name) {
    tFile = paste(temp_file_rs, name, '.Rd')
    if (file.exists(tFile)) {
        load(tFile) 
    } else {
        error_r  = cropMean(error, extent, name, 'error')
        uncert_r  = cropMean(uncert, extent, name, 'uncert')
        obs_r = cropMean(list(obs, obs), extent, name, 'obs')
    }
    
    par(mar = c(1, 3, 0, 0))    
    maxY = max(error_r, uncert_r, obs_r)
    plot(c(12, max(mnths) + 2), c(0, maxY), xaxt = 'n', type = 'n', xaxs = 'i',
         xlab = '', ylab = '')
    mtext(name, adj = 0.1, line = -1.5)
   
    polygonCoords(mnths, error_r, cols[1], lwd = 2)
    polygonCoords(mnths, uncert_r, cols[2], lwd = 2)
    polygonCoords(mnths, obs_r, cols[3], lwd = 2)

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
    plot(obs_r, uncert_r, log = 'xy',
         xlim = range(obs_r,uncert_r, obs_r*1.3), ylim = range(obs_r,uncert_r, obs_r*1.3),
         xlab = '', ylab = '', yaxt = 'n')
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

png("figs/test_time_series.png", height = 7, width = 12, res = 300, units = 'in')


layout(t(matrix(1:8, nrow = 2)), widths = c(.75, 0.25))
par(oma = c(3, 1.2, 1, 1.2))

mapply(plotRegion, regions, names(regions))

mtext.units(outer = TRUE, side = 2, 'Fire counts (k~m-2~)', line = -1)
mtext(outer = TRUE, side = 4, 'Modelled anomolie')
mtext(side = 1, 'Observed anomolie', line = 2.5)
dev.off()
