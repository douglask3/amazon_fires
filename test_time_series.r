library(raster)
library(rasterExtras)
source("libs/make_transparent.r")
source("libs/plotStandardMap.r")
source("libs/dev.off.gitInfo.r")
source("libs/standardGrid.r")
source("libs/YearlySeason.r")
library(gitBasedProjects)
graphics.off()
layers_full = c(1, 99)
layers_control = c(5, 95)
cols = c("tan", "#DDDD00", "red")

dir1 = 'outputs/sampled_posterior_ConFire_solutions3-RoSfirecount-Tnorm/'
dir2 = 'constant_post_2018_full_2002_maxMaxT-MaxW-NewMoist'

error_file = 'fire_summary_precentile.nc'
uncert_file = 'model_summary.nc'
liklihood = 'fire_summary_observed_liklihood.nc'

obs = "outputs/Australia_region/firecount-SE_Aus_2001_onwards.nc"



controls_name = c("standard_fuel", "standard_moisture", "standard_ignitions",
                  "standard_suppression", "variable")

controlLeg = c("Fuel", "Moisture", "Ignitions", "Suppression", "Rate of Spread")
control_cols = make.transparent(c("#4d9221", "#053061", "#67001f", "#c51b7d", "black"), 0.5)
control_cols[4] = make.transparent(control_cols[4], 0.5)
control_dens = c(NA, 30, 30, NA, NA)

mnths = 1:229

fire_season = 9:13

cols_years = make_col_vector(c("#161843", "#FFFF00", "#a50026"), ncols = 19)

#fire_seasons = mnths[seq(fire_season, max(mnths), by = 12)]
#fire_seasons = sapply(fire_season, seq, max(mnths), by = 12)
fire_seasons = YearlySeason(fire_season, 229, NULL)
fire_seasons = t(matrix(unlist(fire_seasons), nrow = length(fire_season)))

temp_file_rs = paste0(c('temp/', dir2, layers_full, layers_control), collapse = '-')
#temp_file_rs = paste0(c('temp/time_sries_fire_vars-ROS-TMAX-WMAX-full', layers), collapse = '-')
temp_file = paste0(temp_file_rs, '-Open.Rd')

error_file = paste(dir1, dir2, 'fire_summary_precentile.nc', sep = '/')
uncert_file = paste(dir1, dir2, 'model_summary.nc', sep = '/')
liklihood =paste(dir1, dir2,  'fire_summary_observed_liklihood.nc', sep = '/')

fname = paste0("figs/time_series-", dir2, '.png')

grab.cache = TRUE

openPost <- function(mnth, file, layer, ...) {
    print(mnth)
    brick(file, level = mnth, ...)[[layer]]
}

openPosts <- function(file, layers, ...)
    lapply(layers, function(i) layer.apply(mnths, openPost, file, i, ...))


regions = list('Gold Coast, Northern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi,  Blue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25))


regions = list('SE Aus temperate BL woodland' = 'outputs/Australia_region/SE_TempBLRegion.nc',
               'Gold Coast, Northern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi, Blue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25),
               'East Gippsland' = c(146.25, 148.75, -38.75, -36.25),
               'Kangaroo Island, Adelaide' = c(136.25, 138.75, -36.25, -33.75))


cropMean <- function(r, extent, nme1, nme2, ...) {
    tfile = paste0(temp_file_rs, nme1, nme2, '.Rd')
    if (file.exists(tfile) && grab.cache) {
        load(tfile)
    } else {
        cropMeani <- function(ri) {
            if (is.character(extent)) {#                
                r0 = ri
                mask = is.na(raster(extent) )
                ri[mask] = NaN
                
            } else {
                ri = crop(ri, extent(extent, ...))                
            }
            ri[ri>9E9] = NaN
            ri[is.na(ri)] = NaN 
            ri[ri<0] = NaN
            
            unlist(layer.apply(ri, mean.raster, na.rm = TRUE))
        }
        r = sapply(r, cropMeani)     
        save(r, file = tfile)
    }
    return(r)
}

polygonCoords <- function(x, y, col, border = col, maxY = NaN, ...) {
    if (!is.na(maxY) && max(y) > maxY) {
        labels = seq(0, signif(max(y), 1), length.out = 5)
        labels = labels[labels < max(y)]
        if (length(labels) < 5) labels = seq(0, signif(max(y), 1), length.out = 10)
        axis(4, at = labels * maxY/max(y), labels = labels)
        y = maxY * y/max(y)
    }
    y = y[1:length(x),]
    polygon(c(x, rev(x)), c(y[,1], rev(y[,2])), col = col, border = border, ...)
}

addDumbells <- function(x, y, cols) {
    points(x[,1], y[,1], pch = 19, col = cols_years, xpd = NA)
    points(x[,2], y[,2], pch = 19, col = cols_years, xpd = NA)

    apply(cbind(x[,], y[,]), 1,
        function(i) lines(c(i[1], i[2]), c(i[3], i[4]), xpd = NA))
}

if (file.exists(temp_file) && grab.cache) {
    load(temp_file) 
} else {
    error = openPosts(error_file, layers_full)
    uncert = openPosts(uncert_file, layers_full, varname = "burnt_area")
    controls = lapply(controls_name, function(i) openPosts(uncert_file, layers_control,
                                                           varname = i))
    save(error, uncert, controls, file = temp_file)
}

obs = brick(obs)
p_value = brick(liklihood, varname = "variable")
liklihood = brick(liklihood, varname = "variable_0")

plotRegion <- function(extent, name, last, plot_control = FALSE) {
    obs_l      = cropMean(list(liklihood), extent, name, 'obs_l2')
    obs_r      = cropMean(list(obs, obs), extent, name, 'obs')
    obs_p      = cropMean(list(p_value), extent, name, 'obs_p1')
    
    error_r    = cropMean(error, extent, name, 'error')
    uncert_r   = cropMean(uncert, extent, name, 'uncert')
    
    controls_r = mapply(cropMean, controls, nme2 = paste0(controls_name, '-CM'), 
                        MoreArgs = list(extent = extent, nme1 = name), SIMPLIFY = FALSE)
    
    par(mar = c(1, 3, 0, 2))  
    if (plot_control) maxY = 1.15  else maxY = max(error_r, uncert_r, obs_r)
    
    plot(c(12, max(mnths) + 2), c(0.0, maxY), xaxt = 'n', type = 'n', xaxs = 'i',
         xlab = '', ylab = '')
    standardGrid()
    mtext(name, adj = 0.1, line = -1.5)
   
    if (plot_control) {
        mapply(polygonCoords, y = controls_r, col = control_cols, density = control_dens,
               MoreArgs = list(x = mnths, lwd = 2, maxY = maxY))
    } else {
        polygonCoords(mnths, error_r, cols[1], lwd = 2)
        polygonCoords(mnths, uncert_r, cols[2], lwd = 2)
        polygonCoords(mnths, obs_r, cols[3], lwd = 2)    

        labels = seq(0, 1, 0.2)
        obs_l0 = obs_l
        obs_l = (1 + (1-obs_l))*maxY/2
        
        lines(mnths, obs_l, col = make.transparent("black", 0.67), lwd = 1.5)

        test2 = obs_p > 0.99
        test1 = obs_p > 0.95
        points(mnths[test1], obs_l[test1], pch = 19, cex = 2)
        points(mnths[test2], obs_l[test2], pch = 4 , cex = 2)
        axis(4, at = (1 + labels)*maxY/2, labels = labels)
    }
    if (name == tail(names(regions), 1)) {
        at = seq(1, max(mnths), by = 12)
        axis(1, at = at, labels = 2001 + round(at/12))
    }
    
    climScale <- function(r) {
        for (mn in 1:12) {
            index = seq(mn, max(mnths), by = 12)
            r[index,] = r[index,] / mean(r[index,])
        }
        r
    }   
    if (!plot_control) {
        obs_r = climScale(obs_r+0.0001)
        uncert_r = climScale(uncert_r+ 0.0001)

        selectSeason <-function(r)  t(apply(fire_seasons, 1, function(i) apply(r[i,], 2, mean)))
        
        
        obs_r = selectSeason(obs_r)
        uncert_r = selectSeason(uncert_r)

        
        par(mar = c(3, 0.5, 0, 3))  
        plot(obs_r, uncert_r,
            xlim = range(obs_r,uncert_r, obs_r*1.3), ylim = range(obs_r,uncert_r, obs_r*1.3),
            xlab = '', ylab = '', yaxt = 'n', log = 'xy')
        grid()
        axis(4)
        lines(x = c(0.0001, 9E9), y = c(0.0001,9E9), col = "black", lty = 2)
        addDumbells(obs_r, uncert_r, cols_years)
        
        for (i in c(5, 8, 10, 19)) {
            text(obs_r[i,1], mean(uncert_r[i,], 1), 2000 + i, srt = -90, adj = c(0.5, -0.5))
        }
    }
}

ploFun <- function(fname, plot_control = FALSE, ...) {
    png(fname, height = 9, width = 7.5, res = 300, units = 'in')
        if (plot_control) lmat = rbind(t(matrix(rep(1:6, each = 2), nrow = 2)))
            else lmat = rbind(t(matrix(1:12, nrow = 2)))
        
        layout(lmat, widths = c(.75, 0.25),
               heights = c(1, 1, 1, 1, 1, 0.3))
        par(oma = c(3, 1.2, 1, 1.2))

        last = c(rep(FALSE, length(regions)-1), TRUE)
        mapply(plotRegion, regions, names(regions), last,
               MoreArgs = list(plot_control = plot_control, ...))
        
        if (plot_control) {
            mtext.units(outer = TRUE, side = 2, 'Standard limitation from controls', line = -1)
            mtext(outer = TRUE, side = 4, 'Rate of spread factor')
                    
            plot.new()
            par(mar = c(1, 0, 1, 0))
            for (i in 1:5) {
                legend('center', controlLeg, density = control_dens,
                    fill = make.transparent(control_cols, 0.75), border = control_cols,
                    horiz = TRUE, bty = 'n')
            }
        } else {
            mtext.units(outer = TRUE, side = 2, 'Fire counts (k~m-2~)', line = -1)
            mtext(outer = TRUE, side = 4, 'Modelled anomolie')
            mtext(side = 1, 'Observed anomolie', line = 2.5)
            
            
            par(mar = c(1, 0, 1.5, 0))
            plot.new()
            legend('top', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                    text.col = cols, horiz = TRUE, bty = 'n', text.font = 2, xpd = NA)
           par(mar = c(1, 0, 1.5, 0))
            plot(c(0.5, 19.5), c(0, 1), type = 'n', axes = FALSE, xlab = '', ylab = '')
            addDumbells(cbind(1:19, 1:19), matrix(rep(c(0,1), 19), ncol = 2), cols_years)
            text(x = seq(1, 19, 6),y = 0,seq(1, 19, 6)+2000, srt = 90, adj = 1.2, xpd = NA)
        }
    dev.off.gitWatermark()
}

ploFun(paste0(fname, '.png'))
ploFun(paste0(fname, '-controls.png'), plot_control = TRUE)
