library(raster)
source("libs/make_transparent.r")
source("libs/plotStandardMap.r")
library(rasterExtras)
graphics.off()
layers = c(5, 95, 50)
cols = c("tan", "#DDDD00", "red")

error_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/fire_summary_precentile.nc'

uncert_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/model_summary.nc'

#error_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area-Tnorm/constant_post_2018_full_2002-attempt3/fire_summary_precentile.nc'

#uncert_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area-Tnorm/constant_post_2018_full_2002-attempt3/model_summary.nc'   

obs = "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc"

#error_file = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018/fire_summary_precentile.nc'
 
#uncert_file = 'outputs/sampled_posterior_ConFire_solutions-firecount-Tnorm/constant_post_2018/model_summary.nc'
 
#obs = "outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc"


mnths = 1:228

fire_season = 8:9

#fire_seasons = mnths[seq(fire_season, max(mnths), by = 12)]
fire_seasons = sapply(fire_season, function(m) mnths[seq(m, max(mnths), by = 12)])

temp_file_rs = paste0(c('temp/time_sries_fire_vars_BA_newMoist10', layers), collapse = '-') 
temp_file_rs = paste0(c('temp/time_sries_fire_vars-DeepSoil15', layers), collapse = '-') 
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

regions = list("A Acre & Southern Amazonas" = -c(71.25, 63.75, 11.25,  6.25),
               "B Northern Mato Grosso" = -c(61.25, 53.75, 11.25,  6.25),  
               "C Maranhão and Piauí" = -c(48.25, 43.25,  8.75,  1.25),
               "D Bolivia" = -c(66.25, 58.75, 18.75, 13.75),   
               "E Paraguay" = -c( 61.25, 53.75, 23.75, 18.75), 
               "F Area of Active Deforesation" = 'outputs/amazon_region/treeCoverTrendRegions.nc')
 
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
    y = y[1:length(x),]
    polygon(c(x, rev(x)), c(y[,1], rev(y[,2])), col = col, border = border, ...)
}

plotRegion <- function(extent, name) {
    tFile = paste(temp_file_rs, name, '.Rd')
    if (file.exists(tFile)) {
        load(tFile) 
    } else {
        error_r  = cropMean(error, extent, name, 'error')
        uncert_r  = cropMean(uncert, extent, name, 'uncert')
        obs_r = cropMean(list(obs, obs, obs), extent, name, 'obs')
    }
    
    par(mar = c(1.5, 3, 0, 0))    
    maxY = max(error_r, uncert_r, obs_r)
    if (name == 'A') maxY = 0.1
    plot(c(12, max(mnths) + 2), c(0, maxY), xaxt = 'n', yaxt = 'n', type = 'n', xaxs = 'i',
         xlab = '', ylab = '')
    if (name == "B") axis(side = 2, at = c(0, 0.02, 0.04, 0.06, 0.08, 0.1))  else axis(side = 2)
     
    addGrid <- function(col = "grey") {
        lapply(fire_seasons[,1], function(i) lines(c(i, i), c(-9E9, 9E9), col = col, lty = 3))
        grid(ny = NULL, nx = 0, col = col)
    }
    addGrid()
    mtext(name, adj = 0.1, line = -1.5)   
    #grid(nx = -1 + length(mnths)/12, ny = NULL)  
    #polygonCoords(mnths, obs_r, cols[3], lwd = 2)  
    polygonCoords(mnths, error_r-0.002, cols[1], lwd = 2)  
    polygonCoords(mnths, obs_r, cols[3], lwd = 1)  
    polygonCoords(mnths, uncert_r, cols[2], lwd = 2)  
    polygonCoords(mnths, obs_r, cols[3], lwd = 1, lty = 3)  
    addGrid(col = make.transparent("black", 0.9)   )
    if (grepl("All Deforested", name)) {
        for (mn in 1:ncol(fire_seasons)) {
            y = obs_r[tail(fire_seasons[,mn], 1),1]
            lines(c(-9E9, 9E9), c(y, y), col = "black", lty = 2, lwd = 0.5)
        }
    }
    #grid(col = make.transparent("black", 0.9), nx = -1 + length(mnths)/12, ny = NULL)
    mtext(name, adj = 0.1, line = -1.5, col = make.transparent("black", 0.5))   

    if (name == tail(names(regions),1)) {
         at = seq(1, max(mnths), by = 12)
        axis(1, at = at, labels = 2001 + round(at/12))
    }
    #if (name == head(names(regions),1) ) {        
        
      #  }

    climScale <- function(r) {        
        for (mn in 1:12) {
            index = seq(mn, max(mnths), by = 12)
            r[index,] = r[index,] / mean(r[index,3])
        }
        r = r[,1:2]
        
        r[r < 0.01] = 0.01
        r
    }   
    obs_r = climScale(obs_r)
    uncert_r = climScale(uncert_r)
    error_r  = climScale(error_r)    

    cols_years = make_col_vector(c("#161843", "#FFFF00", "#a50026"), ncols = 19)
    par(mar = c(1.5, 1, 0, 0.5))  
    dumbell <- function(fire_seasonsi) {
        fire_seasonsi = as.matrix(fire_seasonsi)  
        findFSvalues <- function(r)
            t(apply(fire_seasonsi, 1, function(fs) apply(r, 2, function(i) mean(i[fs]))))
        
        obs_r    = findFSvalues(obs_r   )  
        uncert_r = findFSvalues(uncert_r)
        error_r  = findFSvalues(error_r )     

        plot(range(obs_r), range(uncert_r, error_r), log = 'xy',
             xlim = range(obs_r,uncert_r, obs_r*1.3), ylim = range(obs_r,uncert_r, obs_r*1.3),
             xlab = '', ylab = '', yaxt = 'n', cex = 1000, pch = 19, xaxt = 'n')
        axis(4, padj = -1)
        
        if (name == names(regions)[1])
            mtext(side = 3, month.name[fire_seasonsi[1,1]])
        if (name == 'A') axis(1, at = c(0.1, 0.2, 0.5, 1, 2, 5), padj = -1)
            else axis(1, padj = -1)
        lines(x = c(0.0001, 9E9), y = c(0.0001,9E9), col = "white", lty = 2)
        lines(x = c(0.0001, 9E9), y = c(1, 1), lty = 3, col = "white" )
       
        addDumbells(obs_r, error_r, uncert_r)         
        lines(x = c(0.0001, 9E9), y = c(0.0001,9E9),
              col = make.transparent("white", 0.67), lty = 2)
        lines(x = c(0.0001, 9E9), y = c(1, 1), lty = 3,
              col = make.transparent("white", 0.67 ) ) 
 
    }
    addDumbells <- function(x, ye, yu) {
        points(x[,1], yu[,1], pch = 19, col = cols_years, cex = 0.5)
        points(x[,2], yu[,2], pch = 19, col = cols_years, cex = 0.5) 
        cols_years = make.transparent(cols_years, 0.95)
        for (i in 1:20) {
        apply(cbind(x[,], yu[,], cols_years), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), lwd = 2.67, col = i[5]))   

        apply(cbind(x[,], ye[,], cols_years), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), col = i[5], lwd = 0.6))  
        apply(cbind(x[,], ye[,], cols_years), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), lwd = 0.2, lty = 0.5, col = "white")) 
        }
    } 
    apply(fire_seasons, 2, dumbell)
    if (name == tail(names(regions), 1)) {
        plot.new()
legend('topright', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                text.col = "black", horiz = TRUE, bty = 'n')
        legend('topright', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                text.col = make.transparent(cols, 0.4), horiz = TRUE, bty = 'n')  

        par(mar = c(0, 0, 0.67, 0))
        plot(c(2, 19), c(0, 1), type = 'n', axes = FALSE)
        
        addDumbells(cbind(1:19, 1:19), cbind(rep(0.0, 19), rep(1.0, 19)),
                    cbind(rep(0.3, 19), rep(0.7, 19)))
        text(x = c(2, 10, 19) , y = -0.1, c('2002', '2010', '2019'), xpd = NA)
        text(10, 0.5, 'Uncertainty', xpd = NA)
        text(10, c(0.9, 0.1), 'Error', xpd = NA) 

    }
        
    #for (i in c(4, 7, 10, 19)) {
    #    text(obs_r[i,1], mean(uncert_r[i,], 1), 2000 + i, srt = -90, adj = c(0.5, -0.5))
    #}
}

png("figs/test_time_series.png", height = 183 * 7/8 *6.7/5.7, width = 183, res = 300, units = 'mm')
    layout(t(matrix(c(1:20, 20), nrow = 3)), widths = c(.7, 0.15, 0.15),
            heights = c(1,1,1,1,1,1,0.7))
    par(oma = c(0.67, 1, 2, 2.5))

    mapply(plotRegion, regions, names(regions))

    mtext.units(outer = TRUE, side = 2, 'Burnt area (%)', line = -1)
    mtext(outer = TRUE, side = 4, 'Modelled anomaly', line = 1.2)
    mtext(side = 1, 'Observed anomaly', line = -5.5)
dev.off()
