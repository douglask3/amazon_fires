library(raster)
source("libs/make_transparent.r")
source("libs/plotStandardMap.r")
library(rasterExtras)
graphics.off()
layersr = c(13, 87, 50)
layers0 = seq(10, 90)
cols = c("tan", "#DDDD00", "red")
cols_years = c("#161843", "#FFFF00", "#a50026")
cols_moist = rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))

dir = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/'
dir = "outputs/sampled_posterior_ConFire_solutions_squishy_full_crop/constant_post_2018_full_2002_BG2020_rev_logitnorm_Long_PT/"
dir = "outputs/sampled_posterior_ConFire_solutions_squishy_full_crop2/constant_post_2018_full_2002_BG2020_rev_logitnorm_Long_PT_EX/"

rDir = paste0(dir, "/regions3/")
#dir = "outputs/sampled_posterior_ConFire_solutions_squishy_full_crop/constant_post_2018_full_2002_BG2020_rev_logitnorm_Long_PT_POW2/"
error_file = paste0(dir, 'fire_summary_precentile.nc')

uncert_file = paste0(dir, 'model_summary.nc')

figName = "figs/logitnorm_test_time_series"
 
obs = "inputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc"
moistPlot = FALSE

mnths = 1:228

fire_season = 8:9

#fire_seasons = mnths[seq(fire_season, max(mnths), by2)]
fire_seasons = sapply(fire_season, function(m) mnths[seq(m, max(mnths), by = 12)])


temp_file_rs = paste0(c('temp/time_sries_fire_vars-DeepSoil16-rev-lonitnormal-', layersr), collapse = '-') 
temp_file = paste0(temp_file_rs, '-rev-logit5-Open.Rd')
grab_cache = FALSE
openPost <- function(mnth, file, layer, varname = NULL) {
    print(mnth)
    if (is.null(varname)) dat = brick(file, level = mnth)
    else dat = brick(file, varname = varname, level = mnth)
    
    return(dat[[layer]])
}

openPosts <- function(file, ..., layers = layersr)
    lapply(layers, function(i) layer.apply(mnths, openPost, file, i, ...))

regions = list(A = -c(71.25, 63.75, 11.25-2.5,  6.25-2.5),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75),
               Central = -c(66.25, 53.75, 11.25, 6.25),
               Deep    = -c(71.25, 58.75, 6.25, 1.25),
               "All Deforested" = 'outputs/amazon_region/treeCoverTrendRegions.nc')

regions = list("A Acre & Southern Amazonas" = -c(71.25, 63.75, 11.25,  6.25),
               "B Northern Mato GrossoSmall" = -c(61.25-2.5, 53.75, 11.25-2.5,  6.25),  
               "C Maranhão and Piauí" = -c(48.25, 43.25,  8.75,  1.25),
               "D Bolivia" = -c(66.25, 58.75, 18.75, 13.75),   
               "E Paraguay" = -c( 61.25, 53.75, 23.75, 18.75), 
               "F Area of Active Deforesation" = 'inputs/amazon_region/treeCoverTrendRegions.nc')
 
regions = list("A" = -c(66.25, 58.75, 11.25,  6.25),
               "B" = -c(56.25, 51.25, 8.75,  3.75),  
               "C" = -c(48.75, 43.75,  8.75,  1.25),
               "D" = -c(61.25, 58.75, 18.75, 13.75),   
               "E" = -c( 61.25, 56.25, 21.25, 18.75),
               "F Area of Active Deforesation" = 'inputs/amazon_region/treeCoverTrendRegions.nc')

regions = list("A" = -c(76.25, 71.25, 11.26, 6.25),
               "B" = -c(71.25, 66.25, 11.25, 6.25),
               "C" = -c(66.25, 61.25, 11.25,  6.25),
               "D" = -c(61.25, 56.25, 11.25, 3.75),
               "E" = -c(56.25, 51.25, 8.75,  3.75),  
               "F" = -c(51.25, 46.25,  8.75,  1.25),
               "G" = -c(61.25, 58.75, 18.75, 13.75),   
               "H" = -c( 61.25, 56.25, 21.25, 18.75),
               "I Area of Active Deforesation"
                        = 'inputs/amazon_region/treeCoverTrendRegions.nc')
regions = list("A Acre, Southern Amazonas & Peruvian border " = -c(76.25, 66.25, 11.25, 6.25),
               "B Rondônia & Northern Mato Grosso" = -c(66.25, 58.75, 11.25,  6.25),               
               "C Tocantins" = -c(58.75, 51.25, 8.75,  3.75),  
               "D Maranhão and Piauí" = -c(51.25, 46.25,  8.75,  1.25),
               "E Brazil/Bolivia/Paraguay Border" = -c(61.25, 56.25, 21.25, 13.75),
               "F Area of Active Deforesation" = 'inputs/amazon_region/treeCoverTrendRegions-F.nc',
               "G Humid forest" = 'inputs/amazon_region/treeCoverTrendRegions-G.nc',
               "H Eastern basin" = 'inputs/amazon_region/treeCoverTrendRegions-H.nc',
               "I Southern basin decidious forest" = 'inputs/amazon_region/treeCoverTrendRegions-I.nc')
 
#if (file.exists(temp_file) && grab_cache) {
#    load(temp_file) #
#} else {
    #error      = openPosts(error_file)
    #uncert     = openPosts(uncert_file)
    #uncert0    = openPosts(uncert_file, layers = layers0)
    #fuel       =  openPosts(uncert_file, varname = 'standard_fuel')
    #moist      =  openPosts(uncert_file, varname = 'standard_moisture')
    #ignitions   =  openPosts(uncert_file, varname = 'standard_ignitions')
    #suppression =  openPosts(uncert_file, varname = 'standard_suppression')
    #moist0     = openPosts(uncert_file, varname = 'standard_moisture', layers = layers0)
#    save(error, uncert, fuel, moist, ignitions, suppression, file = temp_file)
#}

obs = brick(obs)

cropMean <- function(r, extent, nme1, nme2, ...) {
    tfile = paste0(temp_file_rs, nme1, nme2, 'mean', '.Rd')
    if (file.exists(tfile)  && F ) {
        load(tfile)
    } else {
        cropMeani <- function(ri) {            
            if (is.character(extent)) {#
                r0 = ri
                
                mask = raster(extent) == 1
                #browser()
                ri[!mask] = NaN
                
                #ri = crop(ri, extent( -83.75, -50, -23.3, -10))
                
            } else {
                ri = crop(ri, extent(extent, ...))                
            }
           
            ri[ri>9E9] = NaN
            #ri[is.na(ri)] = 0 
            #ri[ri<0] = 0
            
            unlist(layer.apply(ri, mean.raster, na.rm = TRUE))
        }
        r = sapply(r, cropMeani)     
        #save(r, file = tfile)
    }
    return(r)
}

rfiles = list.files(rDir)
polygonCoords <- function(x, y, col, border = col, ...) {
    y = y[1:length(x),]
    polygon(c(x, rev(x)), c(y[,1], rev(y[,2])), col = col, border = border, ...)
}
#error = lapply(error, function(i) 1/(1+exp(i*(-1))))
plotRegion <- function(extent, name, legend = F) {
    rID = substr(name, 1, 1)
    openRegionF <- function(id, tranpose = TRUE) {
        files = paste0(rDir, rfiles[grepl(rID, substr(rfiles, 1, 1))])
        file = files[grepl(id, files, fixed = TRUE)]
        dat = read.csv(file) 
        #if (!is.null(sc)) 
        
        #print(file)
        #browser()
        #if (!grepl( 'anomolie', id)) dat = dat * 0.4
        #if (rID != "A" && rID != 'B' && !grepl( 'anomolie', id))

        if (dim(dat)[2] != 1) dat = dat[layers,]
        return(t(dat))
    }
    
    obs_r = obs_r0 = cropMean(list(obs, obs, obs), extent, name, 'obsMNNNN')
    uncert_r = openRegionF("uncert.")
    sc = 1#sum(obs_r[,3])/sum(uncert_r[,3])
    uncert_r = uncert_r * sc
    error_r = openRegionF("error.") * sc
    liki = openRegionF("fire_summary_observed_posterior_position", F)
    pvalue = openRegionF("fire_summary_p_value", F)
    
    #error_r o uncert_r
    #error_r   = cropMean(error, extent, name, 'errorN')
    #uncert_r  = cropMean(uncert, extent, name, 'uncertN')
    #uncert_r0 = cropMean(uncert0, extent, name, 'uncert0') 
          
    #fuel_r    = cropMean(fuel, extent, name, 'fuel') 
    #moist_r   = cropMean(moist, extent, name, 'moist4')      
    #ignitions_r =  cropMean(ignitions, extent, name, 'ignitions')   
    #suppression_r =  cropMean(suppression, extent, name, 'suppression')   
    #moist_r0  = cropMean(moist0, extent, name, 'moist0')
    
    #error_r = error_r[c(228, 1:227),]
    #uncert_r = uncert_r[c(228, 1:227),]
    #moist_r = moist_r
    if (moistPlot) par(mar = c(1.5, 3, 0, 3))   else par(mar = c(1.5, 3, 0, 0)) 
    if (moistPlot)  maxY = max(obs_r) else maxY = max(error_r, uncert_r, obs_r)
    #if (name == 'A') maxY = 0.1
    #dev.new()
    plot(c(12, max(mnths) + 2), c(0, maxY), xaxt = 'n', yaxt = 'n', type = 'n', xaxs = 'i',
         xlab = '', ylab = '')
    if (name == "B") axis(side = 2, at = c(0, 0.02, 0.04, 0.06, 0.08, 0.1))  else axis(side = 2)
    if (moistPlot && substr(name, 1, 1) == "D")
        mtext(side = 4, 'Standard moisture limitation (%)', line = 2)
    addGrid <- function(col = "grey") {
        lapply(fire_seasons[,1], function(i) lines(c(i, i), c(-9E9, 9E9), col = col, lty = 3))
        grid(ny = NULL, nx = 0, col = col)
    }
    addGrid()
    mtext(name, adj = 0.1, line = -1.5)   
    #grid(nx = -1 + length(mnths)/12, ny = NULL)  
    #polygonCoords(mnths, obs_r, cols[3], lwd = 2)  
     if (!moistPlot) polygonCoords(mnths, error_r, cols[1], lwd = 2)  
    polygonCoords(mnths, obs_r, cols[3], lwd = 1)  
    if (moistPlot) {
        maxY2 = max(c(fuel_r, moist_r, suppression_r, ignitions_r))
        plotLim <- function(lim, col) {
            lim = lim * maxY/maxY2
            polygonCoords(mnths, lim, col, lwd = 2)  
            polygonCoords(mnths, uncert_r, cols[2], lwd = 2)  
            polygonCoords(mnths, obs_r, cols[3], lwd = 1)  
            polygonCoords(mnths, lim, make.transparent(col, 0.8), lwd = 2)  
            
        }
        plotLim(fuel_r, "green")
        plotLim(moist_r, "blue")
        plotLim(ignitions_r, "purple")
        plotLim(suppression_r, "black")
        
        labels = seq(0, 1.5*signif(maxY2, 1), length.out = 9)
        labels = labels[labels < maxY2]    
        if (length(labels)<5)  labels = seq(0, 2*signif(maxY2, 1), length.out = 20)
        labels = signif(labels, 1)
        
        axis(4, at = labels * maxY /maxY2, labels = labels)
    } else {
        polygonCoords(mnths, uncert_r, cols[2], lwd = 2)  
    }
    polygonCoords(mnths, obs_r, cols[3], lwd = 1, lty = 3) 
    lineSeg <- function(i) {
        x = c(mnths[i-1], mnths[i]) + 0.5
        y = rep(par("usr")[4]*0.9, 2)
        if (pvalue[i] == 0 ) return()
        lwd =  10*(1-pvalue[i])
        lines(x, y, lwd = lwd) 
    }  
    pvalue0 = pvalue 
    pvalue = 1- pvalue
    pvalue[liki>0.1] = 1
    #lapply(2:length(mnths), lineSeg)
    #browser()
    addGrid(col = make.transparent("black", 0.9)   )
    if (grepl("All Deforested", name)) {
        for (mn in 1:ncol(fire_seasons)) {
            y = obs_r[tail(fire_seasons[,mn], 1),1]
            lines(c(-9E9, 9E9), c(y, y), col = "black", lty = 2, lwd = 0.5)
        }
    }
    #grid(col = make.transparent("black", 0.9), nx = -1 + length(mnths)/12, ny = NULL)
    mtext(name, adj = 0.1, line = -1.5, col = make.transparent("black", 0.5))   

    if (legend) {#if (name == tail(names(regions),1)) {
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
    
   # uncert_r0 = climScale(uncert_r)
    #error_r = climScale(error_r)
    uncert_r = openRegionF("uncert_anomolie.")[,1:2]  
    error_r  = openRegionF("error_anomolie.")[,1:2]  
    #browser()
    par(mar = c(1.5, 1, 0, 0.5))  
    dumbell <- function(fire_seasonsi) {
        fire_seasonsi = as.matrix(fire_seasonsi)  
        findFSvalues <- function(r)
            t(apply(fire_seasonsi, 1, function(fs) apply(r, 2, function(i) mean(i[fs]))))
        
        obs_r    = findFSvalues(obs_r   )  
        uncert_r = findFSvalues(uncert_r)
        error_r  = findFSvalues(error_r )     
        #moist_r  = findFSvalues(moist_r)
        if (moistPlot) {
            mock = 1:nrow(moist_r)
            cols_dumb = apply(moist_r, 2,function(i) {mock[sort.int(i,  index.return=TRUE)[[2]]] = mock; mock})
            cols_dumb = make_col_vector(cols_moist, ncols = nrow(moist_r))[cols_dumb]
            
        } else cols_dumb = make_col_vector(cols_years, ncols = 19)
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
       
        addDumbells(obs_r, error_r, uncert_r, cols_dumb)         
        lines(x = c(0.0001, 9E9), y = c(0.0001,9E9),
              col = make.transparent("white", 0.67), lty = 2)
        lines(x = c(0.0001, 9E9), y = c(1, 1), lty = 3,
              col = make.transparent("white", 0.67 ) ) 
        return(cols_dumb)
 
    }
    addDumbells <- function(x, ye, yu, cols_dumb) {
        points(x[,1], yu[,1], pch = 19, col = cols_dumb, cex = 0.5)
        points(x[,2], yu[,2], pch = 19, col = cols_dumb, cex = 0.5) 
        cols_dumb = make.transparent(cols_dumb, 0.95)
        for (i in 1:20) {
        apply(cbind(x[,], yu[,], cols_dumb), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), lwd = 2.67, col = i[5]))   

        apply(cbind(x[,], ye[,], cols_dumb), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), col = i[5], lwd = 0.6))  
        apply(cbind(x[,], ye[,], cols_dumb), 1,
              function(i) lines(c(i[1], i[2]), c(i[3], i[4]), lwd = 0.2, lty = 0.5, col = "white")) 
        }
    } 
    cols_dumb = apply(fire_seasons, 2, dumbell)[,1]
    
    if (legend) {# (name == tail(names(regions), 1)) {
        plot.new()
legend('topright', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                text.col = "black", horiz = TRUE, bty = 'n')
        legend('topright', c('Full postirior', 'Parameter uncertainty', 'Observations'),
                text.col = make.transparent(cols, 0.4), horiz = TRUE, bty = 'n')  

        par(mar = c(0, 0, 2, 0))
        plot(c(1, 20), c(0, 1), col = "black", cex = 1000, axes = FALSE, pch = 19)
        
        addDumbells(cbind(2:19, 2:19), cbind(rep(0.1, 18), rep(1.0, 18)),
                    cbind(rep(0.3+0.1*0.3, 18), rep(0.7+0.1*0.7, 18)), cols_dumb[-1])
        text(x = c(2, 10, 19) , y = 0.1, c('2002', '2010', '2019'), xpd = NA, col = "white")
        if (!moistPlot) {
            text(10, 0.5, 'Uncertainty', xpd = NA, col = "white")
            text(10, 0.9, 'Error', xpd = NA, col = "white") 
        }
    }
        
    #for (i in c(4, 7, 10, 19)) {
    #    text(obs_r[i,1], mean(uncert_r[i,], 1), 2000 + i, srt = -90, adj = c(0.5, -0.5))
    #}
    #return(list(obs_r0, uncert_r0, moist_r0))
}

plotRegionsSub <- function(id) {
    np = length(id)
    heights =  c(rep(1, np), 0.7)
    height = sum(heights) * 27 + 5
    layout = t(matrix(c(1:((3*np)+2), 3*np + 2), nrow = 3))
    png(paste0(c(figName, id, '.png'), collapse = '-') ,
        height = height, width = 183, res = 300, units = 'mm')
        layout(layout, widths = c(.7, 0.15, 0.15),
                heights = heights)
        par(oma = c(0.67, 1, 2, 2.5))

        outs = mapply(plotRegion, regions[id], names(regions)[id], c(rep(F, np-1), T),SIMPLIFY = FALSE)

        mtext.units(outer = TRUE, side = 2, 'Burnt area (%)', line = -1)
        mtext(outer = TRUE, side = 4, 'Modelled anomaly', line = 1.2)
        mtext(side = 1, 'Observed anomaly', line = -5.5)
    dev.off()
}

plotRegionsSub(1:5)
plotRegionsSub(6:9)
