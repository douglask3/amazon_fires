source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
source("libs/SeasonLegend.r")
source("libs/YearlySeason.r")
source("libs/addLetLab.r")
library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

global_extent = extent(c(-85, -30, -60, 13))

files = c("Burnt area" = "outputs/amazon_region/burnt_area-GFED4s_2.5degree_2001-2016.nc",
          "Fire count" = "outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc")
               
regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75))
              
axisMonth = c(0, 0, 0, 0)
reds9   = c("#FFFBF7", "#F7EBDE", "#EFDBC6", "#E1CA9E", "#D6AE6B", 
            "#C69242", "#B57121", "#9C5108", "#6B3008")
                
aa_cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
phase_cols = c('#313695', '#a50026', '#ffff00', '#313695')
phase_cols = c('#111695', '#22ff22', '#ffff00', '#bb6600', '#a50026', '#111695')
conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

modal_cols = c('#fff7f3' ,'#fcc5c0', '#f768a1','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

limits_rank = seq(0, 0.9, 0.1)
cols_rank = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

obs = lapply(files, brick)
obs[[1]] = obs[[1]] * 100
obs[[2]] = 100* obs[[2]]/raster::area(obs[[2]][[1]])

mxLayers = min(sapply(obs, nlayers))
obs = obs_ia =lapply(obs, function(i) i[[1:mxLayers]])

convert2Climatology <- function(r) 
    layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))

obs = lapply(obs, convert2Climatology)
obs = lapply(obs, raster::crop, global_extent)

ModalMap <- function(obs, txt, addLegend, let, additional) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i)
                               modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

    plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
    addLetLab(let, additional)
    mtext(txt, side = 2, line = -2)
    if (addLegend) 
        StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
                       extend_max = TRUE,
                       labelss = modal_limits, add = TRUE) 
}

srank.raster <- function(r1, r2, lab = '', name = '',season = NULL) {
    if(!is.null(season)) {
        r1 = YearlySeason(season, r1)
        r2 = YearlySeason(season, r2)
    }
    mask = !any(is.na(r1+r2))
    srank.cell <- function(v1, v2) 
         cor.test(v1, v2, method = "spearman")[[4]]
    
    out = r1[[1]]
    out[mask] = mapply(srank.cell, as.data.frame(t(r1[mask])), as.data.frame(t(r2[mask])))
    out[!mask] = NaN
    
    plotStandardMap(out, limits = limits_rank, cols = cols_rank)
    #mtext(name, side = 2, line = -2)
    addLetLab(lab, name)
    StandardLegend(out, limits = limits_rank, cols = cols_rank,
                   extend_max = FALSE, maxLab = 1, add = TRUE, oneSideLabels = FALSE)
    return(out)
}

png("figs/fire_var_seasonality.png", height = 400, width = 160, res = 300, units = 'mm')
    par(oma = c(1, 2, 1, 1))
    layout(rbind(cbind(c(1, 4, 4, 5, 5), c(2, 6, 6, 9, 9), c(2, 6, 7, 7, 9), c(2, 6, 6, 9, 9), c(3, 8, 8, 10, 10)),
                 rbind(c(11, 12, 12, 12, 0), c(13, 14, 14, 14, 15), c(16, 17, 17, 17, 18))),
           heights = c(1, 0.5, 0.5, 0.50, 0.5, 1, 0.75, 0.75), width = c(1, 0.55, 0.45, 0.05, 1))
    
    mask = !any(is.na(obs[[1]]+obs[[2]]))
    x0 = as.vector(obs[[1]][mask]); y0 = as.vector(obs[[2]][mask])
    x = log(x0+0.0001); y = log(y0+0.0001)
    
    cols = densCols(x,y, colramp = colorRampPalette(reds9), bandwidth = 1)
    par(mar = c(7, 2, 1, 1.5)) 
    plot(y~x, pch = 19, col = cols, cex = 1, axes = FALSE, xlab = '', ylab = '')
    addLetLab('a', line = 0)
    mtext.units(side = 1, line = 2, 'Burnt area (%)')
    mtext.units(side = 2, line = 2, 'Fire count (10k~m-2~)')
    addAxis <- function(labels, side) {
        at = log(labels + 0.000001)
        axis(at = at, labels = labels, side = side)
        if (side == 1 || side == 3) FUN = function(i, ...)lines(c(i, i), c(-9E9, 9E9), ...)
            else  FUN = function(i, ...)lines(c(-9E9, 9E9), c(i, i), ...)
        lapply(at, FUN, lty = 2, col = make.transparent("black", 0.67))
    }
    addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 1)
    addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 2)
    abline(lm(y~x))
    mtext.units(side = 3, paste0("~R2~: ",  round(cor(x, y)^2, 2)), line = -2.8, adj = 0.1)
    mtext.units(side = 3, "p < 0.001", line = -4, adj = 0.1)
    
    par(mar = rep(0, 4)) 
    plotAA <- function(r, limits, lab, additional, name, units) {
        aa = mean(r) * 12
        plotStandardMap(aa, limits = limits, cols = aa_cols)
        mtext(name, side = 2, line = -2)
        addLetLab(lab, additional)
        StandardLegend(aa, limits = limits, cols = aa_cols, units = units, add = TRUE,
                        oneSideLabels = FALSE)
    }
    mapply(plotAA, obs, list(c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10),
                             c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10)/2),
           c('b', 'c'), c('Annual average', ''), c('Burnt area', 'Fire count'), c('%', '    10k~m-2~'))
    
    mapply(ModalMap, obs, names(files), c(T, F), c('d', 'g'), c('Modality', '')) 

    pc = lapply(obs,PolarConcentrationAndPhase.RasterBrick, phase_units = "months")

    plotConPhase <- function(pc, let, addLegend = FALSE) {
        plotStandardMap(pc[[1]], limits = 0.5:11.5, cols = phase_cols)
        if (addLegend) additional = c('Phase', 'Concentration') else additional = c('', '')
        addLetLab(let[1], additional[1])
        if (addLegend) SeasonLegend(0.5:11.5, cols = phase_cols, add = FALSE)
        plotStandardMap(pc[[2]], limits = seq(0, 1, 0.1), cols = conc_cols)
        addLetLab(let[2], additional[2])
        if (addLegend) StandardLegend(limits = seq(0, 0.9, 0.1), cols = conc_cols, extend_max = FALSE,
                                      maxLab = 1, dat = pc[[2]], add = TRUE, oneSideLabels = FALSE) 
    }

    mapply(plotConPhase, pc, list(c('e', 'f'), c('h', 'i')), c(TRUE, FALSE))
    
    srank.month  = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'Monthly rank')
    srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'k', 'Annual rank', season = 8)

                   
    plotRegion <- function(region, name, axisMonth) {
        obs = lapply(obs, raster::crop, extent(region))
        
        getQuants <- function(obs) {
            obsv = layer.apply(obs, function(i) quantile(i, c(0.25, 0.5, 0.75)))  
            obsv = matrix(unlist(obsv), nrow = 3)
            return(obsv)
        }
        obsv = lapply(obs, getQuants)
        maxObsV = sapply(obsv, max)
        obsv = mapply('/', obsv, maxObsV, SIMPLIFY = FALSE)
        
        xlim =  c(-1,1)
        
        polarPlot.setup(1:12, obsv[[1]][2,], type = 'l', xlim = xlim, col = 'blue', lwd = 2)
        polarPlot.lines(1:12, obsv[[2]][2,], col = "red", lwd = 2)
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth, labScale = maxObsV[1], nguides = 4, col = "blue")
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth+1, labScale = maxObsV[2], nguides = 4, col = "red")

        polarPlot.polygon(1:12, obsv[[1]][c(1, 3),], col = 'blue', alpha = 0.67, border = TRUE)
        polarPlot.polygon(1:12, obsv[[2]][c(1, 3),], col = 'red', alpha = 0.67, border = TRUE)
        mtext(side = 3, line = 0, name, adj = 0.1)
        return(obsv)
    }
    par(mar = c(0, 0, 3, 1.5))
    obsv = mapply(plotRegion, regions, paste(letters[1:length(regions)], rep('', length(regions)), sep = ') '),
                  axisMonth, SIMPLIFY = FALSE)
    par(mar = c(2, 3, 2, 0))
    plot(c(0, 1), c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    mtext.units("Burnt area (%)", col = "blue", adj = 0, side = 3, line = -2)
    mtext.units("Fire count (10k~m-2~)", col = "red", adj = 0, side = 3, line = -3.8)

dev.off()
