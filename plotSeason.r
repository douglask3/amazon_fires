source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
source("libs/SeasonLegend.r")
library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

global_extent = extent(c(112, 155, -44, -10))
#regions = list(tS = c(-180, 180, -60, -30), 
#               TS = c(-180, 180, -30, 0),
#               TN = c(-180, 180, 0, 30),
#               tN = c(-180, 180, 30, 60),
#               BN = c(-180, 180, 60, 90))

files = c("Burnt Area" = "outputs/Australia_region/burnt_area-GFED4s_2.5degree_2001-2016.nc",
          "Fire Count" = "outputs/Australia_region/firecount-SE_Aus_2001_onwards.nc")

regions = list(NorthWestAus = c(110, 140, -27.5, -10),
               SouthWestAus = c(110, 125, -37.5, -27.5),
               NorthEastAus = c(142, 154, -26, -10),
               SouthEastAus = c(142, 154, -39, -31))
               
regions = list('Gold Coast,\nNorthern Rivers' = c(151.25, 153.75, -31.25, -26.75),
               'Wollemi,\nBlue Mountains NPs' = c(148.75, 151.25, -36.25, -31.25),
               'East\nGippsland' = c(146.25, 148.75, -38.75, -36.25),
               'Kangaroo\nIsland, Adelaide' = c(136.25, 138.75, -36.25, -33.75))

              
axisMonth = c(2, 6, 4, 8)
reds9   = c("#FFFBF7", "#F7EBDE", "#EFDBC6", "#E1CA9E", "#D6AE6B", 
            "#C69242", "#B57121", "#9C5108", "#6B3008")
                
aa_cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
phase_cols = c('#ffff00','#313695', '#a50026', '#ffff00')
conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

limits_rank = seq(0, 0.9, 0.1)
cols_rank = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')

obs = lapply(files, brick)
mxLayers = min(sapply(obs, nlayers))
obs = obs_ia =lapply(obs, function(i) i[[1:mxLayers]])

convert2Climatology <- function(r) 
    layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))

obs = lapply(obs, convert2Climatology)
obs = lapply(obs, raster::crop, global_extent)
obs[[1]] = obs[[1]] * 100*4

ModalMap <- function(obs, txt, addLegend, let) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i) modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

    plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
    addLetLab(let)
    mtext(txt, side = 2)
    if (addLegend) 
        StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
                       extent_max = TRUE,
                       labelss = modal_limits, add = TRUE) 
}

YealySeason <- function(season, r, FUN = 'sum') {
    nyears = ceiling(nlayers(r)/12)
    seasons = lapply(1:nyears, function(i) season + (i-1) * 12)    
    test = sapply(seasons, function(i) all(i < nlayers(r)))
    seasons = seasons[test]
    if (is.null(FUN)) return(seasons)
    if (FUN == "sum") out = layer.apply(seasons, function(i) sum(r[[i]]))
    else out = layer.apply(seasons, function(i) FUN(r[[i]]))
    return(out)
}

srank.raster <- function(r1, r2, lab = '', name = '',season = NULL) {
    if(!is.null(season)) {
        r1 = YealySeason(season, r1)
        r2 = YealySeason(season, r2)
    }
    mask = !any(is.na(r1+r2))
    srank.cell <- function(v1, v2) 
         cor.test(v1, v2, method = "spearman")[[4]]
    
    out = r1[[1]]
    out[mask] = mapply(srank.cell, as.data.frame(t(r1[mask])), as.data.frame(t(r2[mask])))
    out[!mask] = NaN
    
    plotStandardMap(srank.month, limits = limits_rank, cols = cols_rank)
    mtext(name, side = 2, adj = 0.9, line = -0.2)
    addLetLab(lab)
    StandardLegend(aa, limits = limits_rank, cols = cols_rank,add = TRUE, oneSideLabels = FALSE)
    return(out)
}

png("figs/fire_var_seasonality.png", height = 300, width = 183, res = 300, units = 'mm')
    par(oma = c(1, 2, 1, 0))
    layout(rbind(cbind(c(1, 4, 4, 5, 5), c(2, 6, 6, 9, 9), c(2, 6, 7, 7, 9), c(2, 6, 6, 9, 9), c(3, 8, 8, 10, 10)),
                 rbind(c(11, 12, 12, 12, 0), c(13, 14, 14, 14, 15), c(16, 17, 17, 17, 18))),
           heights = c(1, 0.68, 0.32, 0.50, 0.5, 1, 1.3, 1.3), width = c(1, 0.15, 0.3, 0.55, 1))
    
    mask = !any(is.na(obs[[1]]+obs[[2]]))
    x0 = as.vector(obs[[1]][mask]); y0 = as.vector(obs[[2]][mask])
    x = log(x0+0.0001); y = log(y0+0.0001)
    
    cols = densCols(x,y, colramp = colorRampPalette(reds9), bandwidth = 1)
    par(mar = c(3, 2, 0.5, 1.5)) 
    plot(y~x, pch = 19, col = cols, cex = 1, axes = FALSE, xlab = '', ylab = '')
    addLetLab('a')
    mtext.units(side = 1, line = 2, 'Burnt area (%)')
    mtext.units(side = 2, line = 2, 'Fire count (k~m-2~)')
    addAxis <- function(labels, side) {
        at = log(labels + 0.0001)
        axis(at = at, labels = labels, side = side)
        if (side == 1 || side == 3) FUN = function(i, ...)lines(c(i, i), c(-9E9, 9E9), ...)
            else  FUN = function(i, ...)lines(c(-9E9, 9E9), c(i, i), ...)
        lapply(at, FUN, lty = 2, col = make.transparent("black", 0.67))
    }
    addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 1)
    addAxis(c(0, 0.001, 0.01, 0.1, 1, 10, 100), 2)
    abline(lm(y~x))
    mtext.units(side = 3, paste0("~R2~: ",  round(cor(x0, y0)^2, 2)), line = -2.8, adj = 0.1)
    mtext.units(side = 3, "p < 0.001", line = -4, adj = 0.1)
    
    par(mar = rep(0, 4)) 
    plotAA <- function(r, limits, lab, name, units) {
        aa = mean(r) * 12
        plotStandardMap(aa, limits = limits, cols = aa_cols)
        mtext(name, side = 2, adj = 0.9, line = -0.2)
        addLetLab(lab)
        StandardLegend(aa, limits = limits, cols = aa_cols, units = units, add = TRUE, oneSideLabels = FALSE)
    }
    mapply(plotAA, obs, list(c(0, 1, 2, 5, 10, 20, 50), c(0, 0.1, 0.2, 0.5, 1, 2, 5)),
           c('b', 'c'), c('Burnt area', 'Fire count'), c('%', 'k~m-2~'))
    
    mapply(ModalMap, obs, names(files), c(T, F), c('d', 'g')) 

    pc = lapply(obs,PolarConcentrationAndPhase.RasterBrick, phase_units = "months")

    plotConPhase <- function(pc, let, addLegend = FALSE) {
        plotStandardMap(pc[[1]], limits = 0.5:11.5, cols = phase_cols)
        addLetLab(let[1])
        if (addLegend) SeasonLegend(0.5:11.5, cols = phase_cols, add = FALSE)
        plotStandardMap(pc[[2]], limits = seq(0, 1, 0.1), cols = conc_cols)
        addLetLab(let[2])
        if (addLegend) StandardLegend(limits = seq(0, 0.9, 0.1), cols = conc_cols, extent_max = FALSE,
                                      max_lab = 1, dat = pc[[2]], add = TRUE, oneSideLabels = FALSE) 
    }

    mapply(plotConPhase, pc, list(c('e', 'f'), c('h', 'i')), c(TRUE, FALSE))
    
    srank.month = srank.raster(obs_ia[[1]], obs_ia[[2]], 'j', 'monthly rank')
    srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'k', 'annual rank', season = 8:13)

                   
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
    obsv = mapply(plotRegion, regions, paste(c('l', 'm', 'n', 'o'), names(regions), sep = ') '),
                  axisMonth, SIMPLIFY = FALSE)
    par(mar = c(2, 3, 2, 0))
    plot(c(0, 1), c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    mtext.units("Burnt area (%)", col = "blue", adj = 0, side = 3, line = -2)
    mtext.units("Fire count (k~m-2~)", col = "red", adj = 0, side = 3, line = -3.8)

dev.off()