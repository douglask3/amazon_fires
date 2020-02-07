source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
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
              

regions = list(NorthWestAus = c(110, 140, -27.5, -10),
               SouthWestAus = c(110, 125, -37.5, -27.5),
               NorthEastAus = c(142, 154, -26, -10),
               SouthEastAus = c(142, 154, -39, -31))
              
axisMonth = c(2, 6, 4, 6)

modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

obs = brick("outputs/Australia_region/firecount-SE_Aus_2001_onwards.nc")

convert2Climatology <- function(r) 
    layer.apply(1:12, function(mn) mean(obs[[seq(mn, nlayers(r), by = 12)]]))

obs = convert2Climatology(obs)
#obs = obs[[c(7:12, 1:6)]]
obs = raster::crop(obs, global_extent)

modal = testModal(obs)
modal_approx = layer.apply(2:6, function(i) modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

par(mfrow = c(3, 2), mar = rep(0.5, 4))

plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
               extent_max = TRUE,
               labelss = modal_limits, add = TRUE, plot_loc = c(0.1, 0.5, 0.05, 0.08)) 
                

plotRegion <- function(region, name, axisMonth) {
    obs = raster::crop(obs, extent(region))

    obsv = layer.apply(obs, function(i) quantile(i, c(0.25, 0.5, 0.75)))  
    obsv = matrix(unlist(obsv), nrow = 3)
    xlim = c(-max(obsv), max(obsv))

    polarPlot.setup(1:12, obsv[2,], type = 'l', xlim = xlim, col = '#009900', lwd = 2)
    polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth)

    polarPlot.polygon(1:12, obsv[c(1, 3),], col = '#009900', alpha = 0.67, border = TRUE)
    mtext(side = 3, line = -1, name, adj = 0.1)
    return(obsv)
}

obsv = mapply(plotRegion, regions, names(regions), axisMonth, SIMPLIFY = FALSE)
