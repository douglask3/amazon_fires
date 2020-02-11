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

modal_cols = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

obs = lapply(files, brick)
mxLayers = min(sapply(obs, nlayers))
obs = lapply(obs, function(i) i[[1:mxLayers]])

convert2Climatology <- function(r) 
    layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))

obs = lapply(obs, convert2Climatology)
obs = lapply(obs, raster::crop, global_extent)
obs[[1]] = obs[[1]] * 100

ModalMap <- function(obs, txt, addLegend) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i) modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

    plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
    mtext(txt, side = 2)
    if (addLegend) 
        StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
                       extent_max = TRUE,
                       labelss = modal_limits, add = TRUE) 
}
png("figs/fire_var_seasonality.png", height = 8, width = 7, res = 300, units = 'in')
    par(mar = rep(0, 4)) 
    layout(rbind(cbind(c(1, 1, 2, 2), c(3, 3, 6, 6), c(3, 4, 4, 6), c(3, 3, 6, 6), c(5, 5, 7, 7)),
                 rbind(c(8, 9, 9, 9, 10), c(11, 12, 12, 12, 13))),
           heights = c(0.6, 0.4, 0.6, 0.4, 1.3, 1.3), width = c(1, 0.3, 0.3, 0.4, 1))
    mapply(ModalMap, obs, names(files), c(T, F)) 

    pc = lapply(obs,PolarConcentrationAndPhase.RasterBrick, phase_units = "months")
    phase_cols = c('#a50026','#ffff00','#313695', '#a50026')
    conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

    plotConPhase <- function(pc, addLegend = FALSE) {
        plotStandardMap(pc[[1]], limits = 0.5:11.5, cols = phase_cols)
        if (addLegend) SeasonLegend(0.5:11.5, cols = phase_cols, add = FALSE)
        plotStandardMap(pc[[2]], limits = seq(0, 1, 0.1), cols = conc_cols)
        if (addLegend) StandardLegend(limits = seq(0, 1, 0.1), cols = conc_cols,
                                      dat = pc[[2]], add = TRUE) 
    }

    mapply(plotConPhase, pc, c(TRUE, FALSE))
                   
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
    par(mar = rep(1, 4))
    obsv = mapply(plotRegion, regions, names(regions), axisMonth, SIMPLIFY = FALSE)
    plot(c(0, 1), c(0,1), type = 'n', axes = FALSE, xlab = '', ylab = '')
    text.units(0.2, 0.8, "burnt area (%)", col = "blue", adj = 0, cex= 1.3)
    text.units(0.2, 0.5, "fire count (k~m2~)", col = "red", adj = 0, cex = 1.3)
dev.off()