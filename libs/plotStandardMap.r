
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')

library(plotrix)
library(mapdata)
library(mapplots)

plotStandardMap <- function(r, cols, limits, e = NULL, add_legend = FALSE, ...) {
    if (nlayers(r) > 1 && is.null(e)) {
        e = sd.raster(r)
        r = mean(r)
    }
    
    plot_raster_from_raster(r, e = e,
                            cols = cols, limits = limits, add_legend = FALSE,
                            quick = TRUE, ePatternRes = 5, ePatternThick = 0.67,
                            limits_error = c(0.05, 0.1), ...)
    if (add_legend) {
        add_raster_legend2(cols, limits, dat = trend[[2]],
                           transpose = FALSE, srt = 0, oneSideLabels= FALSE,
                           plot_loc = c(0.35, 0.99, 0.09, 0.12),  ylabposScling=0.8, ...)
    }
}