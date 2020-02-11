source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs('../rasterextrafuns/rasterPlotFunctions/R/')
source("libs/return_multiple_from_functions.r")

library(plotrix)
library(mapdata)
library(mapplots)

aus_ste <- readOGR(dsn = "data/state_shape/STE11aAust.shp")

StandardLegend <- function(cols, limits, dat, rightx = 0.95, extend_max = TRUE, oneSideLabels = TRUE, transpose = FALSE,
                           plot_loc = NULL, srt = 0, add = FALSE, ...) {
                           
    if(is.null(plot_loc)){
        if(add) plot_loc = c(0.02, 0.6, 0.1, 0.13)
        else plot_loc = c(0.01, rightx, 0.3, 0.56)
    }
                           
    add_raster_legend2(cols, limits, dat = dat, add = add,
                       transpose = transpose, srt = srt, oneSideLabels= oneSideLabels,
                       plot_loc = plot_loc,
                       ylabposScling = 1, extend_max = extend_max, ...)
}

lineBox <- function(x, y, ...) 
    lines(c(x[1], x[2], x[2], x[1], x[1]), c(y[1], y[1], y[2], y[2], y[1]),...)

plotStandardMap <- function(r, cols, limits, e = NULL, add_legend = FALSE,
                            limits_error = c(0.5, 0.500000001),
                            title2 = '', title3 = '', left_text_adj = NA, 
                            left_text_adj_line = -1.5, left_text_srt = 0, 
                            regions = NULL, ...) {
    
    if (nlayers(r) > 1 && is.null(e)) {
        if (nlayers(r) == 3) {
            e = 1-r[[1]]/r[[3]]
            r = r[[2]]
            
        } else {
            e = sd.raster(r)
            r = mean(r)
        }
    } 
    r[r>9E9] = NaN
    if (!is.null(e)) e[is.na(r)] = NaN
    
    #plot(c(129, 155), c(-44, -21.25), xlab = '', ylab = '', axes = FALSE, type ='n')
    
    plot(c(112, 155), c(-44, -10.5), xlab = '', ylab = '', axes = FALSE, type ='n', yaxs = 'i')
    grid()
    plot_raster_from_raster(r, e = e,coast.lwd = NULL,
                            cols = cols, limits = limits, add_legend = FALSE,interior = TRUE,
                            quick = TRUE, ePatternRes = 5, ePatternThick = 0.67,
                            limits_error = limits_error, add = TRUE, ...)

    lines(aus_ste)
    
    if (!is.null(regions))  lapply(regions, function(i) lineBox(i[1:2], i[3:4], lty = 2))
    
    polygon(c(-62.5, -35, -35, -62.5), c(-56, -56, -50, -50), border = NA, col = "white")
    mtext(title3, adj = 0.1, line = 0.0, srt = left_text_srt)
    mtext(title2, side = 1, line = left_text_adj_line, adj = left_text_adj, srt = left_text_srt)
    if (add_legend) {
        add_raster_legend2(cols, limits, dat = trend[[2]],
                           transpose = FALSE, srt = 0, oneSideLabels= FALSE,
                           plot_loc = c(0.35, 0.99, 0.09, 0.12),  ylabposScling=0.8, ...)
    }
}
