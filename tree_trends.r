library(raster)
library(greenbrown)

file = "outputs/amazon_region/vegetation/treecover-2001-June2018.nc"

cols_trend = c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#f7f7f7',
               '#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419')
limits_trend = c(-15, -10, -5, -2, -1, 1, 2, 5, 10, 15)

cols_tree = c('#ffffe5','#f7fcb9','#d9f0a3','#addd8e','#78c679','#41ab5d','#238443','#006837','#004529')
limits_tree = c(0   , 1, 10, 20, 40, 60, 80)

dat = brick(file) / 0.8

logistic <- function(r) 1/(1+exp(-r))

trend = TrendRaster(logit(dat), freq = 1)
trend[[2]] = (logistic(trend[[2]] * trend[[1]])-0.5) * 100/0.8
trend[[2]] = trend[[2]]# * mean(dat)
dat = dat*100
plotStandardMap <- function(r, cols, limits, e, add_legend = FALSE, ...) {
    plot_raster_from_raster(r, e = e,
                            cols = cols, limits = limits, add_legend = FALSE,
                            quick = TRUE, ePatternRes = 5, ePatternThick = 1, limits_error = c(0.05, 0.1))
    if (add_legend) {
        add_raster_legend2(cols, limits, dat = trend[[2]],
                           transpose = FALSE, srt = 0, oneSideLabels= FALSE,
                           plot_loc = c(0.35, 0.99, 0.09, 0.12),  ylabposScling=0.8, ...)
    }
}

par(mfcol = c(1,2   ))
plotStandardMap(mean(dat), cols_tree, limits_tree, NULL, TRUE, maxLab = 100)
plotStandardMap(trend[[2]], cols_trend, limits_trend, trend[[3]], TRUE, extend_max = TRUE, extend_min = TRUE)  