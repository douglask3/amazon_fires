source("libs/PolarConcentrationAndPhase.r")
source("libs/polarPlot.r")
source("libs/plotStandardMap.r")
source("libs/SeasonLegend.r")
source("libs/YearlySeason.r")
source("libs/addLetLab.r")
source("libs/srank.raster.r")
library(raster)
library(rasterExtras)
source("../gitProjectExtras/gitBasedProjects/R/sourceAllLibs.r")
sourceAllLibs("../rasterextrafuns/rasterPlotFunctions/R/")
graphics.off()

global_extent = extent(c(-85, -30, -60, 13))

files = c("Burnt area" = "outputs/amazon_region/burnt_area-GFED4s_2.5degree_2001-2016.nc",
          "Fire count" = "outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc")
mod_file = NULL# 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeppSoil/model_summary.nc'

units = c('%', '10k~m-2~')    
scale = c(100, NaN)

regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75),   
               E = -c( 61.25, 53.75, 23.75, 18.75))
              
axisMonth = c(0, 0, 0, 0)
reds9   = c("#FFFBF7", "#F7EBDE", "#EFDBC6", "#E1CA9E", "#D6AE6B", 
            "#C69242", "#B57121", "#9C5108", "#6B3008")
                
aa_limits = list(c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10),
                 c(0, 0.1, 0.2, 0.5, 1, 2, 5, 10)/2)
aa_cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
phase_cols = c('#313695', '#a50026', '#ffff00', '#313695')
phase_cols = c('#111695', '#22ff22', '#ffff00', '#bb6600', '#a50026', '#111695')
conc_cols = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')

modal_cols = c('#fff7f3' ,'#fcc5c0', '#f768a1','#ae017e','#7a0177','#49006a')#c('#34eeba','#d95f02','#7570b3')
modal_limits = c(1, 1.1, 1.2, 1.5, 2)

limits_rank = c(-0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8)
cols_rank = c('#8e0152','#c51b7d','#de77ae','#f1b6da','#fde0ef','#e6f5d0','#b8e186','#7fbc41','#4d9221','#276419')

runAll <- function(name, mxLayers = NULL, Layers = NULL) {
obs = lapply(files, brick)


if (!is.null(mod_file))
    obs[[2]] = brick(mod_file)[[1:nlayers(obs[[1]])]]
    #obs[[2]] = layer.apply(1:nlayers(obs[[1]]), function(i)brick(mod_file, level = i))#[[50]])


scaleFun <- function(r, scale) {
    if (is.na(scale)) r = 100 * r/raster::area(obs[[2]][[1]])
    else r = r * scale
    return(r)
}
obs = mapply(scaleFun, obs, scale)
#obs[[1]] = obs[[1]] * 100
#obs[[2]] = 100* obs[[2]]/raster::area(obs[[2]][[1]])

if (is.null(mxLayers)) mxLayers = min(sapply(obs, nlayers))
if (is.null(Layers)) Layers = 1:mxLayers
obs = obs_ia =lapply(obs, function(i) i[[Layers]])

convert2Climatology <- function(r) 
    layer.apply(1:12, function(mn) mean(r[[seq(mn, nlayers(r), by = 12)]]))

obs = lapply(obs, convert2Climatology)
obs = lapply(obs, raster::crop, global_extent)
obs[[2]][is.na(obs[[1]])] = NaN

modal_approx <- function(dat) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i)
                               modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]
    return(modal_approx)
}

ModalMap <- function(obs, txt, addLegend, let, additional) {
    modal = testModal(obs)
    modal_approx = layer.apply(2:6, function(i)
                               modal[[i]] * (0.5-cos(2*pi * modal[[i+6]]/12)/2))
    modal_approx = 1 + sum(modal_approx, na.rm = TRUE)/modal[[1]]

    plotStandardMap(modal_approx -1, limits = modal_limits -1, cols = modal_cols)
    addLetLab(let, additional)
    mtext(txt, side = 2, line = -1)
    if (addLegend) 
        StandardLegend(limits = modal_limits - 1, cols = modal_cols, dat = modal_approx-1,
                       extend_max = TRUE,
                       labelss = modal_limits, add = TRUE) 
}

fname = paste0("figs/fire_var_seasonality-", name, ".png")
png(fname, height = 400, width = 160, res = 300, units = 'mm')
    par(oma = c(1, 2, 1, 1))
    layout(rbind(cbind(c(1, 4, 4, 5, 5), c(2, 6, 6, 9, 9), c(2, 6, 7, 7, 9), c(2, 6, 6, 9, 9), c(3, 8, 8, 10, 10)),
                 rbind(c(11, 12, 12, 12, 0), c(13, 14, 14, 14, 15), c(16, 17, 17, 17, 18))),
           heights = c(1, 0.5, 0.5, 0.50, 0.5, 1, 0.75, 0.75), width = c(1, 0.5, 0.45, 0.05, 1))
    
    mask = !any(is.na(obs[[1]]+obs[[2]]))
    x0 = as.vector(obs[[1]][mask]); y0 = as.vector(obs[[2]][mask])
    x = log(x0+0.000001); y = log(y0+0.000001)
    
    cols = densCols(x,y, colramp = colorRampPalette(reds9), bandwidth = 1)
    par(mar = c(7, 2, 1, 1.5)) 
    plot(y~x, pch = 19, col = cols, cex = 1, axes = FALSE, xlab = '', ylab = '')
    addLetLab('a', line = 0)
    mtext.units(side = 1, line = 2, paste0(names(files)[1], ' (', units[1], ')'))
    mtext.units(side = 2, line = 1, paste0(names(files)[2], ' (', units[2], ')'))
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
    
    par(mar = c(0, 0, 1, 0)) 
    plotAA <- function(r, limits, lab, additional, name, units) {
        aa = mean(r) * 12
        plotStandardMap(aa, limits = limits, cols = aa_cols)
        mtext(name, side = 2, line = -1)
        addLetLab(lab, additional)
        StandardLegend(aa, limits = limits, cols = aa_cols, units = units, add = TRUE,
                        oneSideLabels = FALSE)
    }
    units_aa = units
    units_aa[units == '10k~m-2~'] = '    10k~m-2~'
    mapply(plotAA, obs, aa_limits,
           c('b', 'c'), c('Annual average', ''), names(files), units_aa)
    
    mapply(ModalMap, obs, names(files), c(T, F), c('a', 'd'), c('Modality', '')) 

    pc = lapply(obs,PolarConcentrationAndPhase.RasterBrick, phase_units = "months")

    plotConPhase <- function(pc, let, addLegend = FALSE) {
        plotStandardMap(pc[[1]], limits = 1:12, cols = phase_cols)
        if (addLegend) additional = c('Phase', 'Concentration') else additional = c('', '')
        addLetLab(let[1], additional[1])
        if (addLegend) SeasonLegend(0.5:11.5, cols = phase_cols, add = FALSE)
        plotStandardMap(pc[[2]], limits = seq(0, 1, 0.1), cols = conc_cols)
        addLetLab(let[2], additional[2])
        if (addLegend) StandardLegend(limits = seq(0, 0.9, 0.1), cols = conc_cols, extend_max = FALSE,
                                      maxLab = 1, dat = pc[[2]], add = TRUE, oneSideLabels = FALSE) 
    }

    mapply(plotConPhase, pc, list(c('b', 'c'), c('e', 'f')), c(TRUE, FALSE))
    
    srank.month  = srank.raster(obs_ia[[1]], obs_ia[[2]], 'g', 'Monthly rank')
    if (nlayers(obs_ia[[1]])>12)
        srank.annual = srank.raster(obs_ia[[1]], obs_ia[[2]], 'h', 'Annual rank', season = 8)
    else plot.new()               
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
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth,
                            labScale = maxObsV[1], nguides = 4, col = "blue")
        polarPlot.addGuides(xlim = xlim, axisMonth = axisMonth+1,
                            labScale = maxObsV[2], nguides = 4, col = "red")

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
    mtext.units(paste0(names(files)[1], ' (', units[1], ')'), col = "blue", adj = 0, side = 3, line = -2)
    mtext.units(paste0(names(files)[2], ' (', units[2], ')'), col = "red", adj = 0, side = 3, line = -3.8)

dev.off()
}

runAll("GFED4s_vs_INPE_FC")

files[1] = "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc"
runAll("Modis_vs_INPE_FC")

runAll("Modis_vs_INPE_FC_2019", Layers = c(217:227, 227))

files = c("Burnt area (GFED4s)" = "outputs/amazon_region/burnt_area-GFED4s_2.5degree_2001-2016.nc",
          "Burnt area (MCD64A1)" = "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc")

units = c('%', '%')  
aa_limits[2] = aa_limits[1] 
scale = c(100, 100)
#runAll("GFED4s_vs_Modis")

files = c("Burnt area -MCD64A1" = 
            "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc",
          "Burnt area -Model" =
            "outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc")


mod_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/model_summary.nc'

mod_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/sample_no_7560.nc'
#runAll("MODIS_vs_Model")

runAll("MODIS_vs_Model_2019", Layers = 217:228)
