################################################################################
## cfg                                                                        ##
################################################################################
## Libraries etc
library(raster)
library(rasterExtras)
library(benchmarkMetrics)
source("make_inputs/fuel_moisture_equilibrium.r")
source("libs/writeRaster.Standard.r")

## paths and parameters
files = paste0('data/', c('precip.mon.mean.nc', 'rhum.mon.mean.nc', 'air.mon.mean.nc'))
mask_file = 'data/climate/climate_mask.nc'

fname_outs = paste0('outputs/climate/',c('emc', 'seasonDiff'), '-')

################################################################################
## load data                                                                  ##
################################################################################
loadDat <- function(file) {
    dat = brick(file)
    index =  which(sapply(strsplit(names(dat), '.', fixed = TRUE), substr, 2, 5)[1,] >= 2001)
    return(dat[[index]])
}

dat = lapply(files, loadDat)
################################################################################
##   make emc                                                                 ##
################################################################################

make_emc <- function(i)
    fuel_moisture_equilibrium(0, dat[[2]][[i]], dat[[3]][[i]])   


emc = layer.apply(1:nlayers(dat[[1]]), make_emc)
mask = raster(mask_file)

emc = raster::resample(emc, mask)

################################################################################
##   check height of dry season                                               ##
################################################################################
season = PolarConcentrationAndPhase(dat[[1]]*(-1), justPhase = TRUE, phase_units = 'months')
season = raster::resample(season, emc)
season_diff_from <- function(mn) {
    diff = mn - season
    test1 = diff > 6
    test2 = diff < (-6)
    diff[test1] = diff[test1] - 12
    diff[test2] = diff[test2] + 12
    
    return(diff)
}

season_diff = layer.apply(1:12, season_diff_from)

################################################################################
## output                                                                     ##
################################################################################
season_diff_out = layer.apply(1:nlayers(emc),
                          function(mn) season_diff[[mn - 12*ceiling((mn-12)/12)]])

years =  sapply(strsplit(names(dat[[1]]), '.', fixed = TRUE), substr, 2, 5)[1,]
fname = paste0(fname_outs, paste(range(years), collapse = '-'), '.nc')
writeRaster.Standard(emc, fname[1])
writeRaster.Standard(season_diff_out, fname[2])

