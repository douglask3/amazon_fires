library(raster)
source("libs/make_transparent.r")
#source("libs/plotStandardMap.r")
library(rasterExtras)  
source("libs/Tnorm0.r")    

error_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/fire_summary_precentile.nc' 
uncert_file = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/model_summary.nc'    

obs_file = "inputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc"    

regions = list(A = -c(71.25, 63.75, 11.25,  6.25),
               B = -c(61.25, 53.75, 11.25,  6.25),  
               C = -c(48.25, 43.25,  8.75,  1.25),
               D = -c(66.25, 58.75, 18.75, 13.75),   
               E = -c( 61.25, 53.75, 23.75, 18.75), 
               "F All Deforested" = 'outputs/amazon_region/treeCoverTrendRegions.nc')       
regions = list("A" = -c(76.25, 66.25, 11.25, 6.25),
               "B" = -c(66.25, 58.75, 11.25,  6.25),               
               "C" = -c(58.75, 51.25, 8.75,  3.75),  
               "D" = -c(51.25, 46.25,  8.75,  1.25),
               "E" = -c(61.25, 56.25, 21.25, 13.75),
               "F" = 'inputs/amazon_region/treeCoverTrendRegions-F.nc',
               "G" = 'inputs/amazon_region/treeCoverTrendRegions-G.nc',
               "H" = 'inputs/amazon_region/treeCoverTrendRegions-H.nc',
               "I" = 'inputs/amazon_region/treeCoverTrendRegions-I.nc')
 
mnth_tested = c(224, 225, 116, 117)
quantiles_tested = seq(10, 90, 10)
quantiles_tested = sort(c(quantiles_tested, 42:48, 78:99))
quantiles_tested = 1:99


testMonth <- function(mnth, region, regionName) {
    print(regionName)
    print(mnth)

    temp_file_all = paste0('temp/', 'likiihood_resultsX4_month-', mnth, '-region-', 
                       regionName, '.Rd')
    if (file.exists(temp_file_all)) {
        load(temp_file_all)
        return(out)
    }
    
    obs = brick(obs_file)
    mnths = seq(mnth - floor(mnth/12)*12, nlayers(obs), by = 12)

    cropRegion <- function(rin) {
        if (is.character(region)) {
            out = rin
            browser()
            out[raster(region)!=6] = NaN
            out = crop(out, extent( -83.75, -50, -23.3, 0)) 
            
        } else {
            out = raster::crop(rin, extent(region))
        }
        out[out > 9E9] = NaN
        return(out)
    }
    obs = cropRegion(obs[[mnth]])
    
    obsMean = brick(obs_file)[[mnths]]
    obsMean = mean(cropRegion(obsMean))

    openMod <- function(mn, i, file = error_file) 
        cropRegion(brick(file, level = mn))[[i]]
    
    openMeanModLevels <- function(i, ...) {
        temp_file = paste0('temp/MeanSimLevelsforMnthx-', mnth, '-level-', i,
                            '-region-', regionName, '.nc')
        if (file.exists(temp_file)) {
            out = raster(temp_file)
        } else {
            print(i)
            mod_level = layer.apply(mnths, openMod, i, ...)         
            out = mean(mod_level)                       
            out = writeRaster(out, file = temp_file)
        }
        
        return(out)
    }

    makeModMean <- function(name, ...) {
        temp_file = paste0('temp/MeanSimLevelsforMnth-', mnth, '-name-', name,
                            '-region-', regionName, '.nc')
        if (file.exists(temp_file)) return(raster(temp_file))
        mod_mn = layer.apply(mnths, openMod, 50, ...) 
        mod_sd = layer.apply(mnths, openMod, 84, ...) 
        nl = nlayers(mod_mn)
        mod_mn = mean(mod_mn)
        mod_sd = mean(mod_sd^2)
        
        mask = !is.na(mod_mn + mod_sd)
        out = mod_mn
        getLevel <- function(q) {
            print(q)
            out[mask] = mapply(qnorm, mod_mn[mask], mod_sd[mask], p = q)
            out
        }
        outp = layer.apply(seq(0.01, 0.99, 0.01), getLevel)
        outp = writeRaster(outp, temp_file)
        outp
    }
    
    mod = layer.apply(quantiles_tested, openMod, mn = mnth)
    modU = layer.apply(c(5, 95), openMod, mn = mnth, file = uncert_file)
    modMean  = openMeanModLevels(50)
    modMeanU = makeModMean('uncert', file = uncert_file)
    #layer.apply(quantiles_tested, openMeanModLevel)   
    #modMeanU = mean.Tnorm(openMeanModLevel(50, file = uncert_file),
    #                      openMeanModLevel(84, file = uncert_file))

    meanrasterNA <- function(...) mean.raster(..., na.rm = TRUE)
    which_q <- function(o, m) {
        ti = which(o < m)[1] -1
        
        if (is.na(ti)) ti = tail(quantiles_tested,1) 
        else if (ti>0) ti = quantiles_tested[ti]
        return(ti)
    }

    obsv  = meanrasterNA(obs)
    modv  = unlist(layer.apply(mod, meanrasterNA))
    obsva = meanrasterNA(obs/obsMean)
    modva = modv/unlist(layer.apply(modMean, meanrasterNA))
    t1 = which_q(obsv, modv)
    t2 = which_q(1, modva)
    t3 = which_q(obsva, modva)
   
    modUOverMeanU = modU
    modUOverMeanU[] = modU[]/modMeanU[]
    out = c(obsv, obsva,
           unlist(layer.apply(modU, meanrasterNA)),
           unlist(layer.apply(modUOverMeanU, meanrasterNA)), #[[50]]
           t1, t2, t3)
    save(out, file = temp_file_all)
    return(out)
}

testRegion <- function( ...) {
    out = sapply(mnth_tested, testMonth, ...)
    colnames(out) = mnth_tested
    
    rownames(out) = c("Observed burnt area", "Observed anomoly",
                      "Model burnt area 5", "Model burnt area 95",
                      "Model anomoly 5", "Model anomoly 95",
                      "t1", "t2", "t3")
    out[1,] = out[1,] * 100
    out[3:4,] = out[3:4,]*100
    out[7:9,] = 100 - out[7:9,]
    return(out)
}

tab = mapply(testRegion, regions, names(regions), SIMPLIFY = FALSE)
tab = lapply(tab, round, 3)
tab = lapply(tab, t)
mapply(write.csv,tab, paste0('docs/probTable-', names(regions), '.csv'))

