graphics.off()
library(snow)
library(raster)
library(rasterExtras)
library(benchmarkMetrics)
source("libs/make_transparent.r")
source("libs/srank.raster.r")
source("libs/modal_approx.r")

obs = 'inputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc'
sim_dir = 'outputs/sampled_posterior_ConFire_solutions-burnt_area_MCD-Tnorm/constant_post_2018_full_2002-attempt2-NewMoist-DeepSoil/'
sim_dir = "outputs/sampled_posterior_ConFire_solutions_squishy_full_crop2/constant_post_2018_full_2002_BG2020_rev_logitnorm_Long_PT/"

fireMonths_early = 6:9
fireMonths_late  = 9

Tfname0 = 'temp/benchmarkingSTuff-rev3'
mask    = NULL
extent  = NULL

col_null = c("#e0f3f8", "#74add1", "#313695")
col_ss   = c("#a50026", "#bf812d", "#ffff99")

getAllMonths <- function(fireMonths, dat) {
    months = lapply(fireMonths, seq, nlayers(dat), by = 12)
    monthsByYr = lapply(1:min(sapply(months, length)),
                        function(yr) sapply(months, function(m) m[yr]))
    return(monthsByYr)
}

brickR <- function(r, ...) {
    r = brick(r, ...)[[1:216]]
    if (!is.null(extent)) r = raster::crop(r, extent)
    
    r[r>9E9] = NaN
    if (!is.null(mask)) r[!mask] = NaN
    return(r)
}
obs = brickR(obs)

fireMonths_early = getAllMonths(fireMonths_early, obs)
fireMonths_late  = getAllMonths(fireMonths_late , obs)


obs.aa    = mean(obs)
obs.JJA   = layer.apply(fireMonths_early, function(i) mean(obs[[i]]))
obs.Sep   = layer.apply(fireMonths_late , function(i) mean(obs[[i]]))

obs.aaJJA = mean(obs.JJA)
obs.aaSep = mean(obs.Sep)

sim_files = list.files(sim_dir, full.names = TRUE)
sim_files = sim_files[grepl('sample_no_', sim_files)]#[1:30]

runTempFile <- function(file, FUN, Tfname, Tfname0, MoreArgs) {
    id = strsplit(strsplit(file, 'sample_')[[1]][2], '.nc')[[1]]
    fname = paste0(Tfname0, Tfname, '-', id, '.Rd')
    
    if (file.exists(fname)) {
        load(fname)
    } else {
        out = do.call(FUN, c(file, MoreArgs))
        save(out, file = fname)
    }
    return(out)
}

runAllMember <- function(FUN, Tfname, MoreArgs) {
    #out = lapply(sim_files, runTempFile,  FUN, Tfname = Tfname,Tfname0, MoreArgs)
    #return(out)
    cl = makeSOCKcluster(c("localhost", "localhost", "localhost", "localhost", "localhost"))
        clusterExport(cl=cl,
                      list("brick", "brickR", "NME", "NME.default",
                            "NMSE", "NMSE.default", "score", "score.NME", 
                            "mean.raster", "layer.apply", "nlayers", "YearlySeason",
                            "srank.raster", "mask", "extent"))
        out = parLapply(cl, sim_files, runTempFile, FUN, Tfname = Tfname, Tfname0, MoreArgs)
    stopCluster(cl)    
    return(out)
} 
    

NME_months <- function(file, mnths, obs) {
    library(raster)
    sim = brickR(file, varname = "burnt_area_mode")
    if (!is.na(mnths)) sim  = mean(sim[[mnths]])
    
    nme  = NME (obs, sim)
    nmse = NMSE(obs, sim)
    
    out = rbind(score(nme), score(nmse))
    return(out)
}

NME_null <- function(x) {
    FUN <- function(met, nullMet) {
        med = score(met(x, median(x[], na.rm = TRUE)))[1]
        c(med, nullMet(x)[1:2])
    }
    rbind(FUN(NME, null.NME), FUN(NMSE, null.NMSE))
}
#
#comp.aa.sc = runAllMember(NME_months, 'NME_annualAverage', 1:nlayers(obs), obs.aa)
#comp.aa.nm = NME_null(obs.aa)
#comp.aaJJA = runAllMember(NME_months, 'NME_aaJJA', unlist(fireMonths_early), obs.aaJJA)
#comp.aaSep = runAllMember(NME_months, 'NME_aaSep', unlist(fireMonths_late), obs.aaSep)

plotMT <- function(mod, null, mt = NULL) {   
    if (!is.null(mt)) {
        mod  = sapply(mod, function(i) i[mt,])
        null = null[mt,]
    }
    if (is.null(dim(mod))) mod = t(mod)
    percentile = apply(mod, 1, quantile, c(0.05, 0.95))
    pc_beat = sapply(null[1:2], function(j)100*apply(mod, 1, function(i) sum(i < j))/ncol(mod))

    
    xrange = range(mod, unlist(null))
    plot(xrange, c(0, 1), type = 'n', xlab = '', ylab = '', yaxt = 'n')
    
    addHist <- function(x, col) {
        xi = hist(x, 100, plot = FALSE)$mids
        yi = hist(x, 100, plot = FALSE)$counts
        yi = yi / max(yi)
        polygon(c(xi, tail(xi, 1), xi[1]), c(yi, 0, 0), col = col, lwd = 0.5)
    }
    
    if (!is.null(dim(pc_beat))) pc_beat = t(pc_beat)
    tab = rbind(round(percentile, 3), round(pc_beat,1))
   
    rownames(tab) = c("5%", "95%", "Median-beat", "Mean-beat")[1:nrow(tab)]
    tab = rbind(tab, null[[1]], null[[2]])
    rownames(tab) = c(rownames(tab), "Median-score", "Mean-score")[1:nrow(tab)]
    
    #if (is.null(nrow(mod))) mod =t(mod)
    addModHist <- function() for (mn in 1:nrow(mod)) 
            addHist(mod[mn,], col_ss[mn])    
    
    addHist(null[[3]], col_null[3])    
    addModHist()        
    addHist(null[[3]], make.transparent(col_null[3], 0.67))

    addVerLine <- function(i, col, ...) {
        lines(c(null[[i]], null[[i]]), c(-1, 2), lwd = 4)
        lines(c(null[[i]], null[[i]]), c(-1, 2), lwd = 1.5, col = col, ...)
    }
    addVerLine(1, col_null[1], lty = 4)
    addVerLine(2, col_null[2], lty = 3)

    col_ss = make.transparent(col_ss, 0.67)
    addModHist()
    return(tab)
}

plotNMEscores <- function(Tname, mnths, obs, mt = 1:2, select = FALSE) {
    scores = runAllMember(NME_months, Tname, list(mnths, obs))
    
    Tname = paste0(Tfname0, Tname, '-nulls.Rd' )
    if (file.exists(Tname)) load(Tname)
    else  {
        nulls  = NME_null(obs)   
        save(nulls, file = Tname)
    }
        
    tabs = lapply(mt, function(m) plotMT(scores, nulls, mt = m))
    if (select) {
        pScores = sapply(scores, function(i) i[2,1])
        select = pScores < 0.685
        return(select)
    }
    return(tabs)
}
png("figs/benchmarkingScore.png", width = 6, height = 9, units = 'in', res = 300)
par(mfrow = c(6, 2), mar = c(2, 0.5, 1.5, 0.5), oma = c(1, 3, 0.5, 0))

tabs = list()
tabs[['everything']] = plotNMEscores('NME_allandeveything', NaN, obs, 2, F)
mtext('a) NMSE Monthly spatial', adj = 0.1)
#sim_files = sim_files[select]

tabs[['annual_average']] = plotNMEscores('NME_annualAveragej', 1:nlayers(obs), obs.aa, 1)
mtext('b) NME Annual average', adj = 0.1)
tabs[['season_average']] = plotNMEscores('NME_aaJJA', unlist(fireMonths_early), obs.aaJJA, 1)
mtext('c) NME Fire season average', adj = 0.1)
#plotNMEscores('NME_aaSep', unlist(fireMonths_late), obs.aaSep, 1) 


modal_metric  <- function(file, obs) {
    source("libs/modal_approx.r")
    library(raster)
    print(file)
    sim = brickR(file, varname = "burnt_area_mode")
    sim[sim > 9E9] = NaN
    sim = layer.apply(1:12, function(i) mean(sim[[seq(i, nlayers(sim), by = 12)]]))
    sim = modal_approx(sim)
    
    nme  = NME (obs, sim)
    nmse = NMSE(obs, sim)
    
    return(rbind(score(nme), score(nmse)))    
}

obs.clim = layer.apply(1:12, function(i) mean(obs[[seq(i, nlayers(obs), by = 12)]]))
obs.modal = modal_approx(obs.clim)
comp.modal = runAllMember(modal_metric, 'modal_metric', obs.modal)
nulls.modal  = NME_null(obs.modal) 

tabs[['Modal']] = plotMT(comp.modal, nulls.modal, 1)
mtext('h) Modality', adj = 0.1)

obs_pc = PolarConcentrationAndPhase(obs)
grid_Area = raster::area(obs_pc[[1]], na.rm = TRUE)
total_Area = sum.raster(grid_Area, na.rm = TRUE)
MPDi <- function(file, obs, grid_Area, total_Area) {
    library(raster)
    library(rasterExtras)
    source("libs/PolarConcentrationAndPhase.r")
    sim = brickR(file, varname = "burnt_area_mode")
    
    sim = PolarConcentrationAndPhase(sim)

    mpd = sum.raster(grid_Area * acos(cos(obs[[1]] - sim[[1]])), na.rm = TRUE)/(pi * total_Area)
    con = score(NME(obs[[2]], sim[[2]]))
    return(c(mpd, con))
}

comp.sea.con = sapply(runAllMember(MPDi, 'MPDi', list(obs_pc, grid_Area, total_Area)), function(i) i)

comp.sea.nul = null.MPD(obs.clim)
comp.sea.nul.phase = list(comp.sea.nul[[1]][1],comp.sea.nul[[1]][1], comp.sea.nul[[2]][1,])
comp.sea.nul.conc  = NME_null(obs_pc[[2]])[1,]

tabs[['phase']] = plotMT(comp.sea.con[ 1,], comp.sea.nul.phase)
mtext('d) MPD Fire season phase', adj = 0.1)
tabs[['conc']] = plotMT(comp.sea.con[-1,], comp.sea.nul.conc)
mtext('e) NME Fire season concentration', adj = 0.1)

srankI <- function(file, obs, grid_Area, total_Area) {
    library(raster)
    library(rasterExtras)
    print(file)
    sim = brickR(file,varname = "burnt_area_mode")
    comparisons <- function(...) {
        comp = srank.raster(obs, sim, plot = FALSE, ...)
        sum.raster(grid_Area * comp, na.rm = TRUE)/total_Area
    }
    return(c(comparisons(), comparisons(season = 1:12)))
}

comp.srank = runAllMember(srankI, 'srankIi', list(obs[[1:216]], grid_Area, total_Area))
comp.srank.monthly = sapply(comp.srank, function(i) i[[1]])
comp.srank.annual  = sapply(comp.srank, function(i) i[[2]])

srank.rr <- function(i, Tname = 'srank_rr', ...) {
    temp_file = paste0(Tfname0, Tname, '-', i, '-nulls.Rd')
    if (file.exists(temp_file)) load(temp_file)
    else {
        print(temp_file)
        comp = srank.raster(obs, NULL, plot = FALSE, ...) 
        comp = sum.raster(grid_Area * comp, na.rm = TRUE)/total_Area
        save(comp, file = temp_file)
    }
    return(comp)
}

RR_monthly =  sapply(1:1000,srank.rr)#srank.raster(obs, NULL, plot = FALSE)
RR_annual  =  sapply(1:1000,srank.rr, 'srank_rr_season1-12', season = 1:12)

tabs[['monthly_rank']] = plotMT(comp.srank.monthly, list(0, 0, RR_monthly))
mtext('f) Spearmans rank monthly', adj = 0.1)
tabs[['annual rank']] = plotMT(comp.srank.annual , list(0, 0, RR_annual ))
mtext('g) Spearmans rank annual', adj = 0.1)


Tfname0 = 'temp/benchmarkingSTuff2-masked'

mask   = raster('outputs/amazon_region/treeCoverTrendRegions.nc') == 6
extent = extent( -83.75, -50, -23.3, 0)
mask   = crop(mask, extent)
obs0 = obs

obs= raster::crop(obs, extent)
r[!mask] = NaN
#obs = brickR(obs)

comp.srank = runAllMember(srankI, 'srankIi', list(obs[[1:216]], grid_Area, total_Area))
comp.srank.monthly = sapply(comp.srank, function(i) i[[1]])
comp.srank.annual  = sapply(comp.srank, function(i) i[[2]])

RR_monthly =  sapply(1:1000,srank.rr)#srank.raster(obs, NULL, plot = FALSE)
RR_annual  =  sapply(1:1000,srank.rr, 'srank_rr_season1-12', season = 1:12)

tabs[['monthly_rank_AAD']] = plotMT(comp.srank.monthly, list(0, 0, RR_monthly))
mtext('h) Spearmans rank monthly - AAD', adj = 0.1)
tabs[['annual rank_AAD']] = plotMT(comp.srank.annual , list(0, 0, RR_annual ))
mtext('i) Spearmans rank annual - AAD', adj = 0.1)


par(mar = rep(0,4))
plot.new()
legend("top", pch = 22, pt.bg = col_ss, c("\nStep 1\n", "Step 2", "Step 3"), bty = 'n', col = "black", horiz = TRUE)
plot.new()

legend("top", pch = c(NaN, NaN, 22), lwd = c(4, 4, 0), c("Median", "Mean", "Randonly-\nresampled\n"), col = "black", pt.bg = col_null, bty = 'n', horiz = TRUE,  x.intersp = 0.5)
legend("top", pch = c(NaN, NaN, 22), lwd = c(1.5, 1.5, 0), lty = c(4, 3, 0), c("Median", "Mean", "Randonly-\nresampled\n"), col = col_null, pt.bg = col_null, bty = 'n', horiz = TRUE,  x.intersp = 0.5)
dev.off()

