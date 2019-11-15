library(raster)
library(rasterExtras)
source("libs/sd.raster.r")
source("libs/filename.noPath.r")

file_obs = 'outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc'
dir_sim  = 'outputs/sampled_posterior_ConFire_solutions-firecount/constant_post_2018_2/'

sampleYrs = seq(1, 15, 5)

nfiles = 30

mnth = unlist(lapply(sampleYrs, function(i) seq((i-1) * 12 + 1, i*12)))

brickMask <- function(...) {
    r = brick(...)
    r[r>9E9] = NaN
    r
}

obs = brickMask(file_obs)[[mnth]]

files = list.files(dir_sim, full.names = TRUE)
files = files[grepl('sample_', files)][1:nfiles]

sims = lapply(files, function(file) brickMask(file, varname = "burnt_area")[[mnth]])

mask = !is.na(obs[[1]]) & !is.na(sim[[1]][[1]])
dat = lapply(sims, function(sim) cbind(as.vector(obs[mask]), as.vector(sim[mask])))
dat = do.call(rbind, dat)
sd_dat = sd(apply(dat, 1, '-'))
dat = log(dat)
dat = dat[!apply(is.infinite(dat), 1, any),]

xyrange = range(dat)
xyrange[1] = 0

png('figs/obs_vs_sim.png', height = 7, width = 7, res = 300, unit = 'in')
plot(xyrange, xyrange, type = 'n', xlab = '', ylab = '', axes = FALSE)
mtext(side = 1, 'Obs. fire counts', line = 2)
mtext(side = 2, 'Sim. fire counts', line = 2)

cols = blues9[unlist(mapply(rep, 1:9, 9 + (1:9)^3.88))]
cols = densCols(dat[,1],dat[,2], colramp = colorRampPalette(cols))
points(dat[,2]~dat[,1], pch = 20, cex = 2, col = cols)

mn = seq(0, xyrange[2], length.out = 2000)
mn = exp(mn)
mnLow = log(mn - sd_dat)
mnHigh = log(mn + sd_dat)

mn = log(mn)

lines(mn, mn, col = 'red')      
lines(mn, mnHigh, col = 'red', lty = 2)    
lines(mn, mnLow, col = 'red', lty = 2)    


at = c(1, 3, 10, 30, 100, 300, 1000)
axis(side = 1, at = log(at), labels = at)
axis(side = 2, at = log(at), labels = at)
dev.off()
