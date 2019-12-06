library(raster)
library(rasterExtras)
library(reldist)
library(sn)
graphics.off()
dat = brick('outputs/amazon_region/fire_counts/firecount_TERRA_M__T.nc') 
mask = !is.na(dat[[1]])
pc_samples = 100
nboots = 100
den = FALSE
lims = c(0, 2000)
bin_size0 = 2

binPlt = list(c(0, 200), c(900, 1100), c(1800, 2000))


temp_file_vs = 'temp/values4errorAssement'

indexs = sample(1:nlayers(dat), size = ceiling(nlayers(dat) * (pc_samples/100)^(1/3)),
                replace = FALSE)

npnts = sum.raster(mask)

select_cells <- function(r) {
    smpl = sample(1:npnts, size = npnts * (pc_samples/100)^(2/3), replace = FALSE)
    return(r[mask][smpl])
}

temp_file_vs  = paste0(temp_file_vs, pc_samples,'-', nboots)
temp_file_vsi = paste0(temp_file_vs,  '.Rd')
if (!file.exists(temp_file_vsi)) {
    vdat = unlist(layer.apply(dat, select_cells))
    vdat = rep(vdat, nboots)
    save(vdat, file = temp_file_vsi)
} else load(temp_file_vsi)

rdat = vdat
MODE <- function(v) {
    v = round(v)
    unique_v = unique(v)
    unique_v[which.max(tabulate(match(v, unique_v)))]
}

randomTest <- function(f_random) {
    png(paste0('figs/error_sim-', f_random, '.png'),
        height = 8, width = 6, res = 300, units = 'in')
    layout(rbind(c(1,1,1,1, 1),c(2, 0, 3, 0, 4)),
           heights = c(2, 1), widths = c(1, 0.5, 1, 0.5, 1))
    par(mar =  c(3,0, 2, 0), oma = c(0, 3, 0, 1))
    
    temp_file_vs  = paste0(temp_file_vs, '-random', f_random)
    temp_file_vsi = paste0(temp_file_vs, '.Rd')
    if (!file.exists(temp_file_vsi)) {
        nsmples = length(vdat)    
        index = sample(1:length(vdat), nsmples, replace = TRUE)
        rdat = rdat * (1-f_random) + vdat[index] * f_random
        save(rdat, file = temp_file_vsi)
    } else load(temp_file_vsi)

    plot(0, xlim = lims, ylim = lims, type = 'n', xlab = '', ylab = '')
    mtext(side = 1, 'Obs. fire counts', line = 2)
    mtext(side = 2, 'Rnd fire counts', line = 2)
    if (den) {
        cols = blues9[unlist(mapply(rep, 1:9, 9 + (1:9)^5))]
        cols = densCols(vdat,rdat, colramp = colorRampPalette(cols))
        points(rdat~vdat, pch = 20, cex = 2, col = cols)
    } else {
        for (i in 1) points(vdat, rdat, pch = 20, cex = 1, col = '#99999922')
    }
    lines(c(1, 9E9), c(1, 9E9), lty = 3)

    binHist <- function(mn, mx, print = TRUE, mean = TRUE, plot = TRUE, ...) {
        if (print) print(mn)
        test = vdat > mn & vdat < mx
        sdat = rdat[test]
        if (sum(test) < 50) {
            if ((mx - mn) > 50) return(NULL)
            mn = max(0, mn - 1)
            mx = mx + 1            
            return(binHist(mn, mx, FALSE, mean, ...))
        }
            
        if (mean) return(c(mean(sdat), median(sdat), MODE(sdat),
                           sd(sdat), quantile(sdat, c(0.05, 0.95)))) 
        
        hist = hist(sdat, 1000, plot = plot, ...)
        return(cbind(hist$mids, hist$counts))
    }
    index = seq(lims[1], lims[2], by = bin_size0)
    temp_file_vs = paste0(temp_file_vs, '-binsize-', bin_size0)
    temp_file_vsi = paste0(temp_file_vs, '.Rd')
    if(!file.exists(temp_file_vsi)) {
        out = mapply(binHist, head(index, -1), tail(index, -1))
        save(out, file = temp_file_vsi)
    } else load(temp_file_vsi)
    
    null_index = !sapply(out, is.null)
    x = apply(cbind(head(index, -1), tail(index, -1)), 1, mean)
    y = sapply(out[null_index], function(i) i)
    x = x[null_index]
    #lines(x, y[1,], col = 'red', lwd = 2)
    #lines(x, y[1,] + y[4,], lty = 2, col = 'red', lwd = 2)
    #lines(x, y[1,] - y[4,], lty = 2, col = 'red', lwd = 2)
    
    #lines(x, y[3,], col = 'green')
    
    xtest =  0:10000
    testLnorm <- function(md) {        
        y = dlnorm(xtest, log(md), 0.5)
        wtd.quantile(xtest, c(0.05, 0.5, 0.95), weight = 100* y/max(y))
    }
    sdd = sd(vdat - rdat)
    testTnorm <- function(md) {
        print(md)        
        y = dnorm(xtest, md, sdd)
        wtd.quantile(xtest, c(0.05, 0.5, 0.95), weight = 100* y/max(y))
    }

    testSnorm <- function(md) {
        print(md)        
        y = dsn(xtest, md, sdd, 6)
        wtd.quantile(xtest, c(0.05, 0.5, 0.95), weight = 100* y/max(y))
    }

    testPoisson <- function(md) {
        print(md)        
        y = dpois(xtest, md)
        wtd.quantile(xtest, c(0.05, 0.5, 0.95), weight = 100* y/max(y))
    }
    
    temp_file_vs = paste0(temp_file_vs, '-Dist5.Rd')
    print(temp_file_vs)
    if(!file.exists(temp_file_vs)) {
        tnorm = sapply(y[1,], testTnorm)
        lnorm = sapply(y[2,], testLnorm)
        snorm = sapply(y[1,], testSnorm)
        poisson = sapply(y[1,], testPoisson)
        save(lnorm, tnorm, snorm, poisson, file = temp_file_vs)
    } else {
        load(temp_file_vs)
    }
    
    #lines(x, lnorm[2,], col = 'green', lwd = 2)
    #lines(x, lnorm[1,], col = 'green', lty = 2, lwd = 2)
    #lines(x, lnorm[3,], col = 'green', lty = 2, lwd = 2)
    
    lines(x, poisson[2,], col = 'red', lwd = 2)
    lines(x, poisson[1,], col = 'red', lty = 2, lwd = 2)
    lines(x, poisson[3,], col = 'red', lty = 2, lwd = 2)
    
    lines(x, snorm[2,], col = 'green', lwd = 2)
    lines(x, snorm[1,], col = 'green', lty = 2, lwd = 2)
    lines(x, snorm[3,], col = 'green', lty = 2, lwd = 2)

    lines(x, tnorm[2,], col = 'blue', lwd = 2)
    lines(x, tnorm[1,], col = 'blue', lty = 2, lwd = 2)
    lines(x, tnorm[3,], col = 'blue', lty = 2, lwd = 2)

    
    lines(x, y[2,], col = 'black', lwd = 1)
    lines(x, y[5,], col = 'black', lty = 2, lwd = 1)
    lines(x, y[6,], col = 'black', lty = 2, lwd = 1)

    addRnge <- function(xs) 
        axis(side = 1, at = xs, labels = c('', ''), line = 3)
    
    lapply(binPlt, addRnge)
    
    plotBin <- function(rx) 
        binHist(rx[1], rx[2], mean = FALSE, xlab = '', ylab = '', yaxt = 'n',
                main = paste(rx, collapse = ' - '))
    
    lapply(binPlt, plotBin)
    dev.off()   
}

randomTest(0.05)
randomTest(0.5)
randomTest(0.9)
randomTest(0.1)
#randomTest(0.99)
