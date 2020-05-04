graphics.off()
library(raster)

fire = 'outputs/amazon_region/fire_counts/burnt_area_MCD64A1.006.nc'
tree = 'outputs/amazon_region/vegetation/treecover-2001-June2018.nc'
agri = 'outputs/amazon_region/human/fract_agr2001-2019.nc'
extent =  extent( -83.75, -28.75, -15, 0)

fire = brick(fire)
tree = brick(tree)[[210]]
agri = brick(agri)[[210]]

fire = crop(fire, extent)
tree = crop(tree, extent)
agri = crop(agri, extent)

tree = tree/0.8
tree = (tree + (1/2) * tree * agri/(1-tree-agri))

tree[tree > 1] = 1
tree = tree * 0.8
#treeLayer = nlayers(tree)
#tree = addLayer(tree, tree[[rep(210, 18)]])
fireA = fire
fireA[!is.na(fire)] = 1

mask = raster('outputs/amazon_region/treeCoverTrendRegions.nc')
mask = crop(mask, extent)
mask[!(mask == 6 | mask == 3)]=NaN

treeMean <- function(cov1, cov2) {
    print(c(cov1, cov2))
    tempFile = paste('temp/mean_fire_per_tree_cover_for_', cov1, cov2, '.Rd')
    test = tree > cov1 & tree < (cov2)
    
    if (file.exists(tempFile)) {
        load(tempFile) 
        fireA <<- fireA
    } else {
        meanF = 0
        ar = area(tree[[1]]); art = 0
        FindForMonth <- function(mn, fireA) {
            print(mn)
            for (i in seq(mn, treeLayer, by = 12)) {
               # browser()
                meanF = meanF + sum(fire[[i]][test[[i]]])# * ar[test[[i]]])
                art = art + sum.raster(test[[i]], na.rm = TRUE)#um(ar[test[[i]]])
            }
            
            if (art == 0) return(fireA)
            meanF = meanF/art
            if (meanF == 0) FUN <- function(i) 1
                else FUN <- function(i) fire[[i]][test[[i]]] / meanF
            for (i in seq(mn, nlayers(fireA), by = 12)) 
                fireA[[i]][test[[i]]] =  FUN(i)
            #if (mn == 7) browser()
            return(fireA)
        }
        for (mn in 1:12) fireA = FindForMonth(mn, fireA)
        save(fireA, file = tempFile)
    }
    return(fireA)
}
#covs = quantile(tree[], na.rm = TRUE, seq(0, 1, 0.1))
#for (i in 1:(length(covs)-1)) fireA =  treeMean(covs[i], covs[i+1])

## covert fire to anomolies
make_fire_anom <- function(mn) {
    mn12 = mn - floor(mn/12)*12
    if (mn12 == 0) mn12 = 12
    return(fire[[mn]]/fire_clim[[mn12]])
}
fire_clim = layer.apply(1:12, function(mn) mean(fire[[seq(mn, nlayers(fire), by = 12)]]))
fire_clim[fire_clim == 0]=1
fireA = layer.apply(1:nlayers(fire), make_fire_anom)

cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#bd0026','#800026')
limits = 1/(1+exp(-seq(-10, 0, 0.1)))

dcols = rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4'))

dlimits = 50^(seq(-0.2, 0.2, 0.02))
#dlimits =  c(-0.02, -0.01, -0.005, -0.002, -0.001, 0.001, 0.002, 0.005, 0.01, 0.02)

cols = make.transparent(make_col_vector(cols, limits = limits), 0.8)
dcols = make.transparent(make_col_vector(dcols, limits = dlimits+1), 0.8)

tree_vs_fire_timeSlide <- function(i, fire, limits, cols) {
    
    fire = fire[[i]]
    tree = tree[[i]]
    mask = !is.na(fire + tree) & mask == 6
    
    fire = fire[mask]
    tree = tree[mask]
    x = rep(i, length(tree))
    
    cts = cut_results(fire, limits)
    cols = cols[cts]
    for (nn in 1:1) for (cex in c(2, 1, 0.5, 0.2, 0.1))
        points(x, 100-tree*100, col = cols, pch = 19, cex = cex)
    
}

tree_vs_fire_timeSlide2 <- function(i, fire, limits, cols, ...) {
    if (is.na(mask[i])) return()
    if (is.na(fire[[1]][i])) return()
    x = 0:228
    y = 100 -tree[i]*100
    z = fire[i]
    cols = cols[cut_results(z, limits)]
    mapply(function(x1, x2, col) lines(c(x1, x2), c(y, y), col = col, ...),
           head(x, -1), x[-1], cols)
    
    #lines(x, y, col = cols, ...)
}

doThePlot <- function(...) {
    plot(c(0, 229), c(1, 100), pch = 19, cex = 1000, ylim = c(100, 17), log = 'y', 
        axes = FALSE, xaxs = 'i', yaxs = 'i')
    labels = c(80, 75, 70, 60, 40, 20, 0)
       
    axis(2, at = 100 - labels, labels = labels)
    randomPlot <- function() {
        
        for (lwd in c(5, 3, 2, 1, 0.5, 0.1)) {
            print(lwd)
            index = sample(1:length(mask), size = round(length(mask)), replace = FALSE)
            lapply(index, tree_vs_fire_timeSlide2, lwd = lwd, ...)
        }
    }
    for (nn in 1:3) randomPlot()
    #for (i in 1:5) {
    #    lapply(1:228, tree_vs_fire_timeSlide, ...)
    #    lapply(228:1, tree_vs_fire_timeSlide, ...)
    #}
}
png("figs/NewFig1Mabe.png", height = 150, width =183, units = 'mm', res = 300)
    par(mfrow = c(2, 1), mar = rep(0.5, 4), oma = c(2.5, 2.5, 0.5, 2.5))
    doThePlot(fire, limits, cols)
    doThePlot(fireA, dlimits, dcols)
dev.off()
