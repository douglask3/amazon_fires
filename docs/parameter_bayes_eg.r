library(raster)
source("../rasterextrafuns/rasterPlotFunctions/R/mtext.units.r")
graphics.off()
pfile = 'outputs/params-test_errors-2002-attempt2-NewMoist-DeepSoil2Tnorm-burnt_area_MCD.csv'
reds9   = c("#FFFBF7", "#F7EBDE", "#EFDBC6", "#E1CA9E", "#D6AE6B", 
            "#C69242", "#B57121", "#9C5108", "#6B3008")

params = read.csv(pfile)

carbon = raster('../fireMIPbenchmarking/data/benchmarkData/avitabile_carbon_veg_05.nc')
cover = mean(brick('../LimFIRE/outputs/vegcover2000-2014.nc'))
cover = raster::crop(cover, carbon)
mask = !is.na(carbon+cover)
carbon = carbon[mask]; cover = cover[mask]

grid = matrix(0, ncol = 100, nrow = 100)
carbP = 1:100
covP = 1:100

coverp = cover * 100
addGrid <- function(crp, cvp) {
    grid[crp, cvp] <<-  sum(carbon < crp & carbon > (crp-1) & coverp < cvp & coverp >(cvp -1))
}
for (crp in carbP) for (cvp in covP) addGrid(crp, cvp)
grid = apply(grid, 1, function(i) i/sum(i))
image(carbP, covP, sqrt(grid))
#browser()
cols = reds9[round(seq(1, 9^5)^(1/5))]#[sort(unlist(sapply(1:length(reds9), function(i) i:length(reds9))))]
#test = carbon < 50
carbonp = carbon; coverp = 100*cover
Nc = sapply(0:max(ceiling(carbonp)), function(i) sum(carbonp > (i-1) & carbonp <i))
ps = Nc[ceiling(carbonp)+1]
ps = 1 - ps/max(ps)

test = sample(1:length(ps), 20000, TRUE, ps)
carbonp= carbonp[test] ; coverp = coverp[test]

cols = densCols(carbonp, coverp, colramp = colorRampPalette(cols), bandwidth = 1)
graphics.off()
png("figs/jules_param_convert.png", height = 3, width = 3, res = 300, units = 'in')
par(mar = c(3.1, 3.5, 0.5, 0.1))
plot(carbonp, coverp, pch = 19, col = cols, xlab = '', ylab = '', xlim = c(0.1, 350), log = 'x')
mtext.units('Vegetation carbon (gC ~ha-1~)', side = 1, line = 2)
mtext.units('Vegetation cover (%)', side = 2, line = 2)

fit = nls(cover ~ b*(1-exp(-a*carbon)), algorithm = "port", 
          start = list(a = 0.01, b = 1), lower = list(a =0, b=0.9))
x = seq(0.1, max(carbon), 0.1)
y = predict(fit, newdata=data.frame(carbon = x))
lines(x, y*100, lwd = 3)

fModx = 0.38/0.45
fMod = predict(fit, newdata=data.frame(carbon=fModx))*100 #/coefficients(fit)[1]

lines(c(fModx, fModx), c(0, fMod), col = "#542788", lwd = 2)
lines(c(0.1, fModx), c(fMod, fMod) , col = "#542788", lwd = 2)
dev.off()
browser()
makeExtinction <- function(x0, k, y0, xy0, xy1) {
    x0 = params[,x0]; k = params[,k]
    ymin = 1/(1+exp(-k*(xy0-x0))); ymax = 1/(1+exp(-k*(xy1-x0)))
    yp = ymin + y0 * (ymax - ymin)
    #yp = 0.1
    
    z = -(log((1/yp) - 1)/k) + x0    
}

#fExt = params[,'fuel_x0'] * 100
#mExt = 100 - params[,'moisture_x0'] * 100
fExt = makeExtinction('fuel_x0', "fuel_k", 0.1, 0, 1) *100
mExt = 100-makeExtinction('moisture_x0', "moisture_k", 0.1, 0, 1)*100

fMod = predict(fit, newdata=data.frame(carbon=0.38))*100 /coefficients(fit)[1]
mMod = 0.18 * 100

fPri = 50
mPri = 50
png("figs/Prior_post_inf.png", height = 3, width = 3, res = 300, units = 'in')
par(mar = c(3.5, 3.5, 0.1, 0.1))
plot(c(0, max(c(fExt, fMod, fPri))), c(0, max(c(mExt, mMod, mPri))), type  = 'n', xlab = '', ylab = '')

mtext('Fuel discontinuity point (%)', side = 1, line = 2)
mtext('Moisture of extinction (%)', side = 2, line = 2)

addPoints <- function(cols, x, y, maxCex, minCex = 0.1) {
    cexs = seq(sqrt(maxCex), sqrt(minCex), length.out = length(cols))^2
    mapply(points, cex = cexs, col = cols, MoreArgs = list(x = x, y = y, pch = 19))
}

cols_p = make.transparent("black", sqrt(seq(0.98, 0, length.out = 14)))
addPoints(cols_p, fPri, mPri, 60)

cols = c('#ffffcc','#ffeda0','#fed976','#feb24c','#fd8d3c','#fc4e2a',
         '#e31a1c','#bd0026','#800026')
addPoints(cols, fExt, mExt, 8, 1.5)

cols_p = make.transparent(cols_p, 0.8)
addPoints(cols_p, fPri, mPri, 60)

cols =  make.transparent(cols, 0.95)
smp = sample(1:length(fExt), 1000)
addPoints(cols, fExt[smp], mExt[smp], 8, 1.5)

lines(c(fMod, fMod), c(-9E9, 9E9), col = 'green', lwd = 2)
lines(c(-9E9, 9E9), c(mMod, mMod), col = 'blue', lwd = 2)
points(fMod, mMod, pch = 19, cex = 2, col = 'blue')
dev.off()
