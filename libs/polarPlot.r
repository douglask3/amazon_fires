polarPlot.setup <- function(x, y, xlim = NULL, ylim = xlim, ...) {
    x = 2 * pi * x/12
    yn = y * sin(x)
    xn = y * cos(x)
    x = xn; y = yn
    if (is.null(xlim)) xlim = ylim = max(abs(x), abs(y)) * c(-1, 1)
    plot(x, y, pch = 19, cex = 2, xlab = '', ylab = '', axes = FALSE, xlim = xlim, ylim = ylim,...)
}

polarPlot.polygon <- function(x, y, ...) {
    browser()
}

polarPlot.addGuides <- function(xlim = c(-1, 1), ylim = xlim) {
    lines(c(0, 0), ylim, lwd = 3)
    lines(ylim, c(0, 0), lwd = 3)
    at = seq(0, 1, 0.2)
    at = at[at <= xlim[2]]
    
    mnths = 2 * pi *((0.5:11.5)/12)
    xr = xlim[2] * sin(mnths) * 1.07
    yr = ylim[2] * cos(mnths) * 1.07
    text(x = xr, y = yr, c('J', 'F', 'M', 'A', 'M', 'J', 'J', 'A', 'S', 'O', 'N', 'D'))
    
    
    addRadin <- function(mnths, ...) {
        xr = xlim[2] * sin(mnths) 
        yr = ylim[2] * cos(mnths) 
        mapply(function(i,j) lines(c(0, i), c(0, j), col = make.transparent("white", 0.67), ...), xr, yr)
        mapply(function(i,j) lines(c(0, i), c(0, j), col = make.transparent("black", 0.67), lty = 2, ...), xr, yr)
    }
    addRadin(2 * pi *((0:12)/12))
    for (i in 1:4) addRadin(2 * pi *(c(2, 5, 8, 11)/12), lwd = 2)
    
    addCirclegrid <- function(r) {
        xr = r * sin(seq(-pi, pi, 0.01) + pi/4)
        yr = r * cos(seq(-pi, pi, 0.01) + pi/4)   
        lines(xr, yr, col =  make.transparent("white", 0.33))
        lines(xr, yr, lty = 2,  col =  make.transparent("black", 0.33))
        if (r == 0) cex = 2 else cex = 4
        points(yr[1], xr[1], pch = 19, cex = cex, col = "white")
        text(y = yr[1], x = xr[1], r)                
    }
    lapply(at, addCirclegrid)
}