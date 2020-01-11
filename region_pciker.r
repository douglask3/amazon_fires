## region picker
library(raster)
treeCover = 'data/treecover-2001-June2018.nc'

extent = extent(c(129, 155, -44, -26))

dat = mean(crop(brick(treeCover), extent))   
