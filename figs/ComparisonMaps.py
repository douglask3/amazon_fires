
import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import iris.analysis.cartography
import iris
import iris.quickplot as qplt
import iris.plot as iplt


#Load in GFED data
GFED = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/GFED_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = GFED.coord('time')
print GFED

#Get mean over time finds monthly mean, so multiply by 12 to get annual total burned area. Multiply by 100 to get percentage
GFED = GFED.collapsed('time', iris.analysis.MEAN)*12*100

plt.subplot(1,2,1)
cb = iplt.pcolormesh(GFED, vmin=0.0, vmax=15.0)
plt.colorbar(cb, orientation='horizontal', label='Percentage')
plt.gca().coastlines()
plt.title('GFED')

#Load in satellite data
cube = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/TERRA_MT_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = cube.coord('time')
print cube

#Mean over the period
cube = cube.collapsed('time', iris.analysis.MEAN)

plt.subplot(1,2,2)
cb2 = iplt.pcolormesh(cube)
plt.colorbar(cb2, orientation='horizontal', label='Fire Counts')
plt.gca().coastlines()
plt.title('TERRA')

plt.suptitle('Mean Annual Burned Area, 2001-2016', fontsize=15)
plt.show()


