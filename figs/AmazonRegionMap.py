import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import iris.analysis.cartography
import iris
import iris.quickplot as qplt
import iris.plot as iplt
from matplotlib.patches import Polygon


#Load in GFED data
GFED = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/GFED_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = GFED.coord('time')

#Get the mean burned area over 2001-2016
GFED = GFED.collapsed('time', iris.analysis.MEAN)

#Make a blank copy to show region location
blank = GFED.copy()
blank.data[np.where((blank.data>0))] = 0.0

##Constrain to Amazon Region
#GFED=GFED.extract(iris.Constraint(latitude=lambda cell: (-57.5) < cell < (12.5), longitude=lambda cell: (-82.5) < cell < (-30)))  #South America
Region=GFED.extract(iris.Constraint(latitude=lambda cell: (-17.0) < cell < (0.0), longitude=lambda cell: (-72) < cell < (-44))) #Brazil/Amazon region

iplt.pcolormesh(blank, cmap='Greens')
cb = iplt.pcolormesh(Region, alpha=1)
plt.colorbar(cb, orientation='horizontal', label='Mean GFED Burned Area 2001-2016')
plt.gca().coastlines()
plt.title('Amazon region')

plt.show()


