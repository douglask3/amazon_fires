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

#Get the mean burned area over 2001-2016
GFED = GFED.collapsed('time', iris.analysis.MEAN)

##Constrain to Amazon Region
#GFED=GFED.extract(iris.Constraint(latitude=lambda cell: (-57.5) < cell < (12.5), longitude=lambda cell: (-82.5) < cell < (-30)))  #South America
Region=GFED.extract(iris.Constraint(latitude=lambda cell: (-17.0) < cell < (0.0), longitude=lambda cell: (-72) < cell < (-44))) #Brazil/Amazon region

cb = iplt.pcolormesh(GFED)
# iplt.pcolormesh(Region, alpha=1, color='k')
plt.colorbar(cb, orientation='horizontal', label='Mean GFED Burned Area 2001-2016')
plt.gca().coastlines()
plt.title('Amazon region')

#Plot black square to define Amazon region
plt.plot([-72, -44], [-17,-17], color='red', lw=2)
plt.plot([-72, -44], [0,0], color='red', lw=2)
plt.plot([-72, -72], [-17,0], color='red', lw=2)
plt.plot([-44, -44], [-17,0], color='red', lw=2)

plt.show()

