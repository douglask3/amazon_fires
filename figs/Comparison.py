#Step 1) Make GFED and TERRA the same size
'''
TERRA = months since 2001 (to 2019)
GFED = months since 2000 (to 2016)

ncks -d time,0,179 -o TERRA_MT_2001-2016_CB.nc firecount_TERRA_M__T.nc
ncks -d time,12,191 -o GFED_2001-2016_CB.nc burnt_area-GFED4s_2.5degree_2000-2016.nc
'''


# Step 2) Make plots
### South America total ###

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import iris.analysis.cartography
import iris
import iris.quickplot as qplt

#Load in GFED data
GFED = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/GFED_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = GFED.coord('time')
print GFED

#Get global total burnt area in km2)
#Divide by 1000000000000 to get Mkm2. Divide by 1000000 to get km2
for coord in coords:
    if not GFED.coord(coord).has_bounds():
        GFED.coord(coord).guess_bounds()
grid_weights = iris.analysis.cartography.area_weights(GFED)
GFED = GFED.collapsed(coords, iris.analysis.SUM, weights = grid_weights)/1000000

#Load in satellite data
cube = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/TERRA_MT_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = cube.coord('time')
print cube

#Get global total fire counts. 
#Divide by 1000000000000 to get Million. Divide by 10000000000 to get 10000
for coord in coords:
    if not cube.coord(coord).has_bounds():
        cube.coord(coord).guess_bounds()
grid_weights = iris.analysis.cartography.area_weights(cube)
cube = cube.collapsed(coords, iris.analysis.SUM, weights = grid_weights)/10000000000
print cube.data

fig, ax = plt.subplots()

x = np.arange(180)
ax.plot(x,GFED.data, color='black', label='GFED burned area (km$^2$)')
ax.plot(x,cube.data, color='blue', label='MODIS TERRA M-T \n(10000 fire counts)')
#Alternative: scatter plot
#plt.scatter(x,GFED.data, color='blue')
#plt.scatter(x,cube.data, color='black')

plt.xticks(np.arange(0, 192, step=12),("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))
plt.title('Fire data comparison')
plt.ylabel('Fire')
plt.xlabel('Years')
plt.legend()
plt.show()





### Amazon region ###

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

##Constrain to Amazon Region
#GFED=GFED.extract(iris.Constraint(latitude=lambda cell: (-57.5) < cell < (12.5), longitude=lambda cell: (-82.5) < cell < (-30)))  #South America
GFED=GFED.extract(iris.Constraint(latitude=lambda cell: (-17.0) < cell < (0.0), longitude=lambda cell: (-72) < cell < (-44))) #Brazil/Amazon region

#Get global total burnt area in km2)
#Divide by 1000000000000 to get Mkm2. Divide by 1000000 to get km2
for coord in coords:
    if not GFED.coord(coord).has_bounds():
        GFED.coord(coord).guess_bounds()
grid_weights = iris.analysis.cartography.area_weights(GFED)
GFED = GFED.collapsed(coords, iris.analysis.SUM, weights = grid_weights)/1000000

#Load in satellite data
cube = iris.load_cube('~/PAPERS/AmazonFire/AmazonRegion/TERRA_MT_2001-2016_CB.nc')
coords = ("longitude", "latitude")
time = cube.coord('time')
print cube

cube=cube.extract(iris.Constraint(latitude=lambda cell: (-17.0) < cell < (0.0), longitude=lambda cell: (-72) < cell < (-44))) #Brazil/Amazon region



#Get global total fire counts. 
#Divide by 1000000000000 to get Million. Divide by 10000000000 to get 10000
for coord in coords:
    if not cube.coord(coord).has_bounds():
        cube.coord(coord).guess_bounds()
grid_weights = iris.analysis.cartography.area_weights(cube)
cube = cube.collapsed(coords, iris.analysis.SUM, weights = grid_weights)/10000000000

fig, ax = plt.subplots()

x = np.arange(180)
ax.plot(x,GFED.data, color='black', label='GFED burned area (km$^2$)')
ax.plot(x,cube.data, color='blue', label='MODIS TERRA M-T \n(10000 fire counts)')
#Alternative: scatter plot
#plt.scatter(x,GFED.data, color='blue')
#plt.scatter(x,cube.data, color='black')

plt.xticks(np.arange(0, 192, step=12),("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016"))
plt.title('Fire data comparison, Amazon region')
plt.ylabel('Fire')
plt.xlabel('Years')
plt.legend()
plt.show()


