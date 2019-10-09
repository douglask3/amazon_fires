### Amazon region ###

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import iris.analysis.cartography
import iris
import iris.quickplot as qplt
import iris.plot as iplt
from scipy import stats


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

cube=cube.extract(iris.Constraint(latitude=lambda cell: (-17.0) < cell < (0.0), longitude=lambda cell: (-72) < cell < (-44))) #Brazil/Amazon region


#Get global total fire counts. 
#Divide by 1000000000000 to get Million. Divide by 10000000000 to get 10000
for coord in coords:
    if not cube.coord(coord).has_bounds():
        cube.coord(coord).guess_bounds()
grid_weights = iris.analysis.cartography.area_weights(cube)
cube = cube.collapsed(coords, iris.analysis.SUM, weights = grid_weights)/10000000000

#Calculate linear least squares regression
#https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.linregress.html
x = cube.data
y = GFED.data
slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
rsq = r_value**2
print "r-squared:", rsq

fig = plt.figure()
ax = plt.plot()
plt.plot(x, y, 'o', label='Fire Data')
plt.plot(x, intercept + slope*x, 'r', label='Regression line')
plt.text(300000,110000,'r$^2$= '+str("%.4f" % rsq), color='red')
plt.xlabel('Fire Counts')
plt.ylabel('GFED Burned Area')
plt.title('Fire data relationship')
plt.legend()
plt.show()


