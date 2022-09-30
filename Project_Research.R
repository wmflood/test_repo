library(raster)
library(rgdal)
library(caret)
library(dplyr)
library(tidyverse)

Band4 = raster("LC08_09032022_P218R015_B4.TIF")
Band6 = raster("LC08_09032022_P218R015_B6.TIF")

##Calculating the Normalized Difference Snow Index.
##As indicated in the path names Band 4 is subtracted from Band 6
ndsi = (Band4 - Band6)/(Band4 + Band6)

##Creating an area extent (65 03 30 N, 19 24 16 W) and (64 33 00 N, 18 20 10 W)
e = extent(575070.72, 627699.10, 7161016.68, 7215903.58) 

##Cropping the landsat image to the extent created
ndsi = crop(ndsi, e)
plot(ndsi, col = rev(terrain.colors(10)), main = 'Landsat-NDSI')

##Converting the raster to a vector/matrix
nr = getValues(ndsi)
str(nr)

##Kmeans classification
#Start by setting the seed of the kmeans
set.seed(99)

#For this we are following along with the example. 
#Will need to look into the most optimal and why for this setup for the actual
kmncluster <- kmeans(na.omit(nr), 
                     centers = 10, 
                     iter.max = 500, 
                     nstart = 5, 
                     algorithm="Lloyd")

str(kmncluster)
#should return an object with the class kmeans

##Should look into doing this with random forest as well

##Need to set the kmeans to a raster now
knr <- setValues(ndsi, kmncluster$cluster)
knr

##Plotting out the ndsi --> there is a lot to be desired with the plot
#Should look into doing this plot in ggplot with raster
#Should change up the color pallet to make the image more intuitive 
#Should also look into the terrain.colors to see what is offered
mycolor <- c("#fef65b","#ff0000", "#daa520","#0000ff","#0000ff","#00ff00",
             "#cbbeb5", "#c3ff5b", "#ff7373", "#00ff00", "#808080")
par(mfrow = c(1,2))
plot(ndsi, col = rev(terrain.colors(10)), main = 'Landsat-NDSI')
plot(knr, main = 'Unsupervised classification', col = mycolor )

##Also need to find Iceland terrain classification map to run a confusion matrix against
#Found Iceland land classification data from the CORINE EU land classification
#The two main steps I need to take with this data are as follows:
#X) Need to convert to UTM and meters to match the raster projection
#2) Need to clip down to the study area using e
#3) Need to convert the shapefile into a raster

library(sf)
library(lwgeom)

#Convert to utm and meters for units
iceland_clc = st_read("CLC18.shp")
IS_utm = iceland_clc %>% 
  st_transform_proj(crs = "+proj=utm +zone=27 +datum=WGS84 +units=m +no_defs")
st_crs(IS_utm)

#Clip down to the study area
IS_area = st_crop(IS_utm, e)
ggplot(IS_area, aes(fill=CODE_18), color=NA) + 
  geom_sf()

#Converting the shapefile to raster
IS_area_sp = as(IS_area, "Spatial") #need to convert to sp object first
IS_clc_raster = raster(IS_area_sp, crs = crs(ndsi), )
