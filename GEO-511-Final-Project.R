---
title: "How landscape habitat influences wild eastern turkey populations within New York State"
author: Marissa Zimmer 
output:
    html_document

---

# Introduction

New York State’s (NYS) wild turkey population constitutes a significant part of conservation concerns and support for the hunting community within NYS. In the early 1950s, the Department of Conservation  (NYSDEC) helped restore and repopulate NYS wild turkey populations. The NYSDEC had also partnered with the National Wild Turkey Federation and SUNY Environmental Science and Forestry college in 2010 to research the impact of weather patterns and hunting zones on turkey populations. The results of this study have not been published at this time.

The aim of this project is to research, gather and compare data that involves understanding and explaining how land cover, tree canopy cover, elevation and precipitation can affect wild turkey population levels. Within NYS many individuals have noticed seeing less wild turkeys than in years past and changing habitat factors in localized areas may be one factor affecting population levels. By looking at these environmantal variables the aim of this project is to see if areas with less suitable environmental variables correlate to areas of lower population levels. If the results of this study come back to support this hypothesis then there is room for continued research into habitat suitability within NYS for wild turkey populations. 



# Materials and methods

For this project it was important to highlight the key environmental variables that affect wild turkey populations within New York State. The first environmental variable that was used was landcover from the U.S. Geological Survey (USGS). Specifically, the landcover data is a global digital elevation model (DEM) with a horizontal grid spacing of 30 arc seconds (approximately 1 kilometer). The USGS seperated the land cover data into tiles on a world map which can be selected by grid. For this project I was able to select a grid tile that included NY. The next environmental variable that was chosen for this project was Tree canopy cover from the Global Land Analysis & Discovery. The global tree cover data are per pixel estimates of the 2010 percent maximum (peak growing season) tree canopy cover from cloud-free annual growing season composite Landset 7 ETM + data. A median from the annual tree canopy cover cover values from 2009-2011 were used to estimate the 2010 tree cover. Like the Landcover data the tree cover canopy data was chosen from a grid style map with a spatila resoultion of 1 arc-second per pixel, approximately 30 m per pixel. The next environmental variable that was selected was elevation. 


*Elevation-Dr. Williams 

*Precipitation-PRISM 


##Data

#Part 1: Required Packages 
```{r, message=F, warning=F}
library(raster)
library(sf)
library(tigris)
library(rasterVis)
library(rgdal)
library(dismo)
library(ggplot2)

# cache the results for quick compiling
options(tigris_use_cache = TRUE)
 
```

#Wild Turkey Occurance and Environmental variables used 
```{r}

ebird_data <-read.csv(file = "C:/Users/maris/OneDrive/Desktop/maxent_turkey3.csv")

#Landcover from Dr. Williams 
land_cover = raster ("C:/USers/maris/OneDrive/Desktop/Environmental Factor for Turkey Project/landcover_crop.asc")
crs(land_cover)=4326

#Treecover from Global Forest Watch
tree_cover = raster("C:/USers/maris/OneDrive/Desktop/Environmental Factor for Turkey Project/Hansen_GFC-2019-v1.7_treecover2000_50N_080W (1).tif")
tree_cover

#Precipitation data from PRISM 
precip_cover = raster("C:/USers/maris/OneDrive/Desktop/Environmental Factor for Turkey Project/PRISM_ppt_stable_4kmM3_2020_asc.asc")
crs(precip_cover)=4326

#Elevation data from Dr. Williams 
elevation_cover = raster ("C:/Users/maris/OneDrive/Desktop/Environmental Factor for Turkey Project/elevation.tif")
elevation_cover

#population data from Dr. Williams 
pop_cover= raster ("C:/Users/maris/OneDrive/Desktop/Environmental Factor for Turkey Project/population.asc")
crs(pop_cover)=4326
pop_cover


#Getting all environmental factors on the same resolution, extent: lat/long, crs:4326

ny_counties <-counties(state = "NY", cb = FALSE,
                       resolution = "500k", year = 2020) %>%
                        st_union() %>%
                         st_transform(crs=projection(land_cover))
#landcover 

land_coverny= crop(land_cover, as(ny_counties, "Spatial"))

#treecover

tree_coverny = crop(tree_cover, 
                    as(st_transform(ny_counties,st_crs(tree_cover)), "Spatial"))#%>%

tree_coverny2 = projectRaster(tree_coverny, to=land_coverny,method="ngb")

#elevation 
elevation_coverny= crop(elevation_cover,
                   as(st_transform(ny_counties,st_crs(elevation_cover)), "Spatial"))#%>%

elevation_coverny2 = projectRaster(elevation_coverny,to=land_coverny, method = "ngb")

#precipitation
precip_coverny = crop(precip_cover, 
                      as(st_transform(ny_counties,st_crs(precip_cover)), "Spatial"))#%>%

precip_coverny2 = projectRaster(precip_cover, to=land_coverny,method="ngb")



#masking, stacking, creating my map.

extent_1 = extent(land_coverny)
precip_cover = crop(precip_cover, extent_1)
tree_coverny2 = crop(tree_coverny2, extent_1)
elevation_coverny2 = crop(elevation_coverny2, extent_1)




stack_1= stack(land_coverny,elevation_coverny2,tree_coverny2, precip_coverny2)%>%
  mask(mask=as(ny_counties, "Spatial"))
plot(stack_1)

# Dismo Package 

#variables needed to make a prediction map

all_data <-bioclim(stack_1, ebird_data)
stack_1= stack(land_coverny,elevation_coverny2,tree_coverny2, precip_coverny2)%>%
  mask(mask=as(ny_counties, "Spatial"))
e <-extent(-79.7625, -71.77917, 40.475, 45.01667)

# Plotting the Prediction 
data_predict <-predict(all_data, stack_1, progess='text', ext=e)
plot(data_predict)

#using gplot 

gplot(data_predict) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white', high = 'red') +
  coord_equal()


```

# Results


Tables and figures (maps and other graphics) are carefully planned to convey the results of your analysis. Intense exploration and evidence of many trials and failures. The author looked at the data in many different ways before coming to the final presentation of the data.


# Conclusions

[~200 words]

Clear summary adequately describing the results and putting them in context. Discussion of further questions and ways to continue investigation.

# References

“Global Forest Change 2000–2020data Download.” Global Forest Change, https://storage.googleapis.com/earthenginepartners-hansen/GFC-2020-v1.8/download.html. 

PRISM Climate Group, Oregon State U. (n.d.). Prism.oregonstate.edu. https://prism.oregonstate.edu/recent/monthly.php


