#pop_analysis

#build master raster file
library(raster)


library(rnaturalearth)
library(dplyr)
library(ggplot2)
africa = ne_countries(continent = 'Africa',scale = 10,returnclass = 'sf')
newproj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

library(sf)
sf::sf_use_s2(FALSE)
library(raster)
library(fasterize)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(gridExtra)
library(rmapshaper)

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()

library(terra)
library(rgdal)
library(maptools)
library(raster)
r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)
r_b = rast(r)
#2010
newproj = "+proj=moll +lon_0=0 +x_0=0 +y_0=0"
pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2010_SSP1_RCP2_6.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

library(sf)
sf_use_s2(F)
pr2 <- projectRaster(pop_density, crs=mollCRS)
pr2 <- terra::project(rast(pop_density), y=mollCRS)
pr2_test = raster(pr2)
sum(na.omit(pr2_test[]))


sum(na.omit(pop_density[]))

sum(na.omit(x[]))

mollCRS <- CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0")
#behrmannCRS <- CRS('+proj=cea +lat_ts=30')

#pop_density2 <- projectRaster(pop_density1, crs=behrmannCRS, over=T,res = 50000)

## SpatRaster
x = pop_density
rasters_to_moll = function(x,funct='sum'){
  x <- aggregate(x,fact = 50,fun=sum)
  x = rast(x)
  x <- terra::project(x, res = 50000,align = T,y = mollCRS,method = funct)
  x = raster(x)
  x[is.na(x)]=0
  return(resample(x,r))
}

pop_density2011 = rasters_to_moll(pop_density)
#2030
pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP1_RCP2_6.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP1 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP2_RCP4_5.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP2 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP3_RCP7.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP3 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP4_RCP6.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP4_RCP6 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP4_RCP3_4.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP4_RCP3_4 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2030_SSP5_RCP8_5.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2030_SSP5 = rasters_to_moll(pop_density)


#2050
pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP1_RCP2_6.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP1 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP2_RCP4_5.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP2 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP3_RCP7.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP3 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP4_RCP6.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP4_RCP6 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP4_RCP3_4.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP4_RCP3_4 = rasters_to_moll(pop_density)


pop_density = raster('/Users/gdt366/Dropbox/Copenhagen_postdoc/CMPI6wp_grid_pop_count2050_SSP5_RCP8_5.tif')
crs(pop_density) = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
pop_density2050_SSP5 = rasters_to_moll(pop_density)



raster_all = stack(pop_density2011,
                   pop_density2030_SSP1,pop_density2030_SSP2,pop_density2030_SSP3,pop_density2030_SSP4_RCP6,pop_density2030_SSP4_RCP3_4,pop_density2030_SSP5,
                   pop_density2050_SSP1,pop_density2050_SSP2,pop_density2050_SSP3,pop_density2050_SSP4_RCP6,pop_density2050_SSP4_RCP3_4,pop_density2050_SSP5)

names(raster_all)=c('pop_density2011',
                    'pop_density2030_SSP1','pop_density2030_SSP2','pop_density2030_SSP3','pop_density2030_SSP4_RCP6','pop_density2030_SSP4_RCP3_4','pop_density2030_SSP5',
                    'pop_density2050_SSP1','pop_density2050_SSP2','pop_density2050_SSP3','pop_density2050_SSP4_RCP6','pop_density2050_SSP4_RCP3_4','pop_density2050_SSP5')


save(raster_all,file= 'raster_pop_world_moll.Rdata')


