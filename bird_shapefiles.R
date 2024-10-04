
load('sf1.Rdata')
load('sf2_1.Rdata')
load('sf2_2.Rdata')
load('sf2_3.Rdata')
load('sf2_4.Rdata')
load('sf3.Rdata')
load('sf4.Rdata')
load('sf5.Rdata')

sf_birds = rbind(sf1,sf2_1,sf2_2,sf2_3,sf2_4,sf3,sf4,sf5)
rm(sf1,sf2_1,sf2_2,sf2_3,sf2_4,sf3,sf4,sf5)
duplicated_species = sf_birds[duplicated(sf_birds$sci_name),]$sci_name

duplicated_sf = sf_birds[sf_birds$sci_name %in% duplicated_species,]

library(dplyr)

duplicated_sf = duplicated_sf %>% group_by(sci_name) %>% summarise()

duplicated_sf[duplicated(duplicated_sf$sci_name),]$sci_name

sf_birds_non_suplicated = sf_birds[!sf_birds$sci_name %in% duplicated_species,]

sf_birds = rbind(sf_birds_non_suplicated,duplicated_sf)

length(unique(sf_birds$sci_name)) == nrow(sf_birds)

save(sf_birds_cropped, file = 'sf_birds_summarized_fixed.Rdata')

load('sf_birds_summarized.Rdata')

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()


library(raster)

#e = extent(sf_land)

#sf_birds_cropped = st_crop(sf_birds,extent(sf_land))

bbox_list <- lapply(st_geometry(sf_birds), st_bbox)
maxmin <- as.data.frame(matrix(unlist(bbox_list),nrow=nrow(sf_birds)))
names(maxmin) <- names(bbox_list[[1]])

sf_birds_no_geometry = sf_birds
st_geometry(sf_birds_no_geometry) = NULL
sf_birds_no_geometry = cbind.data.frame(sf_birds_no_geometry,maxmin)

sp_problems = sf_birds_no_geometry[which(sf_birds_no_geometry$xmax > 179.9),]$sci_name
st_bbox(sf_land)
e = extent(c(-179.9,179.9,-90,90))
sf_birds_test = st_crop(sf_birds[sf_birds$sci_name %in% sp_problems,],e)

sf_birds_cropped = st_crop(sf_birds,e)


sf_birds = st_transform(sf_birds_cropped,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

library(fasterize)
test4 = fasterize(sf = st_collection_extract(sf_birds_subset2, "POLYGON"),raster = r_moll,fun = 'sum')
plot(test4)

library(rn)
pdf('test.pdf')
for(i in 1:length(sf_birds_subset2$sci_name)){
  plot(sf_birds_subset2[sf_birds_subset2$sci_name == sf_birds_subset2$sci_name[i],]$geometry,main = sf_birds_subset2$sci_name[i],col = 'orange')
}
dev.off()



r = raster(ext = extent(sf_land), resolution = 0.50000, crs = '+proj=longlat +datum=WGS84 +no_defs')
#sf_birds = st_make_valid(sf_birds)

st_bbox(sf_land)

sf_birds_cropped_moll = st_transform(sf_birds_cropped,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land_moll = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")


r_moll = raster(ext = extent(sf_land_moll), resolution = 50000, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

test3 = fasterize(sf = st_collection_extract(sf_birds_cropped_moll, "POLYGON"),raster = r_moll,fun = 'sum')
plot(test3)



library(fasterize)
sf_birds$sci_name = as.factor(sf_birds$sci_name)



test1 = fasterize(sf = st_collection_extract(sf_birds[sf_birds$sci_name %in% sp_problems,], "POLYGON"),raster = r,fun = 'sum')
plot(test1)

st_bbox(sf_birds)
st_bbox(r)

