library(sf)
sf_use_s2(F)
library(rnaturalearth)
sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

plot(sf_land$geometry)


sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()


library(raster)

r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_amphibians_summarized.Rdata')
sf_amphibians = st_transform(sf_amphibians,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

sf_amphibians = sf_amphibians[sf_amphibians$category != 'EX',]
sf_amphibians = sf_amphibians[sf_amphibians$category != 'EW',]

amphibians_intersected = st_intersects(sf_amphibians,r_grid)
amphibian_richness = unlist(lapply(t(amphibians_intersected), FUN = length))
r_grid$richness_a = amphibian_richness



unique(sf_amphibians$category)

sf = sf_amphibians

library(raster)

wege_function = function(sf,res = 50000,r){
  sf = sf[order(sf$binomial),]
  rgrid = st_as_sf(rasterToPolygons(r))
  st_crs(sf) = st_crs(rgrid)
  sps_grid <- sf::st_intersects(sf, rgrid)
  
  length(sps_grid)
  
  sf$area = unlist(lapply(1:length(sps_grid), function(x) length(sps_grid[[x]])))
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  
  
  input$wege_parcial = unlist(lapply(1:nrow(input), function(x) sqrt(1/input$area[[x]]) *  input$ER[[x]]))
  
 
  input2 = input 
  st_geometry(input2) = NULL
  
    r2 = r
    r2[]= 0
    temp_r = r2
    for(i in 1:nrow(sps_grid)){
    print(i)
    temp_r[sps_grid[[i]]] = input2[i,]$wege_parcial
    r2 = sum(r2,temp_r)
    temp_r[] = 0
    }
    
    r_wege <- r2
  
  return(r_wege)
}
we_function = function(sf,res = 50000,r){
  
  sf = sf[order(sf$binomial),]
  rgrid = st_as_sf(rasterToPolygons(r))
  st_crs(sf) = st_crs(rgrid)
  sps_grid <- sf::st_intersects(sf, rgrid)
  
  length(sps_grid)
  
  sf$area = unlist(lapply(1:length(sps_grid), function(x) length(sps_grid[[x]])))
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  
  
  input$we_parcial = unlist(lapply(1:nrow(input), function(x) (1/input$area[[x]])))
  
  
  input2 = input 
  st_geometry(input2) = NULL
  
  r2 = r
  r2[]= 0
  temp_r = r2
  for(i in 1:nrow(sps_grid)){
    print(i)
    temp_r[sps_grid[[i]]] = input2[i,]$we_parcial
    r2 = sum(r2,temp_r)
    temp_r[] = 0
  }
  
  r_we <- r2
  
  return(r_we)
}
ge_function = function(sf,res = 50000,r){
  
  sf = sf[order(sf$binomial),]
  rgrid = st_as_sf(rasterToPolygons(r))
  st_crs(sf) = st_crs(rgrid)
  sps_grid <- sf::st_intersects(sf, rgrid)
  
  length(sps_grid)
  
  sf$area = unlist(lapply(1:length(sps_grid), function(x) length(sps_grid[[x]])))
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  
  
  input$ge_parcial = unlist(lapply(1:nrow(input), function(x) (input$ER[[x]])))
  
  
  input2 = input 
  st_geometry(input2) = NULL
  
  r2 = r
  r2[]= 0
  temp_r = r2
  for(i in 1:nrow(sps_grid)){
    print(i)
    temp_r[sps_grid[[i]]] = input2[i,]$ge_parcial
    r2 = sum(r2,temp_r)
    temp_r[] = 0
  }
  
  r_ge <- r2
  
  return(r_ge)
}

wege_function_real_area = function(sf,res = 50000,r){
  
  sf = sf[order(sf$binomial),]
  rgrid = st_as_sf(rasterToPolygons(r))
  st_crs(sf) = st_crs(rgrid)
  sps_grid <- sf::st_intersects(sf, rgrid)
  
  length(sps_grid)
  
  sf$area = st_area(sf)/1000000
  units(sf$area) = NULL
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  #input[input$area<20,]$area = 20
  
  
  input$wege_parcial = unlist(lapply(1:nrow(input), function(x) sqrt(1/input$area[[x]]) *  input$ER[[x]]))
  
  
  input2 = input 
  st_geometry(input2) = NULL
  
  r2 = r
  r2[]= 0
  temp_r = r2
  for(i in 1:nrow(sps_grid)){
    print(i)
    temp_r[sps_grid[[i]]] = input2[i,]$wege_parcial
    r2 = sum(r2,temp_r)
    temp_r[] = 0
  }
  
  r_wege <- r2
  
  return(r_wege)
}
we_function_real_area = function(sf,res = 50000,r){
  
  sf = sf[order(sf$binomial),]
  rgrid = st_as_sf(rasterToPolygons(r))
  st_crs(sf) = st_crs(rgrid)
  sps_grid <- sf::st_intersects(sf, rgrid)
  
  length(sps_grid)
  
  sf$area = st_area(sf)/1000000
  units(sf$area) = NULL
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  input[input$area<20,]$area = 20
  
  
  input$we_parcial = unlist(lapply(1:nrow(input), function(x) (1/input$area[[x]])))
  
  
  input2 = input 
  st_geometry(input2) = NULL
  
  r2 = r
  r2[]= 0
  temp_r = r2
  for(i in 1:nrow(sps_grid)){
    print(i)
    temp_r[sps_grid[[i]]] = input2[i,]$we_parcial
    r2 = sum(r2,temp_r)
    temp_r[] = 0
  }
  
  r_we <- r2
  
  return(r_we)
}


raster_wege_amphibians = wege_function(sf = sf_amphibians,r = r)
plot(raster_wege_amphibians)
raster_we_amphibians = we_function(sf = sf_amphibians,r = r)
plot(raster_we_amphibians)
raster_ge_amphibians = ge_function(sf = sf_amphibians,r = r)
plot(raster_ge_amphibians)

temp_layer = st_as_sf(rasterToPolygons(raster_wege_amphibians))
r_grid$wege_a = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_we_amphibians))
r_grid$we_a = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_ge_amphibians))
r_grid$ge_a = temp_layer$layer 

raster_wege2_amphibians = wege_function_real_area(sf = sf_amphibians,r = r)
plot(raster_wege2_amphibians)
raster_we2_amphibians = we_function_real_area(sf = sf_amphibians,r = r)
plot(raster_we_amphibians)

temp_layer = st_as_sf(rasterToPolygons(raster_wege2_amphibians))
r_grid$wege2_a = temp_layer$layer 
temp_layer = st_as_sf(rasterToPolygons(raster_we2_amphibians))
r_grid$we2_a = temp_layer$layer 


rm(sf_amphibians)
rm(birds_intersected2)

load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_reptiles_summarized.Rdata')
sf_reptiles = st_transform(sf_reptiles,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



unique(sf_amphibians$category)
sf_reptiles = sf_reptiles[sf_reptiles$category != 'EX',]
sf_reptiles = sf_reptiles[sf_reptiles$category != 'EW',]
sf_reptiles$category = gsub(sf_reptiles$category,pattern = 'LR/cd',replacement = 'LC')

reptiles_intersected = st_intersects(sf_reptiles,r_grid)
reptile_richness = unlist(lapply(t(reptiles_intersected), FUN = length))
r_grid$richness_r = reptile_richness


raster_wege_reptiles = wege_function(sf = sf_reptiles,r = r)
raster_we_reptiles = we_function(sf = sf_reptiles,r = r)
raster_ge_reptiles = ge_function(sf = sf_reptiles,r = r)
plot(raster_wege2_reptiles)

#test = raster_wege_reptiles
#test[is.na(test)] = 1000
#plot(test)

temp_layer = st_as_sf(rasterToPolygons(raster_wege_reptiles))
r_grid$wege_r = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_we_reptiles))
r_grid$we_r = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_ge_reptiles))
r_grid$ge_r = temp_layer$layer 

unique(sf_reptiles$category)
raster_wege2_reptiles = wege_function_real_area(sf = sf_reptiles,r = r)
plot(raster_wege_reptiles)
raster_we2_reptiles = we_function_real_area(sf = sf_reptiles,r = r)
plot(log10(raster_wege2_reptiles))
plot(raster_we2_reptiles)




temp_layer = st_as_sf(rasterToPolygons(raster_wege2_reptiles))
r_grid$wege2_r = temp_layer$layer 
temp_layer = st_as_sf(rasterToPolygons(raster_we2_reptiles))
r_grid$we2_r = temp_layer$layer 

plot(australia$geometry)
plot(sf_reptiles[sf_reptiles$binomial == 'Oligosoma awakopaka',]$geometry,add=T,col = 'orange')
plot(sf_reptiles[sf_reptiles$binomial == 'Oligosoma awakopaka',]$geometry,col = 'orange')

grep(x = input$binomial,pattern = 'Oligosoma awakopaka')

plot(raster_wege_reptiles)


rm(sf_reptiles)
load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_mammals_summarized.Rdata')
sf_mammals = st_transform(sf_mammals,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

sf_mammals = sf_mammals[sf_mammals$category != 'EX',]
sf_mammals = sf_mammals[sf_mammals$category != 'EW',]

mammals_intersected = st_intersects(sf_mammals,r_grid)
mammal_richness = unlist(lapply(t(mammals_intersected), FUN = length))
r_grid$richness_m = mammal_richness

unique(sf_mammals$category)


raster_wege_mammals = wege_function(sf = sf_mammals,r = r)
raster_we_mammals = we_function(sf = sf_mammals,r = r)
raster_ge_mammals = ge_function(sf = sf_mammals,r = r)

temp_layer = st_as_sf(rasterToPolygons(raster_wege_mammals))
r_grid$wege_m = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_we_mammals))
r_grid$we_m = temp_layer$layer 

temp_layer = st_as_sf(rasterToPolygons(raster_ge_mammals))
r_grid$ge_m = temp_layer$layer 


raster_wege2_mammals = wege_function_real_area(sf = sf_mammals,r = r)
plot(raster_wege2_mammals)
raster_we2_mammals = we_function_real_area(sf = sf_mammals,r = r)
plot(raster_we2_mammals)

temp_layer = st_as_sf(rasterToPolygons(raster_wege2_mammals))
r_grid$wege2_m = temp_layer$layer 
temp_layer = st_as_sf(rasterToPolygons(raster_we2_mammals))
r_grid$we2_m = temp_layer$layer 

plot(raster_wege_mammals)

rm(sf_birds)
load('sf_birds_summarized_fixed.Rdata')

sf_birds = st_transform(sf_birds,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

bird_cat1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/assessments.csv')
bird_cat2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/assessments.csv')
bird_cat = rbind(bird_cat1,bird_cat2)
colnames(bird_cat)
colnames(sf_birds)


sf_birds = merge(sf_birds,bird_cat[,c(3,4)],by.x = 'sci_name',by.y = 'scientificName')
unique(sf_birds$redlistCategory)
sf_birds = sf_birds[sf_birds$redlistCategory != 'Extinct in the Wild',]
sf_birds = sf_birds[sf_birds$redlistCategory != 'Extinct',]

sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Least Concern',
                                replacement = 'LC')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Near Threatened',
                                replacement = 'NT')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Vulnerable',
                                replacement = 'VU')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Endangered',
                                replacement = 'EN')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Critically EN',
                                replacement = 'CR')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Data Deficient',
                                replacement = 'DD')

unique(sf_birds$redlistCategory)


colnames(sf_birds)[1:2] = c('binomial','category')


library(fasterize)
test1 = fasterize(sf = st_collection_extract(sf_birds, "POLYGON"),raster = r,fun = 'sum')
plot(test1)

raster_wege_birds1 = wege_function(sf = sf_birds[1:5000,],r = r)
plot(raster_wege_birds1)
raster_wege_birds2 = wege_function(sf = sf_birds[5001:nrow(sf_birds),],r = r)
plot(raster_wege_birds2)
raster_wege_birds = sum(raster_wege_birds1,raster_wege_birds2)
plot(raster_wege_birds)

r_grid = r
r_grid[] = 0
r_grid = st_as_sf(rasterToPolygons(r_grid))

load('/Users/gdt366/Dropbox/african_snakes/raster_land.Rdata')



sf_land_intersected = st_intersects(sf_land,r_grid)
land_areas = unlist(lapply(t(sf_land_intersected), FUN = length))
r_grid$land_areas = land_areas

birds_intersected = st_intersects(sf_birds[1:5000,],r_grid)
birds_intersected2 = st_intersects(sf_birds[5001:nrow(sf_birds),],r_grid)


bird_richness1 = unlist(lapply(t(birds_intersected), FUN = length))
bird_richness2 = unlist(lapply(t(birds_intersected2), FUN = length))
bird_richness = bird_richness1+bird_richness2

r_grid$richness_b = bird_richness
temp_layer = st_as_sf(rasterToPolygons(raster_wege_birds))
r_grid$wege_b = temp_layer$layer

save(r_grid,file = 'grid_paper_3.Rdata')


raster_we_birds1 = we_function(sf = sf_birds[1:5000,],r = r)
plot(raster_wege_birds1)
raster_we_birds2 = we_function(sf = sf_birds[5001:nrow(sf_birds),],r = r)
plot(raster_wege_birds2)
raster_we_birds = sum(raster_we_birds1,raster_we_birds2)
plot(raster_we_birds)


temp_layer = st_as_sf(rasterToPolygons(raster_we_birds))
r_grid$we_b = temp_layer$layer

raster_ge_birds1 = ge_function(sf = sf_birds[1:5000,],r = r)
plot(raster_ge_birds1)
raster_ge_birds2 = ge_function(sf = sf_birds[5001:nrow(sf_birds),],r = r)
plot(raster_ge_birds2)
raster_ge_birds = sum(raster_ge_birds1,raster_ge_birds2)
plot(raster_ge_birds)

temp_layer = st_as_sf(rasterToPolygons(raster_ge_birds))
r_grid$ge_b = temp_layer$layer



raster_we2_birds1 = we_function_real_area(sf = sf_birds[1:5000,],r = r)
plot(raster_wege_birds1)
raster_we2_birds2 = we_function_real_area(sf = sf_birds[5001:nrow(sf_birds),],r = r)
plot(raster_wege_birds2)
raster_we2_birds = sum(raster_we2_birds1,raster_we2_birds2)
plot(raster_we2_birds)


raster_wege2_birds1 = wege_function_real_area(sf = sf_birds[1:5000,],r = r)
plot(raster_wege_birds1)
raster_wege2_birds2 = wege_function_real_area(sf = sf_birds[5001:nrow(sf_birds),],r = r)
plot(raster_wege_birds2)
raster_wege2_birds = sum(raster_wege2_birds1,raster_wege2_birds2)
plot(raster_wege2_birds)

temp_layer = st_as_sf(rasterToPolygons(raster_we2_birds))
r_grid$we2_b = temp_layer$layer
temp_layer = st_as_sf(rasterToPolygons(raster_wege2_birds))
r_grid$wege2_b = temp_layer$layer

save(r_grid,file = 'grid_paper_3.Rdata')


