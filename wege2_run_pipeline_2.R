#wege2

load('r_grid.Rdata')

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
  
  return(getValues(r_ge))
}

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

sf_land = st_transform(sf_land,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

r = raster(st_zm(sf_land),resolution = 50000,
           crs = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
nrow(amphibians_sf_summ)

sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


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
  if(nrow(input[input$area<20,])>0){
    input[input$area<20,]$area = 20
  }
  
  
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
  
  return(getValues(r_wege))
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
  if(nrow(input[input$area<20,])>0){
    input[input$area<20,]$area = 20
  }
  
  
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
  
  return(getValues(r_we))
}

r_grid$wege2_a = wege_function_real_area(sf,res = 50000,r)
r_grid$we2_a = we_function_real_area(sf,res = 50000,r)
r_grid$ge_a = ge_function(sf,res = 50000,r)

rm(amphibians_sf_summ)

load('r_grid_metrics_run2.Rdata')

load('reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
unique(reptiles_sf_summ$category)
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
nrow(reptiles_sf_summ)

sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf = sf[order(sf$binomial),]
rgrid = st_as_sf(rasterToPolygons(r))
st_crs(sf) = st_crs(rgrid)
sps_grid <- sf::st_intersects(sf, rgrid)

wege_function_real_area = function(sf,res = 50000,r,sps_grid){
  

  
  length(sps_grid)
  
  sf$area = st_area(sf)/1000000
  units(sf$area) = NULL
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  if(nrow(input[input$area<20,])>0){
    input[input$area<20,]$area = 20
  }
  
  
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
  
  return(getValues(r_wege))
}
we_function_real_area = function(sf,res = 50000,r,sps_grid){
  
  
  
  length(sps_grid)
  
  sf$area = st_area(sf)/1000000
  units(sf$area) = NULL
  
  er_df=as.data.frame(t(cbind.data.frame(DD = 0.0513, LC = 9e-04, NT = 0.0071, VU = 0.0513, EN = 0.4276, CR = 0.9688, EW = 1, EX = 1)))
  colnames(er_df) = 'ER'
  er_df$category = rownames(er_df)
  input=merge(x=sf,y=er_df,by.x='category',by.y='category')
  input = input[order(input$binomial),]
  if(nrow(input[input$area<20,])>0){
    input[input$area<20,]$area = 20
  }
  
  
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
  
  return(getValues(r_we))
}
ge_function = function(sf,res = 50000,r,sps_grid){
  
  
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
  
  return(getValues(r_ge))
}

r_grid$wege2_r = wege_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$we2_r = we_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$ge_r = ge_function(sf,res = 50000,r,sps_grid)

rm(reptiles_sf_summ)

save(r_grid,file = 'r_grid_metrics_run2.Rdata')



load('/Users/gdt366/Dropbox/disaster_project/mammals_sf_summ_all_sps_present.Rdata')
unique(mammals_sf_summ$category)
mammals_sf_summ = mammals_sf_summ[mammals_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
nrow(mammals_sf_summ)

sf = st_transform(mammals_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf = sf[order(sf$binomial),]
rgrid = st_as_sf(rasterToPolygons(r))
st_crs(sf) = st_crs(rgrid)
sps_grid <- sf::st_intersects(sf, rgrid)

r_grid$wege2_m = wege_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$we2_m = we_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$ge_m = ge_function(sf,res = 50000,r,sps_grid)


rm(mammals_sf_summ)


save(r_grid,file = 'r_grid_metrics_run2.Rdata')




load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')

birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
nrow(brids_sf_summ)
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))

sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)

sf = sf[order(sf$binomial),]
rgrid = st_as_sf(rasterToPolygons(r))
st_crs(sf) = st_crs(rgrid)
sps_grid <- sf::st_intersects(sf, rgrid)

r_grid$wege2_b = wege_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$we2_b = we_function_real_area(sf,res = 50000,r,sps_grid)
r_grid$ge_b = ge_function(sf,res = 50000,r,sps_grid)

p = ggplot()+geom_sf(data = r_grid[r_grid$wege2_r>0,],aes(col = wege2_r,fill = wege2_r))

ggsave(p,filename = 'test.png')



save(r_grid,file = 'r_grid_metrics_run2.Rdata')


load('irrep_s1_amphibians.Rdata')

cor(r_grid[r_grid$land==1,]$wege2_a,irrep_s1$plot_total)

r_grid$ferrier_a = NA

r_grid[r_grid$land==1,]$ferrier_a = irrep_s1$plot_total

cor(r_grid[r_grid$land==1,]$wege2_a,r_grid[r_grid$land==1,]$ferrier_a)

p = ggplot(data = r_grid[r_grid$land==1,],aes(x = wege2_a,y = ferrier_a))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 2))+
  geom_smooth(method = 'lm',formula = y ~ x)

ggsave(filename = 'ferrier_wege_amphibians.png',plot = p)

y = r_grid[r_grid$land==1,]$ferrier_a
x = r_grid[r_grid$land==1,]$wege2_a
test1 = lm(y ~ poly(x, 2))
summary(test1)

y = r_grid[r_grid$land==1,]$ferrier_a
x = r_grid[r_grid$land==1,]$wege2_a
test2 = lm(y ~ x)
summary(test2)

load('irrep_s1_reptiles.Rdata')

r_grid$ferrier_r = NA
r_grid[r_grid$land==1,]$ferrier_r = irrep_s1$plot_total
cor(r_grid[r_grid$land==1,]$wege2_r,r_grid[r_grid$land==1,]$ferrier_r)

p = ggplot(data = r_grid[r_grid$land==1,],aes(x = wege2_r,y = ferrier_r))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 2))+
  geom_smooth(method = 'lm',formula = y ~ x)

ggsave(filename = 'ferrier_wege_reptiles.png',plot = p)

y = r_grid[r_grid$land==1,]$ferrier_r
x = r_grid[r_grid$land==1,]$wege2_r
test1 = lm(y ~ poly(x, 2))
summary(test1)

test2 = lm(y ~ x)
summary(test2)

load('irrep_s1_mammals.Rdata')

r_grid$ferrier_m = NA
r_grid[r_grid$land==1,]$ferrier_m = irrep_s1$plot_total
cor(r_grid[r_grid$land==1,]$wege2_m,r_grid[r_grid$land==1,]$ferrier_m)

p = ggplot(data = r_grid[r_grid$land==1,],aes(x = wege2_m,y = ferrier_m))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 2))+
  geom_smooth(method = 'lm',formula = y ~ x)


ggsave(filename = 'ferrier_wege_mammals.png',plot = p)

y = r_grid[r_grid$land==1,]$ferrier_m
x = r_grid[r_grid$land==1,]$wege2_m
test1 = lm(y ~ poly(x, 2))
summary(test1)

test2 = lm(y ~ x)
summary(test2)


load('irrep_s1_birds.Rdata')

r_grid$ferrier_b = NA
r_grid[r_grid$land==1,]$ferrier_b = irrep_s1$plot_total
cor(r_grid[r_grid$land==1,]$wege2_b,r_grid[r_grid$land==1,]$ferrier_b)

p = ggplot(data = r_grid[r_grid$land==1,],aes(x = wege2_b,y = ferrier_b))+
  geom_point()+
  geom_smooth(method = 'lm',formula = y ~ poly(x, 2))+
  geom_smooth(method = 'lm',formula = y ~ x)


ggsave(filename = 'ferrier_wege_birds.png',plot = p)

save(r_grid,file = 'r_grid_metrics_run2.Rdata')







