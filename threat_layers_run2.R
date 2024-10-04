#threat_layers
library(sf)
sf_use_s2(F)


sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')

sf_land = st_transform(sf_land,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

r = raster(st_zm(sf_land),resolution = 50000,
           crs = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)


#amphibian_threats----
all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/threats.csv',stringsAsFactors = F)
load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')



hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")


sf_intersected = st_intersects(sf,r_grid)



df_number_threatened = cbind.data.frame(logging = length(sf[sf$binomial %in% all_threats[all_threats$code %in% logging,]$scientificName,]$binomial),
                                        agriculture = length(sf[sf$binomial %in% all_threats[all_threats$code %in% agriculture,]$scientificName,]$binomial),
                                        pollution = length(sf[sf$binomial %in% all_threats[all_threats$code %in% pollution,]$scientificName,]$binomial),
                                        invasive = length(sf[sf$binomial %in% all_threats[all_threats$code %in% invasive,]$scientificName,]$binomial),
                                        hunting = length(sf[sf$binomial %in% all_threats[all_threats$code %in% hunting,]$scientificName,]$binomial),
                                        climate_change = length(sf[sf$binomial %in% all_threats[all_threats$code %in% climate_change,]$scientificName,]$binomial),
                                        urbanization = length(sf[sf$binomial %in% all_threats[all_threats$code %in% urbanization,]$scientificName,]$binomial),
                                        all = length(sf[sf$binomial %in% all_threats[all_threats$code %in% c(logging,agriculture,pollution,invasive,hunting,climate_change,urbanization),]$scientificName,]$binomial))

df_number_threatened=as.data.frame(t(df_number_threatened))

df_number_threatened$percentage = round(100*df_number_threatened$V1/nrow(sf),1)

library(openxlsx)

write.xlsx(df_number_threatened,file = 'df_number_threatened_a.xlsx')

#write.csv(df_number_threatened,'df_number_threatened.csv')

threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)


sf$spid = 1:nrow(sf)

sf$range_size = st_area(sf)
units(sf$range_size) = NULL

sf_weight = 1.0/(sf$range_size^(1/3))

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

ids_threatened = unique(sf[sf$category %in% c('CR','EN','VU'),]$spid)

#x = t(sf_reptiles_intersected)[[100000]]
#x = t(sf_reptiles_intersected)[100000:100010]

test = unlist(x[12303])

weight_layer = function(x,y){
  if(length(x) == 0){
    value = NA
  }else{
    list_zeros = rep(NA,length(sf_intersected))
    list_zeros[test] = 0
    
    if(length(intersect(x,y))==0){
      value = 0
    }else{
    list_zeros[intersect(x,y)] = 1
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
    }
    
  }
  return(value)
}

x = t(sf_intersected)




length(ids_hunting)
length(ids_agriculture)
length(ids_logging)
length(ids_pollution)
length(ids_invasives)
length(ids_climate_change)
length(ids_urbanization)
length(ids_threatened)

time0=Sys.time()
y = ids_hunting

probability_hunting <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_agriculture
probability_agriculture <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_logging
probability_logging <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_pollution
probability_pollution <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_invasives
probability_invasives <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_climate_change
probability_climate_change <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_urbanization
probability_urbanization <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

ids_any_threat = sort(unique(c(ids_hunting,ids_agriculture,ids_logging,ids_pollution,ids_invasives,ids_climate_change,ids_urbanization)))
y = ids_any_threat
probability_any_threat <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_threatened
probability_threatened <- unlist(lapply(x, FUN = weight_layer,y = y))



r_grid$hunting_a = probability_hunting
r_grid$agriculture_a = probability_agriculture
r_grid$logging_a = probability_logging
r_grid$pollution_a = probability_pollution
r_grid$invasives_a = probability_invasives
r_grid$climate_change_a = probability_climate_change
r_grid$urbanization_a = probability_urbanization
r_grid$threatened_a = probability_threatened
r_grid$any_threat_a = probability_any_threat

hist(r_grid$hunting_a)
save(r_grid,file = 'r_grid_metrics_run2.Rdata')

#reptile_threats----
part1 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

load('reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(reptiles_sf_summ)

hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")


sf_intersected = st_intersects(sf,r_grid)



df_number_threatened = cbind.data.frame(logging = length(sf[sf$binomial %in% all_threats[all_threats$code %in% logging,]$scientificName,]$binomial),
                                        agriculture = length(sf[sf$binomial %in% all_threats[all_threats$code %in% agriculture,]$scientificName,]$binomial),
                                        pollution = length(sf[sf$binomial %in% all_threats[all_threats$code %in% pollution,]$scientificName,]$binomial),
                                        invasive = length(sf[sf$binomial %in% all_threats[all_threats$code %in% invasive,]$scientificName,]$binomial),
                                        hunting = length(sf[sf$binomial %in% all_threats[all_threats$code %in% hunting,]$scientificName,]$binomial),
                                        climate_change = length(sf[sf$binomial %in% all_threats[all_threats$code %in% climate_change,]$scientificName,]$binomial),
                                        urbanization = length(sf[sf$binomial %in% all_threats[all_threats$code %in% urbanization,]$scientificName,]$binomial),
                                        all = length(sf[sf$binomial %in% all_threats[all_threats$code %in% c(logging,agriculture,pollution,invasive,hunting,climate_change,urbanization),]$scientificName,]$binomial))

df_number_threatened=as.data.frame(t(df_number_threatened))

df_number_threatened$percentage = round(100*df_number_threatened$V1/nrow(sf),1)

library(openxlsx)

write.xlsx(df_number_threatened,file = 'df_number_threatened_r.xlsx')

#write.csv(df_number_threatened,'df_number_threatened.csv')

threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)


sf$spid = 1:nrow(sf)

sf$range_size = st_area(sf)
units(sf$range_size) = NULL

sf_weight = 1.0/(sf$range_size^(1/3))

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

ids_threatened = unique(sf[sf$category %in% c('CR','EN','VU'),]$spid)

#x = t(sf_reptiles_intersected)[[100000]]
#x = t(sf_reptiles_intersected)[100000:100010]

weight_layer = function(x,y){
  if(length(x) == 0){
    value = NA
  }else{
    list_zeros = rep(NA,length(sf_intersected))
    list_zeros[x] = 0
    list_zeros[intersect(x,y)] = 1
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

x = t(sf_intersected)

length(ids_hunting)
length(ids_agriculture)
length(ids_logging)
length(ids_pollution)
length(ids_invasives)
length(ids_climate_change)
length(ids_urbanization)
length(ids_threatened)

time0=Sys.time()
y = ids_hunting
probability_hunting <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_agriculture
probability_agriculture <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_logging
probability_logging <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_pollution
probability_pollution <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_invasives
probability_invasives <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_climate_change
probability_climate_change <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_urbanization
probability_urbanization <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

ids_any_threat = sort(unique(c(ids_hunting,ids_agriculture,ids_logging,ids_pollution,ids_invasives,ids_climate_change,ids_urbanization)))
y = ids_any_threat
probability_any_threat <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_threatened
probability_threatened <- unlist(lapply(x, FUN = weight_layer,y = y))


load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/r_grid_metrics_run2_deciles_wallace_nov28.Rdata')


r_grid$hunting_r = probability_hunting
r_grid$agriculture_r = probability_agriculture
r_grid$logging_r = probability_logging
r_grid$pollution_r = probability_pollution
r_grid$invasives_r = probability_invasives
r_grid$climate_change_r = probability_climate_change
r_grid$urbanization_r = probability_urbanization
r_grid$threatened_r = probability_threatened
r_grid$any_threat_r = probability_any_threat

hist(r_grid$hunting_r)




save(r_grid,file = 'r_grid_metrics_run2.Rdata')

#mammal_threats----

load('r_grid_metrics_run2.Rdata')
all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/mammal_threats/threats.csv',stringsAsFactors = F)

load('/Users/gdt366/Dropbox/disaster_project/mammals_sf_summ_all_sps_present.Rdata')
mammals_sf_summ
unique(mammals_sf_summ$category)
mammals_sf_summ = mammals_sf_summ[mammals_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(mammals_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")


sf_intersected = st_intersects(sf,r_grid)



df_number_threatened = cbind.data.frame(logging = length(sf[sf$binomial %in% all_threats[all_threats$code %in% logging,]$scientificName,]$binomial),
                                        agriculture = length(sf[sf$binomial %in% all_threats[all_threats$code %in% agriculture,]$scientificName,]$binomial),
                                        pollution = length(sf[sf$binomial %in% all_threats[all_threats$code %in% pollution,]$scientificName,]$binomial),
                                        invasive = length(sf[sf$binomial %in% all_threats[all_threats$code %in% invasive,]$scientificName,]$binomial),
                                        hunting = length(sf[sf$binomial %in% all_threats[all_threats$code %in% hunting,]$scientificName,]$binomial),
                                        climate_change = length(sf[sf$binomial %in% all_threats[all_threats$code %in% climate_change,]$scientificName,]$binomial),
                                        urbanization = length(sf[sf$binomial %in% all_threats[all_threats$code %in% urbanization,]$scientificName,]$binomial),
                                        all = length(sf[sf$binomial %in% all_threats[all_threats$code %in% c(logging,agriculture,pollution,invasive,hunting,climate_change,urbanization),]$scientificName,]$binomial))

df_number_threatened=as.data.frame(t(df_number_threatened))

df_number_threatened$percentage = round(100*df_number_threatened$V1/nrow(sf),1)

library(openxlsx)

write.xlsx(df_number_threatened,file = 'df_number_threatened_m.xlsx')

#write.csv(df_number_threatened,'df_number_threatened.csv')

threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)


sf$spid = 1:nrow(sf)

sf$range_size = st_area(sf)
units(sf$range_size) = NULL

sf_weight = 1.0/(sf$range_size^(1/3))

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

ids_threatened = unique(sf[sf$category %in% c('CR','EN','VU'),]$spid)

#x = t(sf_reptiles_intersected)[[100000]]
#x = t(sf_reptiles_intersected)[100000:100010]

weight_layer = function(x,y){
  if(length(x) == 0){
    value = NA
  }else{
    list_zeros = rep(NA,length(sf_intersected))
    list_zeros[x] = 0
    list_zeros[intersect(x,y)] = 1
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

x = t(sf_intersected)

length(ids_hunting)
length(ids_agriculture)
length(ids_logging)
length(ids_pollution)
length(ids_invasives)
length(ids_climate_change)
length(ids_urbanization)
length(ids_threatened)

time0=Sys.time()
y = ids_hunting


probability_hunting <- unlist(lapply(x, FUN = weight_layer,y = y))


y = ids_agriculture
probability_agriculture <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_logging
probability_logging <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_pollution
probability_pollution <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_invasives
probability_invasives <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_climate_change
probability_climate_change <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_urbanization
probability_urbanization <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

ids_any_threat = sort(unique(c(ids_hunting,ids_agriculture,ids_logging,ids_pollution,ids_invasives,ids_climate_change,ids_urbanization)))
y = ids_any_threat
probability_any_threat <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_threatened
probability_threatened <- unlist(lapply(x, FUN = weight_layer,y = y))


r_grid$hunting_m = probability_hunting
r_grid$agriculture_m = probability_agriculture
r_grid$logging_m = probability_logging
r_grid$pollution_m = probability_pollution
r_grid$invasives_m = probability_invasives
r_grid$climate_change_m = probability_climate_change
r_grid$urbanization_m = probability_urbanization
r_grid$threatened_m = probability_threatened
r_grid$any_threat_m = probability_any_threat

hist(r_grid$hunting_m)


save(r_grid,file = 'r_grid_metrics_run2.Rdata')

#bird_threats----

part1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')

unique(birds_sf_summ$category)


library(raster)
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))
sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)

hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")

colnames(sf)

sf_intersected = st_intersects(sf,r_grid)

df_number_threatened = cbind.data.frame(logging = length(sf[sf$binomial %in% all_threats[all_threats$code %in% logging,]$scientificName,]$binomial),
                                        agriculture = length(sf[sf$binomial %in% all_threats[all_threats$code %in% agriculture,]$scientificName,]$binomial),
                                        pollution = length(sf[sf$binomial %in% all_threats[all_threats$code %in% pollution,]$scientificName,]$binomial),
                                        invasive = length(sf[sf$binomial %in% all_threats[all_threats$code %in% invasive,]$scientificName,]$binomial),
                                        hunting = length(sf[sf$binomial %in% all_threats[all_threats$code %in% hunting,]$scientificName,]$binomial),
                                        climate_change = length(sf[sf$binomial %in% all_threats[all_threats$code %in% climate_change,]$scientificName,]$binomial),
                                        urbanization = length(sf[sf$binomial %in% all_threats[all_threats$code %in% urbanization,]$scientificName,]$binomial),
                                        all = length(sf[sf$binomial %in% all_threats[all_threats$code %in% c(logging,agriculture,pollution,invasive,hunting,climate_change,urbanization),]$scientificName,]$binomial))

df_number_threatened=as.data.frame(t(df_number_threatened))

df_number_threatened$percentage = round(100*df_number_threatened$V1/nrow(sf),1)

library(openxlsx)

write.xlsx(df_number_threatened,file = 'df_number_threatened_b.xlsx')

#write.csv(df_number_threatened,'df_number_threatened.csv')

threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)

sf$spid = 1:nrow(sf)

sf$range_size = st_area(sf)
units(sf$range_size) = NULL

sf_weight = 1.0/(sf$range_size^(1/3))

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

ids_threatened = unique(sf[sf$category %in% c('CR','EN','VU'),]$spid)


weight_layer = function(x,y){
  if(length(x) == 0){
    value = NA
  }else{
    list_zeros = rep(NA,length(sf_intersected))
    list_zeros[x] = 0
    list_zeros[intersect(x,y)] = 1
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

x = t(sf_intersected)

richness_test = unlist(lapply(x,length))


hist(richness_test)
r_grid_new$richness_test = richness_test

p1 = ggplot()+
  geom_sf(data = r_grid_new,aes(fill = richness_test,col = richness_test),linewidth = 0)+
  scale_fill_gradientn(colours = rainbow(6))+
  scale_colour_gradientn(colours = rainbow(6))+
  theme_void()
ggsave('p1.png')


length(ids_hunting)
length(ids_agriculture)
length(ids_logging)
length(ids_pollution)
length(ids_invasives)
length(ids_climate_change)
length(ids_urbanization)
length(ids_threatened)

time0=Sys.time()
y = ids_hunting
probability_hunting <- unlist(lapply(x, FUN = weight_layer,y = y))
length(probability_hunting)

y = ids_agriculture
probability_agriculture <- unlist(lapply(x, FUN = weight_layer,y = y))


y = ids_logging
probability_logging <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_pollution
probability_pollution <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_invasives
probability_invasives <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_climate_change
probability_climate_change <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_urbanization
probability_urbanization <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

ids_any_threat = sort(unique(c(ids_hunting,ids_agriculture,ids_logging,ids_pollution,ids_invasives,ids_climate_change,ids_urbanization)))
y = ids_any_threat
probability_any_threat <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_threatened
probability_threatened <- unlist(lapply(x, FUN = weight_layer,y = y))



load('r_grid_metrics_run2_deciles_wallace_nov28.Rdata')



r_grid$hunting_b = probability_hunting
r_grid$agriculture_b = probability_agriculture
r_grid$logging_b = probability_logging
r_grid$pollution_b = probability_pollution
r_grid$invasives_b = probability_invasives
r_grid$climate_change_b = probability_climate_change
r_grid$urbanization_b = probability_urbanization
r_grid$threatened_b = probability_threatened
r_grid$any_threat_b = probability_any_threat

hist(r_grid$hunting_b)

save(r_grid,file = 'r_grid_metrics_run2_deciles_wallace_dec12.Rdata')

save(r_grid,file = 'r_grid_metrics_run2.Rdata')



hist(r_grid$hunting_r_unc_med)

hist(r_grid$hunting_a_unc_med)


r_grid[is.na(r_grid$hunting_r_unc_med),]$hunting_r_unc_med = 0
threshold = mean(r_grid$hunting_r_unc_med) +2 * sd(r_grid$hunting_r_unc_med)
threshold = mean(r_grid$hunting_r_unc_med) + (2 * sd(r_grid$hunting_r_unc_med))

p1 = ggplot()+
  geom_sf(data = r_grid,aes(fill = hunting_r_unc_med,col = hunting_r_unc_med),linewidth = 0)+
  theme_void()

p2 = ggplot()+
  geom_sf(data = r_grid[r_grid$hunting_r_unc_med>threshold,],aes(fill = hunting_r_unc_med,col = hunting_r_unc_med),linewidth = 0)+
  theme_void()



r_grid$hunting_r_unc_med_log = log10(r_grid$hunting_r_unc_med)

r_grid[is.nan(r_grid$hunting_r_unc_med_log),]$hunting_r_unc_med_log = 0

r_grid_temp = r_grid[r_grid$hunting_r_unc_med_log>0,]


threshold = mean(r_grid_temp$hunting_r_unc_med_log) + (1 * sd(r_grid_temp$hunting_r_unc_med_log))

r_grid_temp$hunting_r_unc_med_log

p3 = ggplot()+
  geom_sf(data = r_grid_temp[r_grid_temp$hunting_r_unc_med_log>threshold,],aes(fill = hunting_r_unc_med_log,col = hunting_r_unc_med_log),linewidth = 0)+
  theme_void()


grid = arrangeGrob(p1,p2,p3,ncol = 1)

ggsave(plot =grid, 'grid.png')

















