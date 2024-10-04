#fixing bird layers September, 17th 2024
library(sf)
sf_use_s2(F)
load('r_grid_all_fixed_19_Jan.Rdata')
colnames(r_grid)
load('birds_sf_summ_breeding_resident_fixed.Rdata')
nrow(birds_sf_summ)


birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
nrow(birds_sf_summ)

sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

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

r_grid$wege2_b = wege_function_real_area(sf,res = 50000,r)
r_grid$we2_b = we_function_real_area(sf,res = 50000,r)
r_grid$ge_b = ge_function(sf,res = 50000,r)

save(r_grid,file = 'r_grid_all_fixed_17_Sep_2024.Rdata')

####

#bird_threats----

part1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)



library(raster)
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))

sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


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

write.xlsx(df_number_threatened,file = 'df_number_threatened_b_17_sep_2024.xlsx')

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


save(r_grid,file = 'r_grid_all_fixed_17_Sep_2024.Rdata')

library(dplyr)
library(sf)
sf_use_s2(F)

get_decile_group <- function(x) {
  if(is.na(x) || x == 0) return(NA)
  sum(x > deciles) # This returns the decile group
}

valid_x <- r_grid[!is.na(r_grid$agriculture_b) & r_grid$agriculture_b != 0 & r_grid$land > 0,]$agriculture_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(agriculture_b_d = ifelse(land==1,sapply(agriculture_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$logging_b) & r_grid$logging_b != 0 & r_grid$land > 0,]$logging_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(logging_b_d = ifelse(land==1,sapply(logging_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$hunting_b) & r_grid$hunting_b != 0 & r_grid$land > 0,]$hunting_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(hunting_b_d = ifelse(land==1,sapply(hunting_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$pollution_b) & r_grid$pollution_b != 0 & r_grid$land > 0,]$pollution_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(pollution_b_d = ifelse(land==1,sapply(pollution_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$invasives_b) & r_grid$invasives_b != 0 & r_grid$land > 0,]$invasives_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(invasives_b_d = ifelse(land==1,sapply(invasives_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$climate_change_b) & r_grid$climate_change_b != 0 & r_grid$land > 0,]$climate_change_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(climate_change_b_d = ifelse(land==1,sapply(climate_change_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$urbanization_b) & r_grid$urbanization_b != 0 & r_grid$land > 0,]$urbanization_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(urbanization_b_d = ifelse(land==1,sapply(urbanization_b, get_decile_group),NA))


#birds ----
r_grid$agriculture_b_hot = NA
r_grid[r_grid$agriculture_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$agriculture_b_hot = 1

r_grid$logging_b_hot = NA
r_grid[r_grid$logging_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$logging_b_hot = 1

r_grid$hunting_b_hot = NA
r_grid[r_grid$hunting_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$hunting_b_hot = 1

r_grid$pollution_b_hot = NA
r_grid[r_grid$pollution_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$pollution_b_hot = 1

r_grid$invasives_b_hot = NA
r_grid[r_grid$invasives_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$invasives_b_hot = 1

r_grid$climate_change_b_hot = NA
r_grid[r_grid$climate_change_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$climate_change_b_hot = 1

r_grid$urbanization_b_hot = NA
r_grid[r_grid$urbanization_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$urbanization_b_hot = 1


r_grid$hotspot_any_threat_b = NA
r_grid[r_grid$agriculture_b_hot %in% 1 | r_grid$logging_b_hot %in% 1 
       |r_grid$hunting_b_hot %in% 1 |r_grid$pollution_b_hot %in% 1 
       |r_grid$invasives_b_hot %in% 1 |r_grid$climate_change_b_hot %in% 1 
       |r_grid$urbanization_b_hot %in% 1 ,]$hotspot_any_threat_b = 1



r_grid$hotspots_all = NA
r_grid[r_grid$hotspot_any_threat_a %in% 1 | r_grid$hotspot_any_threat_r %in% 1 
       |r_grid$hotspot_any_threat_m %in% 1 |r_grid$hotspot_any_threat_b %in% 1,]$hotspots_all = 1



save(r_grid,file = 'r_grid_all_fixed_17_Sep_2024.Rdata')

#birds ----
part1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')

library(raster)


sf$range = st_area(sf)/1000000
units(sf$range) = NULL

x = t(sf_intersected)

y = sf

st_geometry(y) = NULL


threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)
sf$spid = 1:nrow(sf)

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

uncertainty = function(i){
  if(length(x[[i]])==0){
    return(NA)
  }else{
    return(sum(y[x[[i]],]$range))
  }
}

uncertainty_med = function(i){
  if(length(x[[i]])==0){
    return(NA)
  }else{
    return(median(y[x[[i]],]$range))
  }
}


r_grid$richness_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$richness_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$richness_b_unc_med)

sf_intersected = st_intersects(sf[ids_hunting,],r_grid)
x = t(sf_intersected)
y = sf[ids_hunting,]
st_geometry(y) = NULL

r_grid$hunting_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$hunting_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$hunting_b_unc_med)


sf_intersected = st_intersects(sf[ids_agriculture,],r_grid)
x = t(sf_intersected)
y = sf[ids_agriculture,]
st_geometry(y) = NULL
r_grid$agriculture_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$agriculture_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))
colnames(r_grid)

sf_intersected = st_intersects(sf[ids_logging,],r_grid)
x = t(sf_intersected)
y = sf[ids_logging,]
st_geometry(y) = NULL
r_grid$logging_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$logging_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

sf_intersected = st_intersects(sf[ids_pollution,],r_grid)
x = t(sf_intersected)
y = sf[ids_pollution,]
r_grid$pollution_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$pollution_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_invasives,],r_grid)
x = t(sf_intersected)
y = sf[ids_invasives,]
st_geometry(y) = NULL
r_grid$invasives_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$invasives_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))



sf_intersected = st_intersects(sf[ids_climate_change,],r_grid)
x = t(sf_intersected)
y = sf[ids_climate_change,]
st_geometry(y) = NULL
r_grid$climate_change_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$climate_change_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_urbanization,],r_grid)
x = t(sf_intersected)
y = sf[ids_urbanization,]
st_geometry(y) = NULL
r_grid$urbanization_b_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$urbanization_b_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

sf_list=st_intersects(sf,r_grid)

r_grid$richness_b =unlist(lapply((t(sf_list)),FUN = length))


save(r_grid,file = 'r_grid_all_fixed_17_Sep_2024.Rdata')

r_grid[r_grid$land ==1 & !r_grid$agriculture_b_hot%in%1,]$agriculture_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$logging_b_hot%in%1,]$logging_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_b_hot%in%1,]$hunting_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_b_hot%in%1,]$pollution_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_b_hot%in%1,]$invasives_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_b_hot%in%1,]$climate_change_b_hot = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_b_hot%in%1,]$urbanization_b_hot = 0
r_grid$b_cum_threats = NA
r_grid$b_cum_threats = r_grid$agriculture_b_hot+r_grid$logging_b_hot+r_grid$hunting_b_hot+r_grid$pollution_b_hot+r_grid$invasives_b_hot+
  r_grid$climate_change_b_hot+r_grid$urbanization_b_hot

save(r_grid,file = 'r_grid_all_fixed_17_Sep_2024.Rdata')







