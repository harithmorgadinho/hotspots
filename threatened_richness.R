
library(sf)
library(terra)
library(raster)
sf_use_s2(F)
load('r_grid.Rdata')


load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('VU','EN','CR'),]
sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf_list=st_intersects(sf,r_grid)

r_grid$richness_t_a =unlist(lapply((t(sf_list)),FUN = length))

load('reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
unique(reptiles_sf_summ$category)
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf_list=st_intersects(sf,r_grid)

r_grid$richness_t_r =unlist(lapply((t(sf_list)),FUN = length))





load('/Users/gdt366/Dropbox/disaster_project/mammals_sf_summ_all_sps_present.Rdata')
unique(mammals_sf_summ$category)
mammals_sf_summ = mammals_sf_summ[mammals_sf_summ$category %in% c('VU','EN','CR'),]
sf = st_transform(mammals_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf_list=st_intersects(sf,r_grid)

r_grid$richness_t_m =unlist(lapply((t(sf_list)),FUN = length))

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))
sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)


sf_list=st_intersects(sf,r_grid)

r_grid$richness_t_b =unlist(lapply((t(sf_list)),FUN = length))


colnames(r_grid)

save(r_grid,file = 'r_grid_threatened_richness.Rdata')

hist(r_grid$richness_t_a)
hist(r_grid$richness_t_b)
hist(r_grid$richness_t_m)
hist(r_grid$richness_t_r)










