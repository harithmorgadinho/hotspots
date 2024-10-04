library(sf)
sf_use_s2(F)
library(raster)
# load packages
library(prioritizrdata)
library(prioritizr)
library(terra)
library(raster)
library(cluster)
library(dplyr)
load('r_grid.Rdata')

setwd('/Users/gdt366/Dropbox/postdoc_KU_paper_2/amphibians_50x50')
tiff_files <- list.files(path = getwd(), pattern = "\\.tiff$", full.names = TRUE)
raster_list <- lapply(tiff_files, rast)
raster_stack <- rast(raster_list)
setwd("/Users/gdt366/Dropbox/postdoc_KU_paper_2")

terra::gdalCache(100000)

grid_land_only = r_grid[r_grid$land ==1,]
grid_land_only$cost = 1
grid_land_only$locked_in = FALSE
grid_land_only$locked_out = FALSE


#amphibians ----

setwd('/Users/gdt366/Dropbox/postdoc_KU_paper_2/amphibians_50x50')
tiff_files <- list.files(path = getwd(), pattern = "\\.tiff$", full.names = TRUE)
raster_list <- lapply(tiff_files, rast)
raster_stack <- rast(raster_list)
setwd("/Users/gdt366/Dropbox/postdoc_KU_paper_2")

load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(amphibians_sf_summ)


names(raster_stack) = sf$binomial
rm(sf)


p2 <-
  problem(grid_land_only, raster_stack,cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_loglinear_targets(10, 0.9, 100, 0.2) %>% 
  add_highs_solver



s2 <- solve(p2)
hist(s2$solution_1)

irrep_s1 <- eval_ferrier_importance(p2, s2["solution_1"])
irrep_s1$plot_total <- irrep_s1$total

save(irrep_s1, file = 'irrep_s1_amphibians.Rdata')

#reptiles ----


setwd('/Users/gdt366/Dropbox/postdoc_KU_paper_2/reptiles_50x50')
tiff_files <- list.files(path = getwd(), pattern = "\\.tiff$", full.names = TRUE)
raster_list <- lapply(tiff_files, rast)
raster_stack <- rast(raster_list)
setwd("/Users/gdt366/Dropbox/postdoc_KU_paper_2")

terra::gdalCache(100000)

load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(reptiles_sf_summ)


names(raster_stack) = sf$binomial
rm(sf)



p2 <-
  problem(grid_land_only, raster_stack,cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_loglinear_targets(10, 0.9, 100, 0.2) %>% 
  add_highs_solver


s2 <- solve(p2)
hist(s2$solution_1)

irrep_s1 <- eval_ferrier_importance(p2, s2["solution_1"])
irrep_s1$plot_total <- irrep_s1$total

save(irrep_s1, file = 'irrep_s1_reptiles.Rdata')

#birds ----

setwd('/Users/gdt366/Dropbox/postdoc_KU_paper_2/birds_50x50')
tiff_files <- list.files(path = getwd(), pattern = "\\.tiff$", full.names = TRUE)
raster_list <- lapply(tiff_files, rast)
raster_stack <- rast(raster_list)
setwd("/Users/gdt366/Dropbox/postdoc_KU_paper_2")

terra::gdalCache(100000)

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))
sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)

names(raster_stack) = sf$binomial
rm(sf)

p2 <-
  problem(grid_land_only, raster_stack,cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_loglinear_targets(10, 0.9, 100, 0.2) %>% 
  add_highs_solver


s2 <- solve(p2)
hist(s2$solution_1)

irrep_s1 <- eval_ferrier_importance(p2, s2["solution_1"])
irrep_s1$plot_total <- irrep_s1$total

save(irrep_s1, file = 'irrep_s1_birds.Rdata')


