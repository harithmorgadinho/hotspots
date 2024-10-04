#script uncertainty

#uncertainty_map
#threat_layers
library(sf)
sf::sf_use_s2(FALSE)
library(raster)
library(fasterize)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(gridExtra)
library(rmapshaper)


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


hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")

#amphibians ----

load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sf_list=st_intersects(sf,r_grid)

all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/threats.csv',stringsAsFactors = F)

sf$range = st_area(sf)/1000000
units(sf$range) = NULL

x = t(sf_list)

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





r_grid$richness_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$richness_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$richness_a_unc_med)

sf_intersected = st_intersects(sf[ids_hunting,],r_grid)
x = t(sf_intersected)
y = sf[ids_hunting,]
st_geometry(y) = NULL

r_grid$hunting_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$hunting_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$hunting_a_unc_med)


sf_intersected = st_intersects(sf[ids_agriculture,],r_grid)
x = t(sf_intersected)
y = sf[ids_agriculture,]
st_geometry(y) = NULL
r_grid$agriculture_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$agriculture_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))
colnames(r_grid)

sf_intersected = st_intersects(sf[ids_logging,],r_grid)
x = t(sf_intersected)
y = sf[ids_logging,]
st_geometry(y) = NULL
r_grid$logging_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$logging_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

sf_intersected = st_intersects(sf[ids_pollution,],r_grid)
x = t(sf_intersected)
y = sf[ids_pollution,]
r_grid$pollution_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$pollution_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_invasives,],r_grid)
x = t(sf_intersected)
y = sf[ids_invasives,]
st_geometry(y) = NULL
r_grid$invasives_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$invasives_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))



sf_intersected = st_intersects(sf[ids_climate_change,],r_grid)
x = t(sf_intersected)
y = sf[ids_climate_change,]
st_geometry(y) = NULL
r_grid$climate_change_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$climate_change_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_urbanization,],r_grid)
x = t(sf_intersected)
y = sf[ids_urbanization,]
st_geometry(y) = NULL
r_grid$urbanization_a_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$urbanization_a_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))




#reptiles ----

part1 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

load('reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(reptiles_sf_summ)


sf$range = st_area(sf)/1000000
units(sf$range) = NULL

x = t(sf_list)

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





r_grid$richness_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$richness_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$richness_r_unc_med)

sf_intersected = st_intersects(sf[ids_hunting,],r_grid)
x = t(sf_intersected)
y = sf[ids_hunting,]
st_geometry(y) = NULL

r_grid$hunting_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$hunting_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$hunting_r_unc_med)


sf_intersected = st_intersects(sf[ids_agriculture,],r_grid)
x = t(sf_intersected)
y = sf[ids_agriculture,]
st_geometry(y) = NULL
r_grid$agriculture_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$agriculture_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))
colnames(r_grid)

sf_intersected = st_intersects(sf[ids_logging,],r_grid)
x = t(sf_intersected)
y = sf[ids_logging,]
st_geometry(y) = NULL
r_grid$logging_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$logging_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

sf_intersected = st_intersects(sf[ids_pollution,],r_grid)
x = t(sf_intersected)
y = sf[ids_pollution,]
r_grid$pollution_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$pollution_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_invasives,],r_grid)
x = t(sf_intersected)
y = sf[ids_invasives,]
st_geometry(y) = NULL
r_grid$invasives_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$invasives_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))



sf_intersected = st_intersects(sf[ids_climate_change,],r_grid)
x = t(sf_intersected)
y = sf[ids_climate_change,]
st_geometry(y) = NULL
r_grid$climate_change_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$climate_change_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_urbanization,],r_grid)
x = t(sf_intersected)
y = sf[ids_urbanization,]
st_geometry(y) = NULL
r_grid$urbanization_r_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$urbanization_r_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


#mammals ----

all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/mammal_threats/threats.csv',stringsAsFactors = F)

load('/Users/gdt366/Dropbox/disaster_project/mammals_sf_summ_all_sps_present.Rdata')
mammals_sf_summ = mammals_sf_summ[mammals_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(mammals_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


sf$range = st_area(sf)/1000000
units(sf$range) = NULL

x = t(sf_list)

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





r_grid$richness_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$richness_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$richness_m_unc_med)

sf_intersected = st_intersects(sf[ids_hunting,],r_grid)
x = t(sf_intersected)
y = sf[ids_hunting,]
st_geometry(y) = NULL

r_grid$hunting_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$hunting_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

hist(r_grid$hunting_m_unc_med)


sf_intersected = st_intersects(sf[ids_agriculture,],r_grid)
x = t(sf_intersected)
y = sf[ids_agriculture,]
st_geometry(y) = NULL
r_grid$agriculture_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$agriculture_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))
colnames(r_grid)

sf_intersected = st_intersects(sf[ids_logging,],r_grid)
x = t(sf_intersected)
y = sf[ids_logging,]
st_geometry(y) = NULL
r_grid$logging_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$logging_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

sf_intersected = st_intersects(sf[ids_pollution,],r_grid)
x = t(sf_intersected)
y = sf[ids_pollution,]
r_grid$pollution_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$pollution_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_invasives,],r_grid)
x = t(sf_intersected)
y = sf[ids_invasives,]
st_geometry(y) = NULL
r_grid$invasives_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$invasives_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))



sf_intersected = st_intersects(sf[ids_climate_change,],r_grid)
x = t(sf_intersected)
y = sf[ids_climate_change,]
st_geometry(y) = NULL
r_grid$climate_change_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$climate_change_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))


sf_intersected = st_intersects(sf[ids_urbanization,],r_grid)
x = t(sf_intersected)
y = sf[ids_urbanization,]
st_geometry(y) = NULL
r_grid$urbanization_m_unc_sum = unlist(lapply(X = 1:length(x),FUN = uncertainty))
r_grid$urbanization_m_unc_med = unlist(lapply(X = 1:length(x),FUN = uncertainty_med))

#birds ----
part1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')

library(raster)
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))
sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)

sf$range = st_area(sf)/1000000
units(sf$range) = NULL

x = t(sf_list)

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


#old----

save(r_grid,file = 'r_grid_metrics_run2_deciles.Rdata')


library(RColorBrewer)
display.brewer.all()

pal = colorRampPalette(brewer.pal(6,'BrBG'))
pal = colorRampPalette(brewer.pal(11,'PRGn'))
pal = colorRampPalette(c(brewer.pal(4,'RdYlBu'),rev(brewer.pal(4,'BrBG'))))

pal = colorRampPalette(c('#ffffbf','#d73027'))

sf_land_intersected = st_intersects(sf_land,r_grid)
land_areas = unlist(lapply(t(sf_land_intersected), FUN = length))

r_grid = r_grid_uncertainty_layers
num_quantiles <- 10 # For quartiles
r_grid$land_areas = land_areas






ggplot()+
  geom_sf(data = r_grid,aes(col = as.factor(quantile_group)))+
  scale_color_manual(values = rev(pal(10)))

library(gridExtra)
library(png)
library(grid)

img <- readPNG('cat.png', native = TRUE)
cat_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('saw.png', native = TRUE)
saw_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('factory.png', native = TRUE)
factory_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('wolf-icon-png-2858.png', native = TRUE)
hunting_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('agriculture.png', native = TRUE)
agriculture_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('climate_change.png', native = TRUE)
climate_c_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('urbanization2.png', native = TRUE)
urbanization_img <- rasterGrob(img, interpolate=TRUE)

legend.title.size = 10
annotate.size = 3
size_legend_text = 7
legend_height = 0.5



r_grid[r_grid$uncertainty_r_hunting_med==0,]$uncertainty_r_hunting_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_hunting_med), num_quantiles))
r_grid$quantile_group_hun = df_temp$quantile_group



p1 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_hun),col = as.factor(quantile_group_hun)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'e')+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)

r_grid[r_grid$uncertainty_r_agriculture_med==0,]$uncertainty_r_agriculture_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_agriculture_med), num_quantiles))
r_grid$quantile_group_agr = df_temp$quantile_group

p2 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_agr),col = as.factor(quantile_group_agr)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'c')+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

r_grid[r_grid$uncertainty_r_logging_med==0,]$uncertainty_r_logging_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_logging_med), num_quantiles))
r_grid$quantile_group_log = df_temp$quantile_group

p3 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_log),col = as.factor(quantile_group_log)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


r_grid[r_grid$uncertainty_r_pollution_med==0,]$uncertainty_r_pollution_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_pollution_med), num_quantiles))
r_grid$quantile_group_pol = df_temp$quantile_group

p4 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_pol),col = as.factor(quantile_group_pol)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'b')+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

r_grid[r_grid$uncertainty_r_invasives_med==0,]$uncertainty_r_invasives_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_invasives_med), num_quantiles))
r_grid$quantile_group_inv = df_temp$quantile_group

p5 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_inv),col = as.factor(quantile_group_inv)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'd')+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

r_grid[r_grid$uncertainty_r_climate_change_med==0,]$uncertainty_r_climate_change_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_climate_change_med), num_quantiles))
r_grid$quantile_group_clc = df_temp$quantile_group

p6 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_clc),col = as.factor(quantile_group_clc)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15082470,y = -7053445,label = 'Climate Change',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        legend.background = element_rect(fill = "transparent",colour = "transparent"),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

r_grid[r_grid$uncertainty_r_urbanization_med==0,]$uncertainty_r_urbanization_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_r_urbanization_med), num_quantiles))
r_grid$quantile_group_urb = df_temp$quantile_group

p7 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_urb),col = as.factor(quantile_group_urb)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

r_grid[r_grid$uncertainty_layer_reptiles_med==0,]$uncertainty_layer_reptiles_med = NA
df_temp <- r_grid %>% 
  mutate(quantile_group = ntile(log10(uncertainty_layer_reptiles_med), num_quantiles))
r_grid$quantile_group_rich = df_temp$quantile_group

p8 = ggplot() +
  geom_sf(data = r_grid[r_grid$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid,aes(fill = as.factor(quantile_group_rich),col = as.factor(quantile_group_rich)),linewidth = 0.0001) +
  scale_fill_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  scale_colour_manual('Impact uncertainty',values = rev(pal(10)),na.value = NA,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Richness',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(legend_height, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_legend(title.position = "right",reverse = T))+
  guides(col = guide_legend(title.position = "right",reverse = T))+
  labs(tag = 'h')



figSM2_grid = arrangeGrob(p3,p4,p2,p5,p1,p6,p7,p8,ncol=2)

ggsave(file = 'figure_uncertainty_Aug_9.png',plot = figSM2_grid,width = 10,height = 9,dpi = 2000)






colnames(r_grid_uncertainty_layers)
list_groups = c('_a','_r','_m','_b')
list_threats = c('_logging','_agriculture','_hunting','_pollution','_invasives','_climate_change','_urbanization')
list_plots1 = c(paste0('uncertainty',list_groups[1],list_threats),"uncertainty_layer_amphibians")
list_plots2 = c(paste0('uncertainty',list_groups[2],list_threats),"uncertainty_layer_reptiles")
list_plots3 = c(paste0('uncertainty',list_groups[3],list_threats),"uncertainty_layer_mammals")
list_plots4 = c(paste0('uncertainty',list_groups[4],list_threats),"uncertainty_layer_birds")

list_plots = list(list_plots1,list_plots2,list_plots3,list_plots4)

r_grid_plotting = r_grid

for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  p1 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  
  p2 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  
  p3 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  
  p4 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  
  p5 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  
  p6 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  
  p7 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  
  p8 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all','.pdf'),width = 10,height = 8,plot = p_all)
  
}


list_plots1 = c(paste0('uncertainty',list_groups[1],list_threats,'_med'),"uncertainty_layer_amphibians_med")
list_plots2 = c(paste0('uncertainty',list_groups[2],list_threats,'_med'),"uncertainty_layer_reptiles_med")
list_plots3 = c(paste0('uncertainty',list_groups[3],list_threats,'_med'),"uncertainty_layer_mammals_med")
list_plots4 = c(paste0('uncertainty',list_groups[4],list_threats,'_med'),"uncertainty_layer_birds_med")

list_plots = list(list_plots1,list_plots2,list_plots3,list_plots4)

r_grid_plotting = r_grid

for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  p1 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  
  p2 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  
  p3 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  
  p4 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  
  p5 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  
  p6 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  
  p7 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  
  p8 = ggplot()+geom_sf(data = r_grid_plotting,aes(col = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = terrain.colors(100),limits = c(1,max(na.omit(r_grid_plotting$plotting))),na.value = 'grey90')
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all_med','.pdf'),width = 10,height = 8,plot = p_all)
  
}

#>50%
col_pal = colorRampPalette(c('steelblue','khaki','indianred'))
for(i in seq_along(list_plots)){
  print(i)
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[1]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  #min_value = min(na.omit(r_grid_plotting[r_grid_plotting$plotting>0,]$plotting))
  p1 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[1]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[1]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[2]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p2 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[2]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[2]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[3]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p3 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[3]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[3]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[4]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p4 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[4]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[4]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[5]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p5 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[5]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[5]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[6]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p6 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[6]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[6]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[7]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p7 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[7]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[7]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  r_grid_plotting$plotting = r_grid_plotting[[list_plots[[i]][[8]]]]
  r_grid_plotting$plotting = r_grid_plotting$plotting/max(na.omit(r_grid_plotting$plotting))
  
  p8 = ggplot()+
    geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90',fill = 'grey90')+
    geom_sf(data = r_grid_plotting,aes(col = plotting,fill = plotting))+
    scale_color_gradientn(list_plots[[i]][[8]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    scale_fill_gradientn(list_plots[[i]][[8]],colours = col_pal(100),limits = c(0.5,max(na.omit(r_grid_plotting$plotting))),na.value = 'transparent')+
    theme_void()
  
  p_all = arrangeGrob(p1,p2,p3,p4,p5,p6,p7,p8,ncol=2)
  
  ggsave(filename = paste0(list_groups[i],'p_all_med_over_05','.png'),width = 10,height = 8,plot = p_all)
  
}

