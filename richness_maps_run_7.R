#plotting----
load('birds_breeding_resident_filtered_fixed.Rdata')
load('mammals_filtered.Rdata')
load('reptiles_filtered.Rdata')
load('amphibians_filtered.Rdata')
library(raster)
library(rnaturalearth)
library(sf)
sf_use_s2(F)
library(terra)
nrow(birds_breeding_resident)+nrow(mammals)+nrow(reptiles)+nrow(amphibians)
sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
library(raster)
r = raster(extent(sf_land),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
background_r_grid = st_as_sf(rasterToPolygons(r))
background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



#load('sf_land_sum.Rdata')

sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r = raster(extent(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)




world_map = ne_countries(scale = 10,returnclass = 'sf')

r = raster(extent(world_map),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)

r_grid = st_as_sf(rasterToPolygons(r))
r_grid$layer = 1:nrow(r_grid)
r_grid = st_transform(r_grid,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


amphibians_grid = st_intersects(amphibians,r_grid)
reptiles_grid = st_intersects(reptiles,r_grid)
mammals_grid = st_intersects(mammals,r_grid)
#birds_grid = st_intersects(birds,r_grid)
birds_breeding_grid = st_intersects(birds_breeding_resident,r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))
r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))
r_grid$richness_m = unlist(lapply(t(mammals_grid),length))
r_grid$richness_b = unlist(lapply(t(birds_breeding_grid),length))




load('amphibians_filtered.Rdata')
nrow(amphibians)

load('reptiles_filtered.Rdata')
nrow(reptiles)

load('mammals_filtered.Rdata')
nrow(mammals)

load('birds_breeding_resident_filtered_fixed.Rdata')
birds_breeding = birds_breeding_resident

load('r_grid_vulcanos_earthquakes_tsunamis_hurricanes_run6.Rdata')

library(openxlsx)

amphibians_list = read.xlsx('amphibians_grid2_all_disastersv2_run7.xlsx')
colnames(amphibians_list)
v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0.245 & amphibians_list$total_perc_afected_vul > 24.5 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial
e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0.245 & amphibians_list$total_perc_afected_ear > 24.5 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0.245 & amphibians_list$total_perc_afected_tsu > 24.5 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial

a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

reptiles_list = read.xlsx('reptiles_grid2_all_disasters_run7_APRL_17.xlsx')

v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0.245 & reptiles_list$total_perc_afected_vul > 24.5 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial
e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0.245 & reptiles_list$total_perc_afected_ear > 24.5 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial
t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0.245 & reptiles_list$total_perc_afected_tsu > 24.5 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial
h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0.245 & reptiles_list$total_perc_afected_hur > 24.5 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial

r_list = c(v_r_list,e_r_list,t_r_list,h_r_list)

mammals_list = read.xlsx('mammals_grid2_all_disasters_run7_APRL_17.xlsx')

v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0.245 & mammals_list$total_perc_afected_vul > 24.5 & !is.na(mammals_list$total_perc_afected_vul),]$binomial
e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0.245 & mammals_list$total_perc_afected_ear > 24.5 & !is.na(mammals_list$total_perc_afected_ear),]$binomial
t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0.245 & mammals_list$total_perc_afected_tsu > 24.5 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial
h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0.245 & mammals_list$total_perc_afected_hur > 24.5 & !is.na(mammals_list$total_perc_afected_hur),]$binomial

m_list = c(v_m_list,e_m_list,t_m_list,h_m_list)

birds_list = read.xlsx('birds_breeding_grid2_all_disasters_run7_APRL_17.xlsx')

v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0.245 & birds_list$total_perc_afected_vul > 24.5 & !is.na(birds_list$total_perc_afected_vul),]$binomial
e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0.245 & birds_list$total_perc_afected_ear > 24.5 & !is.na(birds_list$total_perc_afected_ear),]$binomial
t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0.245 & birds_list$total_perc_afected_tsu > 24.5 & !is.na(birds_list$total_perc_afected_tsu),]$binomial
h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0.245 & birds_list$total_perc_afected_hur > 24.5 & !is.na(birds_list$total_perc_afected_hur),]$binomial

b_list = c(v_b_list,e_b_list,t_b_list,h_b_list)


st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% a_list,],r_grid)

st_crs(reptiles) = st_crs(r_grid)
reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% r_list,],r_grid)

st_crs(mammals) = st_crs(r_grid)
mammals_grid = st_intersects(mammals[mammals$binomial%in% m_list,],r_grid)

st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% b_list,],r_grid)

nrow(birds_breeding_resident)+nrow(mammals)+nrow(reptiles)+nrow(amphibians)


r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))
r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))
r_grid$richness_m = unlist(lapply(t(mammals_grid),length))
r_grid$richness_b = unlist(lapply(t(birds_grid),length))


r_grid_temp = r_grid
st_geometry(r_grid_temp) = NULL
r_grid_temp[is.na(r_grid_temp)]= 0

r_grid$all_vertebrates = unlist(rowSums(r_grid_temp[,c('richness_a','richness_r','richness_m','richness_b')]))

r_grid[r_grid$all_vertebrates%in%0,]$all_vertebrates = NA
hist(r_grid$all_vertebrates)


pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'

library(raster)
library(rnaturalearth)
world_map = ne_countries(scale = 10,returnclass = 'sf')

pal1 = colorRampPalette(rev(c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')))
world_map = st_transform(world_map,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84")


vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$all_vertebrates>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$all_vertebrates>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$all_vertebrates>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$all_vertebrates>0,]),200000))


pal2 =colorRampPalette(hcl.colors(10,'Viridis',rev = T)[1:10])

library(ggplot2)

theme_harith = theme(legend.position = c(0.925,0.9),
                     legend.text = element_text(family = myfont,size = 5),
                     legend.key.width = unit(0.25,'cm'),
                     legend.key.height =  unit(0.25,'cm'),
                     legend.title = element_text(family = myfont,size = 6,hjust = 0.5,angle = 90),
                     legend.key = element_rect(fill = 'black'))

pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))
pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))

plot_all = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$all_vertebrates>0,],aes(fill = all_vertebrates,col = all_vertebrates),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[4])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'vertebrates',size = 3,family = myfont)



ggsave(filename = 'richness_2500km_plus_1100_all_sps_Apr17.png',plot = plot_all,dpi = 2000,width = 6,height = 4)


#plot_all_affected species

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0 & amphibians_list$total_perc_afected_vul > 0 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial
e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0 & amphibians_list$total_perc_afected_ear > 0 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0 & amphibians_list$total_perc_afected_tsu > 0 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial

a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

reptiles_list = read.xlsx('reptiles_grid2_all_disasters_run7_APRL_17.xlsx')

v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0 & reptiles_list$total_perc_afected_vul > 0 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial
e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0 & reptiles_list$total_perc_afected_ear > 0 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial
t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0 & reptiles_list$total_perc_afected_tsu > 0 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial
h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0 & reptiles_list$total_perc_afected_hur > 0 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial

r_list = c(v_r_list,e_r_list,t_r_list,h_r_list)

mammals_list = read.xlsx('mammals_grid2_all_disasters_run7_APRL_17.xlsx')

v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0 & mammals_list$total_perc_afected_vul > 0 & !is.na(mammals_list$total_perc_afected_vul),]$binomial
e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0 & mammals_list$total_perc_afected_ear > 0 & !is.na(mammals_list$total_perc_afected_ear),]$binomial
t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0 & mammals_list$total_perc_afected_tsu > 0 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial
h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0 & mammals_list$total_perc_afected_hur > 0 & !is.na(mammals_list$total_perc_afected_hur),]$binomial

m_list = c(v_m_list,e_m_list,t_m_list,h_m_list)

birds_list = read.xlsx('birds_breeding_grid2_all_disasters_run7_APRL_17.xlsx')

v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0 & birds_list$total_perc_afected_vul > 0 & !is.na(birds_list$total_perc_afected_vul),]$binomial
e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0 & birds_list$total_perc_afected_ear > 0 & !is.na(birds_list$total_perc_afected_ear),]$binomial
t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0 & birds_list$total_perc_afected_tsu > 0 & !is.na(birds_list$total_perc_afected_tsu),]$binomial
h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0 & birds_list$total_perc_afected_hur > 0 & !is.na(birds_list$total_perc_afected_hur),]$binomial

b_list = c(v_b_list,e_b_list,t_b_list,h_b_list)



st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% a_list,],r_grid)

st_crs(reptiles) = st_crs(r_grid)
reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% r_list,],r_grid)

st_crs(mammals) = st_crs(r_grid)
mammals_grid = st_intersects(mammals[mammals$binomial%in% m_list,],r_grid)

st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% b_list,],r_grid)

nrow(birds_breeding_resident)+nrow(mammals)+nrow(reptiles)+nrow(amphibians)


r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))
r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))
r_grid$richness_m = unlist(lapply(t(mammals_grid),length))
r_grid$richness_b = unlist(lapply(t(birds_grid),length))




#save(r_grid,file = 'r_grid_richness_2500km_plus_1100.Rdata')

#load('r_grid_richness_2500km.Rdata')

#load('r_grid_richness_2500km_plus_1100.Rdata')

r_grid_temp = r_grid
st_geometry(r_grid_temp) = NULL
r_grid_temp[is.na(r_grid_temp)]= 0

r_grid$all_vertebrates = unlist(rowSums(r_grid_temp[,c('richness_a','richness_r','richness_m','richness_b')]))
r_grid[r_grid$all_vertebrates%in%0,]$all_vertebrates = NA
hist(r_grid$all_vertebrates)



pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
pal_earthquakes = colorRampPalette(rev(hcl.colors(5,'YlOrRd')))
pal_tsunamis = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))

pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))

pal_hurricanes = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))
pal_hurricanes = colorRampPalette(c('#f2f0f7','#cbc9e2','#9e9ac8','#756bb1','#54278f'))
pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))


vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0 & r_grid$all_vertebrates>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0 & r_grid$all_vertebrates>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0 & r_grid$all_vertebrates>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0 & r_grid$all_vertebrates>0,]),200000))

plot_all_2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$all_vertebrates>0,],aes(fill = all_vertebrates,col = all_vertebrates),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))
#+annotate(geom = 'text',x = 15540050,y = -7017985, label = 'vertebrates',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_sps_v2_Apr17.png',plot = plot_all_2,dpi = 2000,width = 6,height = 4)

library(gridExtra)
grid = arrangeGrob(plot_all_2,plot_all,ncol = 1)

ggsave(filename = 'richness_grid_Apr17.png',plot = grid,dpi = 2000,width = 6,height = 6)

#amphibians----

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0.245 & amphibians_list$total_perc_afected_vul > 24.5 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial
e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0.245 & amphibians_list$total_perc_afected_ear > 24.5 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0.245 & amphibians_list$total_perc_afected_tsu > 24.5 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial

a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))
colnames(r_grid)
vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_a>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_a>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_a>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'amphibians',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_Apr17.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0 & amphibians_list$total_perc_afected_vul > 0 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial
e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0 & amphibians_list$total_perc_afected_ear > 0 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0 & amphibians_list$total_perc_afected_tsu > 0 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial

a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  # geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'amphibians_grid_Apr17.png',plot = grid,dpi = 2000,width = 6,height = 6)



##vulcanoes----

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0.245 & amphibians_list$total_perc_afected_vul > 24.5 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial

# e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0.245 & amphibians_list$total_perc_afected_ear > 24.5 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
# t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0.245 & amphibians_list$total_perc_afected_tsu > 24.5 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% v_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_a>0,]),500000))
#vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245,]),500000))

# tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_a>0,]),400000))
# earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_a>0,]),300000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))



plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'amphibians\n volcanoes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_vul>0 & amphibians_list$total_perc_afected_vul > 0 & !is.na(amphibians_list$total_perc_afected_vul),]$binomial
# e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0 & amphibians_list$total_perc_afected_ear > 0 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
# t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0 & amphibians_list$total_perc_afected_tsu > 0 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% v_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_v_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'amphibians_grid_apr17_v.png',plot = grid,dpi = 2000,width = 6,height = 6)



##earthquakes----

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0.245 & amphibians_list$total_perc_afected_ear > 24.5 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
# t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0.245 & amphibians_list$total_perc_afected_tsu > 24.5 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% e_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))


# tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_a>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_a>0,]),500000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))



plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'amphibians\n earthquakes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_e_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



e_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_ear>0 & amphibians_list$total_perc_afected_ear > 0 & !is.na(amphibians_list$total_perc_afected_ear),]$binomial
# t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0 & amphibians_list$total_perc_afected_tsu > 0 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% e_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_e_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'amphibians_grid_apr17_e.png',plot = grid,dpi = 2000,width = 6,height = 6)

##tsunamis----

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0.245 & amphibians_list$total_perc_afected_tsu > 24.5 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% t_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))


tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_a>0,]),500000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))



plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'amphibians\n tsunamis',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_t_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



t_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_tsu>0 & amphibians_list$total_perc_afected_tsu > 0 & !is.na(amphibians_list$total_perc_afected_tsu),]$binomial
# h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% t_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_t_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'amphibians_grid_apr17_t.png',plot = grid,dpi = 2000,width = 6,height = 6)

##hurricanes----

amphibians_list = read.xlsx('amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0.245 & amphibians_list$total_perc_afected_hur > 24.5 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial


st_crs(amphibians) = st_crs(r_grid)
amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% h_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))


hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),500000))



plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'amphibians\n hurricanes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_h_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



h_a_list = amphibians_list[amphibians_list$avg_impact_sum_final_hur>0 & amphibians_list$total_perc_afected_hur > 0 & !is.na(amphibians_list$total_perc_afected_hur),]$binomial


amphibians_grid = st_intersects(amphibians[amphibians$binomial %in% h_a_list,],r_grid)

r_grid$richness_a = unlist(lapply(t(amphibians_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_a>0,],aes(fill = richness_a,col = richness_a),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_amphibians_apr17_h_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'amphibians_grid_apr17_h.png',plot = grid,dpi = 2000,width = 6,height = 6)






#birds----

birds_list = read.xlsx('birds_breeding_grid2_all_disasters_run7_APRL_17.xlsx')

v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0.245 & birds_list$total_perc_afected_vul > 24.5 & !is.na(birds_list$total_perc_afected_vul),]$binomial
e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0.245 & birds_list$total_perc_afected_ear > 24.5 & !is.na(birds_list$total_perc_afected_ear),]$binomial
t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0.245 & birds_list$total_perc_afected_tsu > 24.5 & !is.na(birds_list$total_perc_afected_tsu),]$binomial
h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0.245 & birds_list$total_perc_afected_hur > 24.5 & !is.na(birds_list$total_perc_afected_hur),]$binomial

b_list = c(v_b_list,e_b_list,t_b_list,h_b_list)

st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_b>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_b>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_b>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_b>0,]),200000))


breaks = c(5,10,15)

plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'birds',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0 & birds_list$total_perc_afected_vul > 0 & !is.na(birds_list$total_perc_afected_vul),]$binomial
e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0 & birds_list$total_perc_afected_ear > 0 & !is.na(birds_list$total_perc_afected_ear),]$binomial
t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0 & birds_list$total_perc_afected_tsu > 0 & !is.na(birds_list$total_perc_afected_tsu),]$binomial
h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0 & birds_list$total_perc_afected_hur > 0 & !is.na(birds_list$total_perc_afected_hur),]$binomial

b_list = c(v_b_list,e_b_list,t_b_list,h_b_list)

birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_v2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'birds_grid_apr17.png',plot = grid,dpi = 2000,width = 6,height = 6)

birds_list = read.xlsx('birds_breeding_grid2_all_disasters_run7_APRL_17.xlsx')

##vulcanoes----


v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0.245 & birds_list$total_perc_afected_vul > 24.5 & !is.na(birds_list$total_perc_afected_vul),]$binomial

# e_a_list = birds_list[birds_list$avg_impact_sum_final_ear>0.245 & birds_list$total_perc_afected_ear > 24.5 & !is.na(birds_list$total_perc_afected_ear),]$binomial
# t_a_list = birds_list[birds_list$avg_impact_sum_final_tsu>0.245 & birds_list$total_perc_afected_tsu > 24.5 & !is.na(birds_list$total_perc_afected_tsu),]$binomial
# h_a_list = birds_list[birds_list$avg_impact_sum_final_hur>0.245 & birds_list$total_perc_afected_hur > 24.5 & !is.na(birds_list$total_perc_afected_hur),]$binomial
# 
# a_list = c(v_a_list,e_a_list,t_a_list,h_a_list)

st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% v_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_b>0,]),500000))

breaks = c(5,10,15,20)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'birds\n volcanoes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_v_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_b_list = birds_list[birds_list$avg_impact_sum_final_vul>0 & birds_list$total_perc_afected_vul > 0 & !is.na(birds_list$total_perc_afected_vul),]$binomial

birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% v_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_v_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'birds_grid_apr17_v.png',plot = grid,dpi = 2000,width = 6,height = 6)



##earthquakes----


e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0.245 & birds_list$total_perc_afected_ear > 24.5 & !is.na(birds_list$total_perc_afected_ear),]$binomial

st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% e_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_b>0,]),500000))


breaks = c(2,4,6,8,10)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'birds\n earthquakes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_e_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



e_b_list = birds_list[birds_list$avg_impact_sum_final_ear>0 & birds_list$total_perc_afected_ear > 0 & !is.na(birds_list$total_perc_afected_ear),]$binomial

birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% e_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_e_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'birds_grid_apr17_e.png',plot = grid,dpi = 2000,width = 6,height = 6)

##tsunamis----

t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0.245 & birds_list$total_perc_afected_tsu > 24.5 & !is.na(birds_list$total_perc_afected_tsu),]$binomial


st_crs(birds_breeding) = st_crs(r_grid)
birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% t_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))


tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_b>0,]),500000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))

breaks = c(5,10,15,20)


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'birds\n tsunamis',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_t_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



t_b_list = birds_list[birds_list$avg_impact_sum_final_tsu>0 & birds_list$total_perc_afected_tsu > 0 & !is.na(birds_list$total_perc_afected_tsu),]$binomial


birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% t_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_t_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'birds_grid_apr17_t.png',plot = grid,dpi = 2000,width = 6,height = 6)

##hurricanes----


h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0.245 & birds_list$total_perc_afected_hur > 24.5 & !is.na(birds_list$total_perc_afected_hur),]$binomial

birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% h_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_b>0,]),500000))


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'birds\n hurricanes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_h_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



h_b_list = birds_list[birds_list$avg_impact_sum_final_hur>0 & birds_list$total_perc_afected_hur > 0 & !is.na(birds_list$total_perc_afected_hur),]$binomial


birds_grid = st_intersects(birds_breeding[birds_breeding$binomial %in% h_b_list,],r_grid)

r_grid$richness_b = unlist(lapply(t(birds_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_apr17_h_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'birds_grid_apr17_h.png',plot = grid,dpi = 2000,width = 6,height = 6)




#mammals----

mammals_list = read.xlsx('mammals_grid2_all_disasters_run7_APRL_17.xlsx')

v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0.245 & mammals_list$total_perc_afected_vul > 24.5 & !is.na(mammals_list$total_perc_afected_vul),]$binomial
e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0.245 & mammals_list$total_perc_afected_ear > 24.5 & !is.na(mammals_list$total_perc_afected_ear),]$binomial
t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0.245 & mammals_list$total_perc_afected_tsu > 24.5 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial
h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0.245 & mammals_list$total_perc_afected_hur > 24.5 & !is.na(mammals_list$total_perc_afected_hur),]$binomial

m_list = c(v_m_list,e_m_list,t_m_list,h_m_list)

st_crs(mammals) = st_crs(r_grid)
mammals_grid = st_intersects(mammals[mammals$binomial %in% m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_m>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_m>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_m>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_m>0,]),200000))


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'mammals',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0 & mammals_list$total_perc_afected_vul > 0 & !is.na(mammals_list$total_perc_afected_vul),]$binomial
e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0 & mammals_list$total_perc_afected_ear > 0 & !is.na(mammals_list$total_perc_afected_ear),]$binomial
t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0 & mammals_list$total_perc_afected_tsu > 0 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial
h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0 & mammals_list$total_perc_afected_hur > 0 & !is.na(mammals_list$total_perc_afected_hur),]$binomial

m_list = c(v_m_list,e_m_list,t_m_list,h_m_list)

mammals_grid = st_intersects(mammals[mammals$binomial %in% m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  # geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_v2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'mammals_grid_apr17.png',plot = grid,dpi = 2000,width = 6,height = 6)


##vulcanoes----


v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0.245 & mammals_list$total_perc_afected_vul > 24.5 & !is.na(mammals_list$total_perc_afected_vul),]$binomial
mammals_grid = st_intersects(mammals[mammals$binomial %in% v_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_m>0,]),500000))

breaks = c(5,10,15,20)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'mammals\n volcanoes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_v_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_m_list = mammals_list[mammals_list$avg_impact_sum_final_vul>0 & mammals_list$total_perc_afected_vul > 0 & !is.na(mammals_list$total_perc_afected_vul),]$binomial

mammals_grid = st_intersects(mammals[mammals$binomial %in% v_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_v_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'mammals_grid_apr17_v.png',plot = grid,dpi = 2000,width = 6,height = 6)



##earthquakes----


e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0.245 & mammals_list$total_perc_afected_ear > 24.5 & !is.na(mammals_list$total_perc_afected_ear),]$binomial

mammals_grid = st_intersects(mammals[mammals$binomial %in% e_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_m>0,]),500000))


breaks = c(2,4,6,8,10)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'mammals\n earthquakes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_e_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



e_m_list = mammals_list[mammals_list$avg_impact_sum_final_ear>0 & mammals_list$total_perc_afected_ear > 0 & !is.na(mammals_list$total_perc_afected_ear),]$binomial

mammals_grid = st_intersects(mammals[mammals$binomial %in% e_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_e_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'mammals_grid_apr17_e.png',plot = grid,dpi = 2000,width = 6,height = 6)

##tsunamis----

t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0.245 & mammals_list$total_perc_afected_tsu > 24.5 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial

mammals_grid = st_intersects(mammals[mammals$binomial %in% t_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))


tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_m>0,]),500000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))

breaks = c(5,10,15,20)


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'mammals\n tsunamis',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_t_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



t_m_list = mammals_list[mammals_list$avg_impact_sum_final_tsu>0 & mammals_list$total_perc_afected_tsu > 0 & !is.na(mammals_list$total_perc_afected_tsu),]$binomial


mammals_grid = st_intersects(mammals[mammals$binomial %in% t_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_t_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'mammals_grid_apr17_t.png',plot = grid,dpi = 2000,width = 6,height = 6)

##hurricanes----


h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0.245 & mammals_list$total_perc_afected_hur > 24.5 & !is.na(mammals_list$total_perc_afected_hur),]$binomial

mammals_grid = st_intersects(mammals[mammals$binomial %in% h_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_m>0,]),500000))

breaks = c(5,10,15,20)
breaks = c(2,4,6,8,10,12)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'mammals\n hurricanes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_h_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



h_m_list = mammals_list[mammals_list$avg_impact_sum_final_hur>0 & mammals_list$total_perc_afected_hur > 0 & !is.na(mammals_list$total_perc_afected_hur),]$binomial


mammals_grid = st_intersects(mammals[mammals$binomial %in% h_m_list,],r_grid)

r_grid$richness_m = unlist(lapply(t(mammals_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_apr17_h_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'mammals_grid_apr17_h.png',plot = grid,dpi = 2000,width = 6,height = 6)



#reptiles----

reptiles_list = read.xlsx('reptiles_grid2_all_disasters_run7_APRL_17.xlsx')

v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0.245 & reptiles_list$total_perc_afected_vul > 24.5 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial
e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0.245 & reptiles_list$total_perc_afected_ear > 24.5 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial
t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0.245 & reptiles_list$total_perc_afected_tsu > 24.5 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial
h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0.245 & reptiles_list$total_perc_afected_hur > 24.5 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial

r_list = c(v_r_list,e_r_list,t_r_list,h_r_list)

st_crs(reptiles) = st_crs(r_grid)
reptiles_grid = st_intersects(reptiles[reptiles$binomial %in%r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_r>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_r>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_r>0,]),200000))


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'reptiles',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0 & reptiles_list$total_perc_afected_vul > 0 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial
e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0 & reptiles_list$total_perc_afected_ear > 0 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial
t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0 & reptiles_list$total_perc_afected_tsu > 0 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial
h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0 & reptiles_list$total_perc_afected_hur > 0 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial

r_list = c(v_r_list,e_r_list,t_r_list,h_r_list)

reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  # geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_v2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'reptiles_grid_apr17.png',plot = grid,dpi = 2000,width = 6,height = 6)


##vulcanoes----


v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0.245 & reptiles_list$total_perc_afected_vul > 24.5 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial


st_crs(reptiles) = st_crs(r_grid)
reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% v_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))

breaks = c(5,10,15,20)
plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[5])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'reptiles\n volcanoes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_v_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



v_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_vul>0 & reptiles_list$total_perc_afected_vul > 0 & !is.na(reptiles_list$total_perc_afected_vul),]$binomial

reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% v_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_v_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'reptiles_grid_apr17_v.png',plot = grid,dpi = 2000,width = 6,height = 6)



##earthquakes----


e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0.245 & reptiles_list$total_perc_afected_ear > 24.5 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial

st_crs(birds_breeding) = st_crs(r_grid)
reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% e_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))


breaks = c(2,4,6,8,10)
breaks = c(5,10,15,20)

plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  # geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[3])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'reptiles\n earthquakes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_e_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



e_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_ear>0 & reptiles_list$total_perc_afected_ear > 0 & !is.na(reptiles_list$total_perc_afected_ear),]$binomial

reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% e_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_e_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'reptiles_grid_apr17_e.png',plot = grid,dpi = 2000,width = 6,height = 6)

##tsunamis----

t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0.245 & reptiles_list$total_perc_afected_tsu > 24.5 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial

reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% t_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))


tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))
# hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_a>0,]),200000))

breaks = c(5,10,15,20)


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[4])+
  # geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at high risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'reptiles\n tsunamis',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_t_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



t_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_tsu>0 & reptiles_list$total_perc_afected_tsu > 0 & !is.na(reptiles_list$total_perc_afected_tsu),]$binomial


reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% t_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  scale_colour_gradientn('Species at risk',colours = pal2(10),breaks = breaks)+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_t_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'reptiles_grid_apr17_t.png',plot = grid,dpi = 2000,width = 6,height = 6)

##hurricanes----


h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0.245 & reptiles_list$total_perc_afected_hur > 24.5 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial

reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% h_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))


plot1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[3])+
  scale_fill_gradientn('Species at high risk',colours = pal2(10))+
  scale_colour_gradientn('Species at high risk',colours = pal2(10))+
  theme_void()+
  # theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
  #       legend.key.width = unit(1.5,'cm'),
  #       legend.key.height =  unit(0.25,'cm'),
  #       legend.title = element_text(family = myfont))+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))+
  annotate(geom = 'text',x = 15540050,y = -7017985, label = 'reptiles\n hurricanes',size = 3,family = myfont)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_h_1.png',plot = plot1,dpi = 2000,width = 6,height = 4)



h_r_list = reptiles_list[reptiles_list$avg_impact_sum_final_hur>0 & reptiles_list$total_perc_afected_hur > 0 & !is.na(reptiles_list$total_perc_afected_hur),]$binomial


reptiles_grid = st_intersects(reptiles[reptiles$binomial %in% h_r_list,],r_grid)

r_grid$richness_r = unlist(lapply(t(reptiles_grid),length))

plot2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',linewidth = 0.05,col = 'lightblue')+
  geom_sf(data = world_map,bg = 'grey70',linewidth = 0.05,col = 'grey70')+
  geom_sf(data = r_grid[r_grid$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  scale_fill_gradientn('Species at risk',colours = pal2(10))+
  scale_colour_gradientn('Species at risk',colours = pal2(10))+
  theme_void()+
  theme_harith+
  guides(fill = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'),
         col = guide_colourbar(title.position = "left",label.position = 'right',frame.colour = 'black'))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_apr17_h_2.png',plot = plot2,dpi = 2000,width = 6,height = 4)


grid = arrangeGrob(plot2,plot1,ncol = 1)

ggsave(filename = 'reptiles_grid_apr17_h.png',plot = grid,dpi = 2000,width = 6,height = 6)











vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.245 & r_grid$richness_r>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.245 & r_grid$richness_r>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.245 & r_grid$richness_r>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.245 & r_grid$richness_r>0,]),200000))

plot2 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid_richness[r_grid_richness$richness_r>0,],aes(fill = richness_r,col = richness_r),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[4])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[3])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[2])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[4])+
  scale_fill_gradientn('Number of species (reptiles)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (reptiles)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_reptiles_Feb29.png',plot = plot2,dpi = 2000,width = 6,height = 4)






vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.25 & r_grid$richness_m>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.25 & r_grid$richness_m>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.25 & r_grid$richness_m>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.25 & r_grid$richness_m>0,]),200000))

plot3 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid_richness[r_grid_richness$richness_m>0,],aes(fill = richness_m,col = richness_m),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[4])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[3])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[2])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[4])+
  scale_fill_gradientn('Number of species (mammals)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (mammals)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_mammals_Feb1.png',plot = plot3,dpi = 2000,width = 6,height = 4)


vulcano_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$vulcanos_prob_sum_final>0.25 & r_grid$richness_b>0,]),500000))
tsunamis_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$tsunamis_prob_sum_final>0.25 & r_grid$richness_b>0,]),400000))
earthquake_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$earthquakes_prob_sum_final>0.25 & r_grid$richness_b>0,]),300000))
hurricane_buffer_high = st_union(st_buffer(st_centroid(r_grid[r_grid$hurricanes_prob_sum_final>0.25 & r_grid$richness_b>0,]),200000))

plot4 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid_richness[r_grid_richness$richness_b>0,],aes(fill = richness_b,col = richness_b),linewidth = 0.01)+
  geom_sf(data = vulcano_buffer_high,bg = NA, col = pal_vul(5)[4])+
  geom_sf(data = tsunamis_buffer_high,bg = NA, col = pal_tsunamis(5)[3])+
  geom_sf(data = earthquake_buffer_high,bg = NA, col = pal_earthquakes(5)[2])+
  geom_sf(data = hurricane_buffer_high,bg = NA, col = pal_hurricanes(5)[4])+
  scale_fill_gradientn('Number of species (birds)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (birds)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_birds_Feb1.png',plot = plot4,dpi = 2000,width = 6,height = 4)



# p5 = ggplot()+
#   geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
#   geom_sf(data = r_grid[!is.na(r_grid$richness_b_breeding),],aes(fill = richness_b_breeding,col = richness_b_breeding),linewidth = 0.01)+
#   scale_fill_gradientn('Number of species (birds-breeding)',colours = pal1(10))+
#   scale_colour_gradientn('Number of species (birds-breeding)',colours = pal1(10))+
#   theme_void()+
#   theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
#         legend.key.width = unit(1.5,'cm'),
#         legend.key.height =  unit(0.25,'cm'),
#         legend.title = element_text(family = myfont))+
#   guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

p6 = ggplot()+
  geom_sf(data = world_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[!is.na(r_grid$all_vertebrates),],aes(fill = all_vertebrates,col = all_vertebrates),linewidth = 0.01)+
  scale_fill_gradientn('Number of species (combined)',colours = pal1(10))+
  scale_colour_gradientn('Number of species (combined)',colours = pal1(10))+
  theme_void()+
  theme(legend.position = 'bottom',legend.text = element_text(family = myfont),
        legend.key.width = unit(1.5,'cm'),
        legend.key.height =  unit(0.25,'cm'),
        legend.title = element_text(family = myfont))+
  guides(fill = guide_colourbar(title.position = "top",title.hjust = 0.5))

library(gridExtra)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed_all_species.png',plot = p4,dpi = 2000,width = 6,height = 4)


gridded_maps = arrangeGrob(p1,p2,p3,p4,ncol = 2)

#gridded_maps = arrangeGrob(p1,p2,p3,p4,p5,p6,ncol = 2)

ggsave(filename = 'richness_2500km_plus_1100_all_fixed.png',plot = gridded_maps,dpi = 2000,width = 10,height = 8)






