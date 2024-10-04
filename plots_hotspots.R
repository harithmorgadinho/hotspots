
library(sf)
sf_use_s2(F)
library(ggplot2)
load('r_grid_all_fixed_17_Sep_2024.Rdata')

#amphibians
r_grid[r_grid$land ==1 & !r_grid$agriculture_a_hot%in%1,]$agriculture_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$logging_a_hot%in%1,]$logging_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_a_hot%in%1,]$hunting_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_a_hot%in%1,]$pollution_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_a_hot%in%1,]$invasives_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_a_hot%in%1,]$climate_change_a_hot = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_a_hot%in%1,]$urbanization_a_hot = 0
r_grid$a_cum_threats = NA
r_grid$a_cum_threats = r_grid$agriculture_a_hot+r_grid$logging_a_hot+r_grid$hunting_a_hot+r_grid$pollution_a_hot+r_grid$invasives_a_hot+
  r_grid$climate_change_a_hot+r_grid$urbanization_a_hot

#reptiles
r_grid[r_grid$land ==1 & !r_grid$agriculture_r_hot%in%1,]$agriculture_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$logging_r_hot%in%1,]$logging_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_r_hot%in%1,]$hunting_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_r_hot%in%1,]$pollution_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_r_hot%in%1,]$invasives_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_r_hot%in%1,]$climate_change_r_hot = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_r_hot%in%1,]$urbanization_r_hot = 0

r_grid$r_cum_threats = NA
r_grid$r_cum_threats = r_grid$agriculture_r_hot+r_grid$logging_r_hot+r_grid$hunting_r_hot+r_grid$pollution_r_hot+r_grid$invasives_r_hot+
  r_grid$climate_change_r_hot+r_grid$urbanization_r_hot

#mammals
r_grid[r_grid$land ==1 & !r_grid$agriculture_m_hot%in%1,]$agriculture_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$logging_m_hot%in%1,]$logging_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_m_hot%in%1,]$hunting_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_m_hot%in%1,]$pollution_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_m_hot%in%1,]$invasives_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_m_hot%in%1,]$climate_change_m_hot = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_m_hot%in%1,]$urbanization_m_hot = 0

r_grid$m_cum_threats = NA
r_grid$m_cum_threats = r_grid$agriculture_m_hot+r_grid$logging_m_hot+r_grid$hunting_m_hot+r_grid$pollution_m_hot+r_grid$invasives_m_hot+
  r_grid$climate_change_m_hot+r_grid$urbanization_m_hot

#birds
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


library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'






pal_threats = colorRampPalette(hcl.colors(5,'Inferno',rev = T))
pal_a = colorRampPalette(hcl.colors(5,'BluYl',rev = T))
pal_r = colorRampPalette(hcl.colors(5,'YlGn',rev = T))
pal_m = colorRampPalette(hcl.colors(5,'YlOrRd',rev = T))
pal_b = colorRampPalette(hcl.colors(5,'PinkYl',rev = T))

# sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
# r = raster(extent(sf_land),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
# background_r_grid = st_as_sf(rasterToPolygons(r))
# background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
# background_r_grid$layer = 1
# background_r_grid_summ = background_r_grid %>% group_by(layer) %>% summarise()
# 
# sf_land_low_res = st_simplify(sf_land,dTolerance = 0.01)

library(raster)
wwf_regions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')
r = raster(extent(wwf_regions),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
background_r_grid = st_as_sf(rasterToPolygons(r))
background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
background_map = st_transform(wwf_regions, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")




theme_harith = theme(legend.position = c(0.93,0.81),
                     legend.key.height  = unit(0.3,'cm'),
                     legend.key.width  = unit(0.3,'cm'),
                     legend.title = element_text(size = 10,hjust = 0.5,angle = 90,family = myfont),
                     legend.text = element_text(size = 10,hjust = 0.5,family = myfont))

theme_2 = theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  panel.spacing = unit(c(0,0,0,0),"mm"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank())

guides_harith = guides(fill = guide_legend(label.position = 'right',title.position = 'left',direction = 'vertical'),
       colour = guide_legend(label.position = 'right',title.position = 'left',direction = 'vertical'))


p_1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$a_cum_threats>0,],aes(col = a_cum_threats,fill = a_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+
  theme_harith+
  guides_harith+
  theme_2+
  annotate(geom = 'text',label = 'Amphibians',x = 16040050, y= -7517985,size = 5,family = myfont)

p_2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$r_cum_threats>0,],aes(col = r_cum_threats,fill = r_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+
  theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Reptiles',x = 16040050, y= -7517985,size = 5,family = myfont)


p_3 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$m_cum_threats>0,],aes(col = m_cum_threats,fill = m_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Mammals',x = 16040050, y= -7517985,size = 5,family = myfont)

p_4 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$b_cum_threats>0,],aes(col = b_cum_threats,fill = b_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by hotspot',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Birds',x = 16040050, y= -7517985,size = 5,family = myfont)


library(gridExtra)
grid = arrangeGrob(p_1,p_2,p_3,p_4,ncol=2)


ggsave(filename = 'cum_threats.png',plot = grid,width = 10,height = 5,dpi = 500)

r_grid$all_cum_threats = NA

r_grid$all_cum_threats = r_grid$a_cum_threats + r_grid$r_cum_threats + r_grid$m_cum_threats + r_grid$b_cum_threats

#save(r_grid,file = 'r_grid_all_fixed_19_Jan.Rdata')

hotspots = st_read('/Users/gdt366/Dropbox/hotspots_2016_1/hotspots_2016_1.shp')
hotspots = hotspots[-40,]
hotspot40 = st_read('/Users/gdt366/Dropbox/postdoc_KU_paper_2/hotspot_fixed.shp')
hotspots = rbind(hotspots,hotspot40)

hotspots = st_crop(hotspots,extent(c(-179,179,-90,90)))

hotspots_area = st_transform(hotspots,crs = st_crs(r_grid)[[1]])

# load('sf_land_sum.Rdata')
# nrow(sf_land)



coords <- matrix(c(-179.9, -89.9, 179.9, -89.9, 179.9, 89.9, -179.9, 89.9, -179.9, -89.9), ncol = 2, byrow = TRUE)
polygon <- st_polygon(list(coords))

sf_object <- st_sf(geometry = st_sfc(polygon, crs = 4326))
plot(sf_object)
sf_globe_outline = st_transform(sf_object,crs = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
plot(sf_globe_outline)

# r_grid_melt
# r_grid_melt = st_union(sf_land)

library(rnaturalearth)
worldmap = ne_countries(scale = 10,returnclass = 'sf')
worldmap = st_transform(worldmap,crs = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')


theme_harith_plot2 = theme(legend.position = c(0.5,-0.04),
                     legend.key.height  = unit(0.75,'cm'),
                     legend.key.width  = unit(1.5,'cm'),
                     legend.title = element_text(size = 12,hjust = 0.5,family = myfont),
                     legend.text = element_text(size = 12,hjust = 0.5,family = myfont),
                     legend.spacing.x = unit(0, 'cm'))

theme_2_plot2 = theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  panel.spacing = unit(c(0,0,0,0),"mm"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank())

guides_harith_plot2 = guides(fill = guide_legend(label.position = 'bottom',title.position = 'top',direction = 'horizontal',nrow = 1),
                       colour = guide_legend(label.position = 'bottom',title.position = 'top',direction = 'horizontal',nrow = 1))

p_all = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = hotspots_area,col = 'transparent',linewidth = 0.1,fill = 'grey80')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$all_cum_threats>0,],aes(col = all_cum_threats,fill = all_cum_threats),linewidth = 0)+
  geom_sf(data = hotspots_area,col = alpha("black",0.2),linewidth = 0.2,fill = NA)+
  scale_fill_gradientn('Threats by taxa',colours = pal_m(10),breaks = c(1:14))+
  scale_colour_gradientn('Threats by taxa',colours = pal_m(10),breaks = c(1:14))+
  theme_void()+theme_harith_plot2+theme_2_plot2+
  guides_harith_plot2+
  #geom_sf(data = background_r_grid,bg = NA,col = 'black',linewidth = 0.1)+
  annotate(geom = 'text',label = 'Vertebrate hotspots',x = 15040050, y= -7917985,size = 5,family = myfont)+
  theme(legend.key.spacing.x = unit(0,'cm'))


# ggsave(filename = 'cum_threats_all_test.png',plot = p_all,width = 6,height = 5,dpi = 500)
# 
# ggsave(filename = 'cum_threats_all.png',plot = p_all,width = 6,height = 5,dpi = 2000)

library(patchwork)
bottom_grid <- p_1 + p_4 + p_3 + p_2 + 
  plot_layout(nrow = 2, ncol = 2)

combined_plot <- p_all + bottom_grid + 
  plot_layout(heights = c(1.1, 0.9))

ggsave(filename = 'Figure1_Sep_23.png',plot = combined_plot,width = 12,height = 14,dpi = 2000)
ggsave(filename = 'Figure1_Sep_23.1500.png',plot = combined_plot,width = 12,height = 14,dpi = 1500)







