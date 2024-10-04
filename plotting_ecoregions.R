#load('r_grid_metrics_run2_deciles_nov_27.Rdata')
load('r_grid_all_fixed_17_Sep_2024.Rdata')

load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_NOV26.Rdata')

r_grid$ecoregion = r_grid_soc_eco$ECO_NAME
unique(r_grid$ecoregion)
plot(r_grid[r_grid$ecoregion %in% 'South Antarctic Peninsula tundra',]$geometry)

library(dplyr)


r_grid[is.na(r_grid$hotspots_all),]$hotspots_all = 0

wwf_regions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')
wwf_regions = st_transform(wwf_regions,"+proj=moll +lon_0=0 +x_0=0 +y_0=0")


df_hotspots_ecoregion = r_grid %>%
  group_by(ecoregion) %>%
  summarise(
    hotspot_ecoregion = sum(hotspots_all), # Sum of hotspots for each ecoregion
    total = n(), # Number of rows in each ecoregion
    avg_hotspots = 100*(hotspot_ecoregion / total) # Average hotspots per ecoregion
  )

st_geometry(df_hotspots_ecoregion) = NULL
wwf_regions_merged = merge(wwf_regions,df_hotspots_ecoregion,by.x = 'ECO_NAME',by.y = 'ecoregion',all.x = T)

wwf_regions_merged[is.na(wwf_regions_merged$avg_hotspots),]$avg_hotspots = 0

library(ggplot2)


library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'


pal_m = colorRampPalette(hcl.colors(5,'YlOrRd',rev = T))

# sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
# r = raster(extent(sf_land),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
# background_r_grid = st_as_sf(rasterToPolygons(r))
# background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
# background_r_grid$layer = 1
# background_r_grid_summ = background_r_grid %>% group_by(layer) %>% summarise()

wwf_regions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')
r = raster(extent(wwf_regions),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
background_r_grid = st_as_sf(rasterToPolygons(r))
background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
background_map = st_transform(wwf_regions, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")




theme_harith_plot2 = theme(legend.position = c(0.5,-0.05),
                           legend.key.height  = unit(0.5,'cm'),
                           legend.key.width  = unit(1.25,'cm'),
                           legend.title = element_text(size = 10,hjust = 0.5,family = myfont),
                           legend.text = element_text(size = 10,hjust = 0.5,family = myfont),
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



p = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = wwf_regions_merged, aes(fill = avg_hotspots, col = avg_hotspots),linewidth = 0)+
  theme_void()+theme_harith_plot2+theme_2_plot2+
  guides_harith_plot2+
  scale_fill_gradientn('Percentage of ecoregion',colours = pal_m(10),breaks = 10*c(0:10))+
  scale_colour_gradientn('Percentage of ecoregion',colours = pal_m(10),breaks = 10*c(0:10))+
  theme(legend.key.spacing.x = unit(0,'cm'))

ggsave(filename = 'ecoregions_plot_23_sep.png',plot = p,width = 8,height = 5,dpi = 2000)






