library(openxlsx)
library(RColorBrewer)
color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens"),
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds"),
  "Pollution" = brewer.pal(8,"Blues"),
  "Invasives" = brewer.pal(8,"Purples"),
  "Climate change" = brewer.pal(8,"Oranges"),
  "Urbanization" = brewer.pal(8,"RdPu"))

list_dfs3 = read.xlsx('list_dfs3.xlsx',sheet = 1)

list_cells_9 = list_dfs3[list_dfs3$quantile==9,]$layer
list_cells_8 = list_dfs3[list_dfs3$quantile==8,]$layer

list_cells_8 = setdiff(list_cells_8,list_cells_9)

plot(r_grid2[r_grid2$layer %in% list_cells_8,]$geometry)
plot(r_grid2[r_grid2$layer %in% list_cells_9,]$geometry)

groups = c('_a','_r','_m','_b')
logging = c(paste0('logging',groups))
pollution = c(paste0('pollution',groups))
invasives = c(paste0('invasives',groups))
hunting = c(paste0('hunting',groups))
agriculture = c(paste0('agriculture',groups))
climate_change = c(paste0('climate_change',groups))
urbanization = c(paste0('urbanization',groups))


list_logging = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% logging,]$layer)
list_logging_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% logging,]$layer)
list_logging_8 = setdiff(list_logging_8,list_logging)



list_pollution = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% pollution,]$layer)
list_invasives = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% invasives,]$layer)
list_hunting = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% hunting,]$layer)
list_agriculture = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% agriculture,]$layer)
list_climate_change = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% climate_change,]$layer)
list_urbanization = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% urbanization,]$layer)

quantiles  = r_grid2[['wege2_a']] %>%
  quantile(probs = seq(0.1, 1, length.out = 10),na.rm = T)


cells_wege_quantiles_9_a = r_grid2[r_grid2$wege2_a>quantiles[9],]$layer
cells_wege_quantiles_8_a = r_grid2[r_grid2$wege2_a>quantiles[8],]$layer
cells_wege_quantiles_8_a = setdiff(cells_wege_quantiles_8_a,cells_wege_quantiles_9_a)

quantiles  = r_grid2[['wege2_r']] %>%
  quantile(probs = seq(0.1, 1, length.out = 10),na.rm = T)


cells_wege_quantiles_9_r = r_grid2[r_grid2$wege2_r>quantiles[9],]$layer
cells_wege_quantiles_8_r = r_grid2[r_grid2$wege2_r>quantiles[8],]$layer
cells_wege_quantiles_8_r = setdiff(cells_wege_quantiles_8_r,cells_wege_quantiles_9_r)

quantiles  = r_grid2[['wege2_m']] %>%
  quantile(probs = seq(0.1, 1, length.out = 10),na.rm = T)

cells_wege_quantiles_9_m = r_grid2[r_grid2$wege2_m>quantiles[9],]$layer
cells_wege_quantiles_8_m = r_grid2[r_grid2$wege2_m>quantiles[8],]$layer
cells_wege_quantiles_8_m = setdiff(cells_wege_quantiles_8_m,cells_wege_quantiles_9_m)

quantiles  = r_grid2[['wege2_b']] %>%
  quantile(probs = seq(0.1, 1, length.out = 10),na.rm = T)

cells_wege_quantiles_9_b = r_grid2[r_grid2$wege2_b>quantiles[9],]$layer
cells_wege_quantiles_8_b = r_grid2[r_grid2$wege2_b>quantiles[8],]$layer
cells_wege_quantiles_8_b = setdiff(cells_wege_quantiles_8_b,cells_wege_quantiles_9_b)

plot(r_grid2[r_grid2$layer %in% cells_wege_quantiles_9_a,]$geometry)
plot(r_grid2[r_grid2$layer %in% cells_wege_quantiles_9_m,]$geometry)



list_logging = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% logging,]$layer)
list_logging_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% logging,]$layer)
list_logging_8 = setdiff(list_logging_8,list_logging)
r_grid2_threat_maps = r_grid2
r_grid2_threat_maps$plotting = NA
r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_logging,]$plotting = 9
r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_logging_8,]$plotting = 8


r_grid[is.na(r_grid$agriculture_a_hot),]$agriculture_a_hot = 0
r_grid[is.na(r_grid$logging_a_hot),]$logging_a_hot = 0
r_grid[is.na(r_grid$hunting_a_hot),]$hunting_a_hot = 0
r_grid[is.na(r_grid$pollution_a_hot),]$pollution_a_hot = 0
r_grid[is.na(r_grid$climate_change_a_hot),]$climate_change_a_hot = 0
r_grid[is.na(r_grid$invasives_a_hot),]$invasives_a_hot = 0
r_grid[is.na(r_grid$urbanization_a_hot),]$urbanization_a_hot = 0

r_grid[is.na(r_grid$agriculture_r_hot),]$agriculture_r_hot = 0
r_grid[is.na(r_grid$logging_r_hot),]$logging_r_hot = 0
r_grid[is.na(r_grid$hunting_r_hot),]$hunting_r_hot = 0
r_grid[is.na(r_grid$pollution_r_hot),]$pollution_r_hot = 0
r_grid[is.na(r_grid$climate_change_r_hot),]$climate_change_r_hot = 0
r_grid[is.na(r_grid$invasives_r_hot),]$invasives_r_hot = 0
r_grid[is.na(r_grid$urbanization_r_hot),]$urbanization_r_hot = 0

r_grid[is.na(r_grid$agriculture_m_hot),]$agriculture_m_hot = 0
r_grid[is.na(r_grid$logging_m_hot),]$logging_m_hot = 0
r_grid[is.na(r_grid$hunting_m_hot),]$hunting_m_hot = 0
r_grid[is.na(r_grid$pollution_m_hot),]$pollution_m_hot = 0
r_grid[is.na(r_grid$climate_change_m_hot),]$climate_change_m_hot = 0
r_grid[is.na(r_grid$invasives_m_hot),]$invasives_m_hot = 0
r_grid[is.na(r_grid$urbanization_m_hot),]$urbanization_m_hot = 0

r_grid[is.na(r_grid$agriculture_b_hot),]$agriculture_b_hot = 0
r_grid[is.na(r_grid$logging_b_hot),]$logging_b_hot = 0
r_grid[is.na(r_grid$hunting_b_hot),]$hunting_b_hot = 0
r_grid[is.na(r_grid$pollution_b_hot),]$pollution_b_hot = 0
r_grid[is.na(r_grid$climate_change_b_hot),]$climate_change_b_hot = 0
r_grid[is.na(r_grid$invasives_b_hot),]$invasives_b_hot = 0
r_grid[is.na(r_grid$urbanization_b_hot),]$urbanization_b_hot = 0


r_grid$cum_agriculture = r_grid$agriculture_a_hot+
  r_grid$agriculture_r_hot+
  r_grid$agriculture_m_hot+
  r_grid$agriculture_b_hot

r_grid$cum_logging = r_grid$logging_a_hot+
  r_grid$logging_r_hot+
  r_grid$logging_m_hot+
  r_grid$logging_b_hot

r_grid$cum_hunting = r_grid$hunting_a_hot+
  r_grid$hunting_r_hot+
  r_grid$hunting_m_hot+
  r_grid$hunting_b_hot


r_grid$cum_pollution = r_grid$pollution_a_hot+
  r_grid$pollution_r_hot+
  r_grid$pollution_m_hot+
  r_grid$pollution_b_hot

r_grid$cum_invasives = r_grid$invasives_a_hot+
  r_grid$invasives_r_hot+
  r_grid$invasives_m_hot+
  r_grid$invasives_b_hot

r_grid$cum_climate_change = r_grid$climate_change_a_hot+
  r_grid$climate_change_r_hot+
  r_grid$climate_change_m_hot+
  r_grid$climate_change_b_hot

r_grid$cum_urbanization = r_grid$urbanization_a_hot+
  r_grid$urbanization_r_hot+
  r_grid$urbanization_m_hot+
  r_grid$urbanization_b_hot

#----
# 
# 
# table(r_grid$cum_urbanization)
# 
# 
# p_logging = ggplot()+
#   geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Logging[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Logging[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'a')
# 
# list_agriculture = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% agriculture,]$layer)
# list_agriculture_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% agriculture,]$layer)
# list_agriculture_8 = setdiff(list_agriculture_8,list_agriculture)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_agriculture,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_agriculture_8,]$plotting = 8
# 
# p_agriculture = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Logging[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Logging[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'b')
# 
# list_hunting = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% hunting,]$layer)
# list_hunting_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% hunting,]$layer)
# list_hunting_8 = setdiff(list_hunting_8,list_hunting)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_hunting,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_hunting_8,]$plotting = 8
# 
# p_hunting = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Hunting[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Hunting[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'c')
# 
# list_pollution = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% pollution,]$layer)
# list_pollution_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% pollution,]$layer)
# list_pollution_8 = setdiff(list_pollution_8,list_pollution)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_pollution,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_pollution_8,]$plotting = 8
# 
# p_pollution = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Pollution[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Pollution[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'd')
# 
# list_invasives = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% invasives,]$layer)
# list_invasives_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% invasives,]$layer)
# list_invasives_8 = setdiff(list_invasives_8,list_invasives)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_invasives,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_invasives_8,]$plotting = 8
# 
# p_invasives = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Invasives[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Invasives[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'e')
# 
# list_climate_change = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% climate_change,]$layer)
# list_climate_change_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% climate_change,]$layer)
# list_climate_change_8 = setdiff(list_climate_change_8,list_climate_change)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_logging,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_logging_8,]$plotting = 8
# 
# p_climate_change = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$`Climate change`[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$`Climate change`[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'f')
# 
# list_urbanization = unique(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% urbanization,]$layer)
# list_urbanization_8 = unique(list_dfs3[list_dfs3$quantile == 8 & list_dfs3$threat%in% urbanization,]$layer)
# list_urbanization_8 = setdiff(list_urbanization_8,list_urbanization)
# r_grid2_threat_maps = r_grid2
# r_grid2_threat_maps$plotting = NA
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_urbanization,]$plotting = 9
# r_grid2_threat_maps[r_grid2_threat_maps$layer %in% list_urbanization_8,]$plotting = 8
# 
# p_urbanization = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95')+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(plotting),col = as.factor(plotting)),col = NA)+
#   theme_void()+
#   scale_fill_manual('',values = color.schemes$Urbanization[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   scale_colour_manual('',values = color.schemes$Urbanization[c(3,8)],na.value = 'grey95',na.translate=FALSE)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = c(0.9,0.9),
#         legend.direction = "vertical",
#         legend.key.width = unit(0.25, "cm"),
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'g')
# # ggplot()+
# #   geom_sf(data = r_grid2_threat_maps,aes(col = as.factor(plotting)))
# 
# p_all = arrangeGrob(p_logging,p_agriculture,p_hunting,p_pollution,p_invasives,p_climate_change,p_urbanization,ncol=2)
# ggsave(filename = 'threats_combined_quantiles.png',plot = p_all,dpi = 1000,width = 6,height = 6)
# 
# 
# 
# df_logging = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% logging,]$layer))
# df_pollution = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% pollution,]$layer))
# df_invasives = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% invasives,]$layer))
# df_hunting = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% hunting,]$layer))
# df_agriculture = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% agriculture,]$layer))
# df_climate_change = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% climate_change,]$layer))
# df_urbanization = as.data.frame(table(list_dfs3[list_dfs3$quantile == 9 & list_dfs3$threat%in% urbanization,]$layer))
# 
# 
# df_layer_count = as.data.frame(table(c(list_logging,list_pollution,list_invasives,list_hunting,list_agriculture,list_climate_change,list_urbanization)))
# 
#-----
library(gridExtra)
library(png)
library(grid)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/cat.png', native = TRUE)
cat_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/postdoc_KU_paper_2/chainsaw_raw_simple_fixed.png', native = TRUE)
saw_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/factory.png', native = TRUE)
factory_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/wolf-icon-png-2858.png', native = TRUE)
hunting_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/agriculture.png', native = TRUE)
agriculture_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/climate_change.png', native = TRUE)
climate_c_img <- rasterGrob(img, interpolate=TRUE)

img <- readPNG('/Users/gdt366/Dropbox/Copenhagen_postdoc/urbanization2.png', native = TRUE)
urbanization_img <- rasterGrob(img, interpolate=TRUE)


pal_qualitative = c('#a41623','#469aaf','#ffa62b','#272727')

#----

library(cowplot)

quantile9_colour = 'grey60'

decile9_grey_cells_a = r_grid[r_grid$logging_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$logging_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$logging_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$logging_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))

p_logging = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = quantile9_colour,col = quantile9_colour,linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_logging %in% c(1,2,3,4),],aes(fill = as.factor(cum_logging),col = as.factor(cum_logging)),linewidth = 0)+
  theme_void()+
  # scale_fill_manual('',values = color.schemes$Logging[c(3,5,6,8)])+
  # scale_colour_manual('',values = color.schemes$Logging[c(3,5,6,8)])+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = c(0.9,0.9),
        legend.direction = "vertical",
        legend.key.width = unit(0.25, "cm"),
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)



stacked_bar = as.data.frame(table(r_grid[r_grid$cum_logging>0,]$cum_logging))
logging_cells = sum(stacked_bar$Freq)
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))
p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(pal_qualitative))+
  theme(legend.position = 'none')

p_logging <-
  ggdraw() +
  draw_plot(p_logging) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)


decile9_grey_cells_a = r_grid[r_grid$agriculture_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$agriculture_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$agriculture_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$agriculture_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))


p_agriculture = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = quantile9_colour,col = quantile9_colour,linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_agriculture %in% c(1,2,3,4),],aes(fill = as.factor(cum_agriculture),col = as.factor(cum_agriculture)),linewidth = 0)+
  theme_void()+
  #scale_fill_manual('',values = color.schemes$Agriculture[c(2,3,4,5)])+
  #scale_colour_manual('',values = color.schemes$Agriculture[c(2,3,4,5)])+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'b')+
  annotation_custom(agriculture_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_agriculture>0,]$cum_agriculture))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_agriculture <-
  ggdraw() +
  draw_plot(p_agriculture) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)

decile9_grey_cells_a = r_grid[r_grid$hunting_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$hunting_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$hunting_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$hunting_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))

p_hunting = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = quantile9_colour,col = quantile9_colour,linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_hunting %in% c(1,2,3,4),],aes(fill = as.factor(cum_hunting),col = as.factor(cum_hunting)),linewidth = 0)+
  theme_void()+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'c')+
  annotation_custom(hunting_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_hunting>0,]$cum_hunting))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_hunting <-
  ggdraw() +
  draw_plot(p_hunting) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)

decile9_grey_cells_a = r_grid[r_grid$pollution_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$pollution_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$pollution_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$pollution_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))


p_pollution = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = 'grey60',col = 'grey60',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_pollution %in% c(1,2,3,4),],aes(fill = as.factor(cum_pollution),col = as.factor(cum_pollution)),linewidth = 0)+
  theme_void()+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'd')+
  annotation_custom(factory_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_pollution>0,]$cum_pollution))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_pollution <-
  ggdraw() +
  draw_plot(p_pollution) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)




decile9_grey_cells_a = r_grid[r_grid$invasives_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$invasives_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$invasives_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$invasives_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))


p_invasives = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = 'grey60',col = 'grey60',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_invasives %in% c(1,2,3,4),],aes(fill = as.factor(cum_invasives),col = as.factor(cum_invasives)),linewidth = 0)+
  theme_void()+
  # scale_fill_manual('',values = color.schemes$Invasives[c(3,5,6,8)])+
  # scale_colour_manual('',values = color.schemes$Invasives[c(3,5,6,8)])+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'e')+
  annotation_custom(cat_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_invasives>0,]$cum_invasives))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_invasives <-
  ggdraw() +
  draw_plot(p_invasives) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)

decile9_grey_cells_a = r_grid[r_grid$climate_change_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$climate_change_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$climate_change_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$climate_change_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))


p_climate_change = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = quantile9_colour,col = quantile9_colour,linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_climate_change %in% c(1,2,3,4),],aes(fill = as.factor(cum_climate_change),col = as.factor(cum_climate_change)),linewidth = 0)+
  theme_void()+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "horizontal",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_climate_change>0,]$cum_climate_change))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_climate_change <-
  ggdraw() +
  draw_plot(p_climate_change) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)


decile9_grey_cells_a = r_grid[r_grid$urbanization_a_d %in% 9 & r_grid$wege2_a_d %in% 10,]$layer
decile9_grey_cells_r = r_grid[r_grid$urbanization_r_d %in% 9 & r_grid$wege2_r_d %in% 10,]$layer
decile9_grey_cells_m = r_grid[r_grid$urbanization_m_d %in% 9 & r_grid$wege2_m_d %in% 10,]$layer
decile9_grey_cells_b = r_grid[r_grid$urbanization_b_d %in% 9 & r_grid$wege2_b_d %in% 10,]$layer

decile9_grey_cells = c(unique(decile9_grey_cells_a,decile9_grey_cells_r,decile9_grey_cells_m,decile9_grey_cells_b))


p_urbanization = ggplot()+
  geom_sf(data = r_grid[r_grid$land %in% 1,],bg = 'grey95',col = 'grey95')+
  geom_sf(data = r_grid[r_grid$layer %in% decile9_grey_cells,],bg = quantile9_colour,col = quantile9_colour,linewidth = 0)+
  geom_sf(data = r_grid[r_grid$cum_urbanization %in% c(1,2,3,4),],aes(fill = as.factor(cum_urbanization),col = as.factor(cum_urbanization)),linewidth = 0)+
  theme_void()+
  # scale_fill_manual('',values = color.schemes$Urbanization[c(3,5,6,8)])+
  # scale_colour_manual('',values = color.schemes$Urbanization[c(3,5,6,8)])+
  scale_fill_manual('',values = pal_qualitative)+
  scale_colour_manual('',values = pal_qualitative)+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        legend.position = 'none',
        legend.direction = "vertical",
        legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.25, "cm"))+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17982470, xmax=-13082470, ymin=4252467, ymax= 8952467)

stacked_bar = as.data.frame(table(r_grid[r_grid$cum_urbanization>0,]$cum_urbanization))

stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))

p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1),linewidth = 0) + 
  geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = rev(c(head(pal_qualitative,length(unique(stacked_bar$Var1))))))+
  theme(legend.position = 'none')+
  scale_x_continuous(limits = c(0,logging_cells))

p_urbanization <-
  ggdraw() +
  draw_plot(p_urbanization) +
  draw_plot(p_stacked_bar, x = 0, y = 0, width = 1, height = .05)



df_a = cbind.data.frame(taxon = 'amphibians',agriculture = nrow(r_grid[r_grid$agriculture_a_hot%in%1,]),
                        logging = nrow(r_grid[r_grid$logging_a_hot%in%1,]),
                        hunting = nrow(r_grid[r_grid$hunting_a_hot%in%1,]),
                        pollution = nrow(r_grid[r_grid$pollution_a_hot%in%1,]),
                        invasives = nrow(r_grid[r_grid$invasives_a_hot%in%1,]),
                        climate_change = nrow(r_grid[r_grid$climate_change_a_hot%in%1,]),
                        urbanization = nrow(r_grid[r_grid$urbanization_a_hot%in%1,]))

df_r = cbind.data.frame(taxon = 'reptiles',agriculture = nrow(r_grid[r_grid$agriculture_r_hot%in%1,]),
                        logging = nrow(r_grid[r_grid$logging_r_hot%in%1,]),
                        hunting = nrow(r_grid[r_grid$hunting_r_hot%in%1,]),
                        pollution = nrow(r_grid[r_grid$pollution_r_hot%in%1,]),
                        invasives = nrow(r_grid[r_grid$invasives_r_hot%in%1,]),
                        climate_change = nrow(r_grid[r_grid$climate_change_r_hot%in%1,]),
                        urbanization = nrow(r_grid[r_grid$urbanization_r_hot%in%1,]))

df_m = cbind.data.frame(taxon = 'mammals',agriculture = nrow(r_grid[r_grid$agriculture_m_hot%in%1,]),
                        logging = nrow(r_grid[r_grid$logging_m_hot%in%1,]),
                        hunting = nrow(r_grid[r_grid$hunting_m_hot%in%1,]),
                        pollution = nrow(r_grid[r_grid$pollution_m_hot%in%1,]),
                        invasives = nrow(r_grid[r_grid$invasives_m_hot%in%1,]),
                        climate_change = nrow(r_grid[r_grid$climate_change_m_hot%in%1,]),
                        urbanization = nrow(r_grid[r_grid$urbanization_m_hot%in%1,]))

df_b = cbind.data.frame(taxon = 'birds',agriculture = nrow(r_grid[r_grid$agriculture_b_hot%in%1,]),
                        logging = nrow(r_grid[r_grid$logging_b_hot%in%1,]),
                        hunting = nrow(r_grid[r_grid$hunting_b_hot%in%1,]),
                        pollution = nrow(r_grid[r_grid$pollution_b_hot%in%1,]),
                        invasives = nrow(r_grid[r_grid$invasives_b_hot%in%1,]),
                        climate_change = nrow(r_grid[r_grid$climate_change_b_hot%in%1,]),
                        urbanization = nrow(r_grid[r_grid$urbanization_b_hot%in%1,]))

                        

df_all_taxa_grids = rbind(df_a,df_r,df_m,df_b)

df_all_taxa_grids = melt(df_all_taxa_grids)

df_all_taxa_grids$taxon = factor(df_all_taxa_grids$taxon, levels = rev(c('amphibians','birds','mammals','reptiles')))
df_all_taxa_grids$variable = factor(df_all_taxa_grids$variable, levels = c('urbanization','climate_change','invasives','hunting','pollution','agriculture','logging'))

theme_harith = theme(axis.title.y = element_blank(),
                     axis.title.x = element_text(family = myfont,size = 7),
                     panel.background = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position = c(0.75,0.4),
                     axis.text = element_text(colour = 'black',family = myfont,size = 7),
                     axis.text.y = element_text(colour = 'black',hjust = 0.9,size = 8),
                     legend.title = element_blank(),
                     legend.text = element_text(family = myfont,size = 7),
                     legend.key.height = unit(0.25, "cm"),
                     axis.line = element_line())

p_bar_chart_all = ggplot()+
  geom_bar(data = df_all_taxa_grids,aes(x = variable,y = value, fill = taxon),position = 'dodge',stat = 'identity')+
  coord_flip()+
  theme_harith+
  ylab('Number of grids')+
  guides(fill = guide_legend(reverse = T))+
  scale_y_continuous(expand = c(0.02,0.01),limits = c(0,3800),breaks = c(0,1000,2000,3000,3700))+
  scale_x_discrete(labels = c('Urbanization','Climate change','Invasives','Hunting','Pollution','Agriculture','Logging'))+
  scale_fill_manual(values = c('#a6611a','#dfc27d','#80cdc1','#018571'),labels = rev(c('Amphibians','Birds','Mammals','Reptiles')))+
  labs(tag = 'h')+
  theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))




#----
# r_grid2_threat_maps = r_grid2
# df_wege = as.data.frame(table(c(cells_wege_quantiles_9_a,cells_wege_quantiles_9_r,cells_wege_quantiles_9_m,cells_wege_quantiles_9_b)))
# r_grid2_threat_maps = merge(r_grid2_threat_maps,df_wege,by.x = 'layer','Var1')
# list_wege_8 = unique(c(c(cells_wege_quantiles_8_a,cells_wege_quantiles_8_r,cells_wege_quantiles_8_m,cells_wege_quantiles_8_b)))
# p_wege = ggplot()+
#   geom_sf(data = r_grid2[r_grid2$land_areas>0,],bg = 'grey95',col = 'grey95',linewidth = 0)+
#   geom_sf(data = r_grid2[r_grid2$layer %in% list_wege_8,],bg = 'grey70',col = 'grey70',linewidth = 0)+
#   geom_sf(data = r_grid2_threat_maps,aes(fill = as.factor(Freq),col = as.factor(Freq)),linewidth = 0)+
#   theme_void()+
#   # scale_fill_manual('',values = color.schemes$Urbanization[c(3,5,6,8)])+
#   # scale_colour_manual('',values = color.schemes$Urbanization[c(3,5,6,8)])+
#   scale_fill_manual('',values = pal_qualitative)+
#   scale_colour_manual('',values = pal_qualitative)+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
#         legend.position = 'none',
#         legend.direction = "vertical",
#         legend.text = element_text(family = myfont,hjust = 0.5,size = 6),
#         legend.key.width = unit(0.25, "cm"),
#         legend.key.height = unit(0.25, "cm"))+
#   theme(plot.tag = element_text(hjust = 0,vjust = 1,family = myfont))+
#   labs(tag = 'h')+
#   annotate(geom = 'text',x = -17200600,y = 7852467,label = 'WEGE',size = 4,family = myfont)
# 
# stacked_bar = as.data.frame(table(df_wege$Freq))
# stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
# #stacked_bar$Var1 = factor(x = stacked_bar$Var1,levels = rev(unique(sort(stacked_bar$Var1))))
# # p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
# #   geom_bar(position = "stack",stat = 'identity',col = 'black',linewidth = 0.1)+
# #   #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
# #   theme_void()+
# #   theme(plot.margin = unit(c(0,0.2,0,0), "cm"))+
# #   scale_fill_manual(values = rev(pal_qualitative))+
# #   theme(legend.position = 'none')+
# #   geom_text(aes(x = sum(stacked_bar$Freq),y = '1',label = '%'),size = 2,hjust = -1,family = myfont)
# 
# 
# p_stacked_bar = ggplot(stacked_bar, aes(x=1, y=Freq, fill = Var1)) +
#   geom_bar(stat="identity", width=1) +
#   coord_polar("y", start=0)+
#   theme_void()+
#   xlim(.2,2.5) +
#   scale_fill_manual(values = pal_qualitative)+
#   geom_text(aes(label = paste0(round(percentage,0), "%")), 
#             position = position_stack(vjust=0.5),col = 'white',
#             size = 2.8,family = myfont)+
#   theme(legend.position = 'none')+
#   theme(plot.margin = unit(c(0,0,0,0), "cm"))
# 
# 
# 
# 
# p_wege <-
#   ggdraw() +
#   draw_plot(p_wege) +
#   draw_plot(p_stacked_bar, x = 0.4, y = 0.4, width = 1, height = 0.85)
# 
# 

#----


p_all = arrangeGrob(p_logging,p_agriculture,p_hunting,p_pollution,p_invasives,p_climate_change,p_urbanization,p_bar_chart_all,ncol=2)

ggsave(filename = 'threats_combined_run2.png',plot = p_all,dpi = 2000,width = 6,height = 6)



#----

stacked_bar = as.data.frame(table(df_climate_change$Freq))
stacked_bar$percentage = 100*stacked_bar$Freq/sum(stacked_bar$Freq)
p_stacked_bar = ggplot(data=stacked_bar, aes(y = '1', x = Freq,fill = Var1)) + 
  geom_bar(position = "fill",stat = 'identity',col = 'black',linewidth = 0.1)+
  #geom_text(aes(y = '1', x = c(Freq/2),label=paste0(round(percentage,1),'%')),position = 'fill',vjust = -1,size=3)+
  theme_void()+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  scale_fill_manual(values = pal_qualitative)+
  theme(legend.position = 'none')+
  scale_x_reverse()

plot.with.inset <-
  ggdraw() +
  draw_plot(p_urbanization) +
  draw_plot(stacked_bar, x = 0, y = 0.7, width = 1, height = .025)



geom_histogram(position = "stack", binwidth=1,aes(group = Freq,fill = Freq))


