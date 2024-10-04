load('r_grid2_threats_wege_biomes.Rdata')
library(openxlsx)
library(dplyr)
library(raster)
library(ggplot2)
library(cowplot)
library(sf)
sf_use_s2(F)
wwf_ecoregions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')
wwf_ecoregions = st_transform(wwf_ecoregions,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
r_grid2$square_area = st_area(r_grid2)
units(r_grid2$square_area) = NULL
r_grid2_ecoregions = st_intersection(r_grid2,wwf_ecoregions)
r_grid2_ecoregions$area_intersection = st_area(r_grid2_ecoregions)
units(r_grid2_ecoregions$area_intersection) = NULL

r_grid2_ecoregions$percentage_area = r_grid2_ecoregions$area_intersection/r_grid2_ecoregions$square_area
colnames(r_grid2_ecoregions)
r_grid2$ecoregion = NA
ecoregion_list = unique(wwf_ecoregions$ECO_NAME)
for(i in seq_along(ecoregion_list)){
  print(i)
  ecoregion_list_temp = r_grid2_ecoregions[r_grid2_ecoregions$ECO_NAME == ecoregion_list[i] & 
                                             r_grid2_ecoregions$percentage_area>0.5,]$layer
  
  if(length(ecoregion_list_temp)>0){
    r_grid2[r_grid2$layer %in% ecoregion_list_temp,]$ecoregion = ecoregion_list[i]
  }
  
}

library(ggplot2)
# p_test = ggplot()+ geom_sf(data = r_grid2,aes(fill = ecoregion,col = ecoregion))+
#   theme_void()+
#   theme(legend.position = 'none')
#   
# 
# 
# ggsave(plot = p_test,filename = 'p_test.png')


region_guesser = function(na.cells, reference_regions, layer = 'layer'){
  temp_distance = st_distance(na.cells,reference_regions)
  
  final_df_nas = data.frame()
  
  for(i in seq_along(na.cells[[layer]])){
    print(na.cells[[layer]][i])
    temp = temp_distance[i,]
    which(temp == min(temp))
    units(temp)=NULL
    tempdf=cbind.data.frame(layer = na.cells[[layer]][i],region = which(temp == min(temp)),minimum_distance = min(temp))
    final_df_nas = rbind(final_df_nas,tempdf)
  }
  
  return(final_df_nas)
}

na.cells = r_grid2[is.na(r_grid2$ecoregion),]
df_ecoregions=region_guesser(na.cells = na.cells,reference_regions = wwf_ecoregions,layer = "layer")

# df_ecoregions = final_df_nas

df_subset = wwf_ecoregions[,c(1,2)]
st_geometry(df_subset) = NULL
df_subset$newid = 1:nrow(df_subset)
df_ecoregions = merge(df_ecoregions,df_subset,by.x = 'region',by.y = 'newid',all.x = T)
nrow(df_ecoregions)

df_ecoregions_filter1 = df_ecoregions[df_ecoregions$minimum_distance<250000,]

nrow(df_ecoregions_filter1)
nrow(df_ecoregions)

list_na_cells = sort(df_ecoregions_filter1$layer)
for(i in seq_along(list_na_cells)){
  print(i)
  temp_df = df_ecoregions_filter1[df_ecoregions_filter1$layer %in% list_na_cells[i],]
  name_to_use= temp_df[order(temp_df$minimum_distance,decreasing = F),]$ECO_NAME[1]
  
  r_grid2[r_grid2$layer%in% list_na_cells[i],]$ecoregion = name_to_use
}

# p_test = ggplot()+ geom_sf(data = r_grid2,aes(fill = ecoregion,col = ecoregion))+
#   theme_void()+
#   theme(legend.position = 'none')
# 
# unique(r_grid2$ecoregion)
# 
# ggsave(plot = p_test,filename = 'p_test2.png')

still_na = r_grid2[is.na(r_grid2$ecoregion),]$layer

df_ecoregions[df_ecoregions$layer %in% still_na,]

list_na_cells = sort(still_na)

for(i in seq_along(list_na_cells)){
  print(i)
  temp_df = df_ecoregions[df_ecoregions$layer %in% list_na_cells[i],]
  name_to_use= temp_df[order(temp_df$minimum_distance,decreasing = F),]$ECO_NAME[1]
  
  r_grid2[r_grid2$layer%in% list_na_cells[i],]$ecoregion = name_to_use
}

# p_test = ggplot()+ geom_sf(data = r_grid2,aes(fill = ecoregion,col = ecoregion))+
#   theme_void()+
#   theme(legend.position = 'bottom')
# 
# unique(r_grid2$ecoregion)
# 
# ggsave(plot = p_test,filename = 'p_test3.png')

list_eco = unique(r_grid2$ecoregion)



quantiles_wege_a  = r_grid2[['wege2_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_r  = r_grid2[['wege2_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_m  = r_grid2[['wege2_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_b  = r_grid2[['wege2_b']] %>%
  quantile(probs = 0.9,na.rm = T)

colnames(r_grid2)
#a
quantiles_wege_hunting_a  = r_grid2[['hunting_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_agriculture_a  = r_grid2[['agriculture_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_pollution_a  = r_grid2[['pollution_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_invasives_a  = r_grid2[['invasives_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_logging_a  = r_grid2[['logging_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_climate_change_a  = r_grid2[['climate_change_a']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_urbanization_a  = r_grid2[['urbanization_a']] %>%
  quantile(probs = 0.9,na.rm = T)
#r

quantiles_wege_hunting_r  = r_grid2[['hunting_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_rgriculture_r  = r_grid2[['agriculture_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_pollution_r  = r_grid2[['pollution_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_invasives_r  = r_grid2[['invasives_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_logging_r  = r_grid2[['logging_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_climate_change_r  = r_grid2[['climate_change_r']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_urbanization_r  = r_grid2[['urbanization_r']] %>%
  quantile(probs = 0.9,na.rm = T)

#m
quantiles_wege_hunting_m  = r_grid2[['hunting_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_mgriculture_m  = r_grid2[['agriculture_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_pollution_m  = r_grid2[['pollution_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_invasives_m  = r_grid2[['invasives_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_logging_m  = r_grid2[['logging_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_climate_change_m  = r_grid2[['climate_change_m']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_urbanization_m  = r_grid2[['urbanization_m']] %>%
  quantile(probs = 0.9,na.rm = T)

#b
quantiles_wege_hunting_b  = r_grid2[['hunting_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_bgriculture_b  = r_grid2[['agriculture_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_pollution_b  = r_grid2[['pollution_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_invasives_b  = r_grid2[['invasives_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_logging_b  = r_grid2[['logging_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_climate_change_b  = r_grid2[['climate_change_b']] %>%
  quantile(probs = 0.9,na.rm = T)
quantiles_wege_urbanization_b  = r_grid2[['urbanization_b']] %>%
  quantile(probs = 0.9,na.rm = T)

df_final = data.frame()


for(i in seq_along(list_eco)){
  print(i)
  
  ecoregion = list_eco[i]
  temp_df = r_grid2[r_grid2$ecoregion %in% ecoregion,]
  number_cells_ecoregion=nrow(temp_df)
  #a----
  hunting_a_temp=temp_df %>% filter(hunting_a>quantiles_wege_hunting_a & wege2_a>quantiles_wege_a) %>% nrow()
  hunting_a_temp = hunting_a_temp/number_cells_ecoregion
  
  logging_a_temp=temp_df %>% filter(logging_a>quantiles_wege_logging_a & wege2_a>quantiles_wege_a) %>% nrow()
  logging_a_temp = logging_a_temp/number_cells_ecoregion
  
  agriculture_a_temp=temp_df %>% filter(agriculture_a>quantiles_wege_agriculture_a & wege2_a>quantiles_wege_a) %>% nrow()
  agriculture_a_temp = agriculture_a_temp/number_cells_ecoregion
  
  pollution_a_temp=temp_df %>% filter(pollution_a>quantiles_wege_pollution_a & wege2_a>quantiles_wege_a) %>% nrow()
  pollution_a_temp = pollution_a_temp/number_cells_ecoregion
  
  invasives_a_temp=temp_df %>% filter(invasives_a>quantiles_wege_invasives_a & wege2_a>quantiles_wege_a) %>% nrow()
  invasives_a_temp = invasives_a_temp/number_cells_ecoregion
  
  climate_change_a_temp=temp_df %>% filter(climate_change_a>quantiles_wege_climate_change_a & wege2_a>quantiles_wege_a) %>% nrow()
  climate_change_a_temp = climate_change_a_temp/number_cells_ecoregion
  
  urbanization_a_temp=temp_df %>% filter(urbanization_a>quantiles_wege_urbanization_a & wege2_a>quantiles_wege_a) %>% nrow()
  urbanization_a_temp = urbanization_a_temp/number_cells_ecoregion
  
  any_threat_a_temp=temp_df %>% filter(c(hunting_a>quantiles_wege_hunting_a |
                                    logging_a>quantiles_wege_logging_a |
                                    agriculture_a>quantiles_wege_agriculture_a |
                                    pollution_a>quantiles_wege_pollution_a |
                                    invasives_a>quantiles_wege_invasives_a |
                                    climate_change_a>quantiles_wege_climate_change_a |
                                  urbanization_a>quantiles_wege_urbanization_a) 
                                  & wege2_a>quantiles_wege_a) %>% nrow()
  any_threat_a_temp =  any_threat_a_temp/number_cells_ecoregion

  #r----
  
  hunting_r_temp=temp_df %>% filter(hunting_r>quantiles_wege_hunting_r & wege2_r>quantiles_wege_r) %>% nrow()
  hunting_r_temp = hunting_r_temp/number_cells_ecoregion
  
  logging_r_temp=temp_df %>% filter(logging_r>quantiles_wege_logging_r & wege2_r>quantiles_wege_r) %>% nrow()
  logging_r_temp = logging_r_temp/number_cells_ecoregion
  
  agriculture_r_temp=temp_df %>% filter(agriculture_r>quantiles_wege_rgriculture_r & wege2_r>quantiles_wege_r) %>% nrow()
  agriculture_r_temp = agriculture_r_temp/number_cells_ecoregion
  
  pollution_r_temp=temp_df %>% filter(pollution_r>quantiles_wege_pollution_r & wege2_r>quantiles_wege_r) %>% nrow()
  pollution_r_temp = pollution_r_temp/number_cells_ecoregion
  
  invasives_r_temp=temp_df %>% filter(invasives_r>quantiles_wege_invasives_r & wege2_r>quantiles_wege_r) %>% nrow()
  invasives_r_temp = invasives_r_temp/number_cells_ecoregion
  
  climate_change_r_temp=temp_df %>% filter(climate_change_r>quantiles_wege_climate_change_r & wege2_r>quantiles_wege_r) %>% nrow()
  climate_change_r_temp = climate_change_r_temp/number_cells_ecoregion
  
  urbanization_r_temp=temp_df %>% filter(urbanization_r>quantiles_wege_urbanization_r & wege2_r>quantiles_wege_r) %>% nrow()
  urbanization_r_temp = urbanization_r_temp/number_cells_ecoregion
  
  any_threat_r_temp=temp_df %>% filter(c(hunting_r>quantiles_wege_hunting_r |
                                         logging_r>quantiles_wege_logging_r |
                                         agriculture_r>quantiles_wege_rgriculture_r |
                                         pollution_r>quantiles_wege_pollution_r |
                                         invasives_r>quantiles_wege_invasives_r |
                                         climate_change_r>quantiles_wege_climate_change_r |
                                         urbanization_r>quantiles_wege_urbanization_r) 
                                       & wege2_r>quantiles_wege_r) %>% nrow()
  any_threat_r_temp =  any_threat_r_temp/number_cells_ecoregion
  
  #m----
  hunting_m_temp=temp_df %>% filter(hunting_m>quantiles_wege_hunting_m & wege2_m>quantiles_wege_m) %>% nrow()
  hunting_m_temp = hunting_m_temp/number_cells_ecoregion
  
  logging_m_temp=temp_df %>% filter(logging_m>quantiles_wege_logging_m & wege2_m>quantiles_wege_m) %>% nrow()
  logging_m_temp = logging_m_temp/number_cells_ecoregion
  
  agriculture_m_temp=temp_df %>% filter(agriculture_m>quantiles_wege_mgriculture_m & wege2_m>quantiles_wege_m) %>% nrow()
  agriculture_m_temp = agriculture_m_temp/number_cells_ecoregion
  
  pollution_m_temp=temp_df %>% filter(pollution_m>quantiles_wege_pollution_m & wege2_m>quantiles_wege_m) %>% nrow()
  pollution_m_temp = pollution_m_temp/number_cells_ecoregion
  
  invasives_m_temp=temp_df %>% filter(invasives_m>quantiles_wege_invasives_m & wege2_m>quantiles_wege_m) %>% nrow()
  invasives_m_temp = invasives_m_temp/number_cells_ecoregion
  
  climate_change_m_temp=temp_df %>% filter(climate_change_m>quantiles_wege_climate_change_m & wege2_m>quantiles_wege_m) %>% nrow()
  climate_change_m_temp = climate_change_m_temp/number_cells_ecoregion
  
  urbanization_m_temp=temp_df %>% filter(urbanization_m>quantiles_wege_urbanization_m & wege2_m>quantiles_wege_m) %>% nrow()
  urbanization_m_temp = urbanization_m_temp/number_cells_ecoregion
  
  any_threat_m_temp=temp_df %>% filter(c(hunting_m>quantiles_wege_hunting_m |
                                         logging_m>quantiles_wege_logging_m |
                                         agriculture_m>quantiles_wege_mgriculture_m |
                                         pollution_m>quantiles_wege_pollution_m |
                                         invasives_m>quantiles_wege_invasives_m |
                                         climate_change_m>quantiles_wege_climate_change_m |
                                         urbanization_m>quantiles_wege_urbanization_m) 
                                       & wege2_m>quantiles_wege_m) %>% nrow()
  any_threat_m_temp =  any_threat_m_temp/number_cells_ecoregion
  
  #b----
  hunting_b_temp=temp_df %>% filter(hunting_b>quantiles_wege_hunting_b & wege2_b>quantiles_wege_b) %>% nrow()
  hunting_b_temp = hunting_b_temp/number_cells_ecoregion
  
  logging_b_temp=temp_df %>% filter(logging_b>quantiles_wege_logging_b & wege2_b>quantiles_wege_b) %>% nrow()
  logging_b_temp = logging_b_temp/number_cells_ecoregion
  
  agriculture_b_temp=temp_df %>% filter(agriculture_b>quantiles_wege_bgriculture_b & wege2_b>quantiles_wege_b) %>% nrow()
  agriculture_b_temp = agriculture_b_temp/number_cells_ecoregion
  
  pollution_b_temp=temp_df %>% filter(pollution_b>quantiles_wege_pollution_b & wege2_b>quantiles_wege_b) %>% nrow()
  pollution_b_temp = pollution_b_temp/number_cells_ecoregion
  
  invasives_b_temp=temp_df %>% filter(invasives_b>quantiles_wege_invasives_b & wege2_b>quantiles_wege_b) %>% nrow()
  invasives_b_temp = invasives_b_temp/number_cells_ecoregion
  
  climate_change_b_temp=temp_df %>% filter(climate_change_b>quantiles_wege_climate_change_b & wege2_b>quantiles_wege_b) %>% nrow()
  climate_change_b_temp = climate_change_b_temp/number_cells_ecoregion
  
  urbanization_b_temp=temp_df %>% filter(urbanization_b>quantiles_wege_urbanization_b & wege2_b>quantiles_wege_b) %>% nrow()
  urbanization_b_temp = urbanization_b_temp/number_cells_ecoregion
  
  any_threat_b_temp=temp_df %>% filter(hunting_b>quantiles_wege_hunting_b |
                                         logging_b>quantiles_wege_logging_b |
                                         agriculture_b>quantiles_wege_bgriculture_b |
                                         pollution_b>quantiles_wege_pollution_b |
                                         invasives_b>quantiles_wege_invasives_b |
                                         climate_change_b>quantiles_wege_climate_change_b |
                                         urbanization_b>quantiles_wege_urbanization_b 
                                       & wege2_b>quantiles_wege_b) %>% nrow()
  any_threat_b_temp =  any_threat_b_temp/number_cells_ecoregion
  
  any_threat_any_taxa_a=temp_df %>% filter(c(hunting_a>quantiles_wege_hunting_a |
                                         logging_a>quantiles_wege_logging_a |
                                         agriculture_a>quantiles_wege_agriculture_a |
                                         pollution_a>quantiles_wege_pollution_a |
                                         invasives_a>quantiles_wege_invasives_a |
                                         climate_change_a>quantiles_wege_climate_change_a |
                                         urbanization_a>quantiles_wege_urbanization_a) 
                                       & wege2_a>quantiles_wege_a)
  any_threat_any_taxa_a = any_threat_any_taxa_a$layer
  
  any_threat_any_taxa_r=temp_df %>% filter(c(hunting_r>quantiles_wege_hunting_r |
                                             logging_r>quantiles_wege_logging_r |
                                             agriculture_r>quantiles_wege_rgriculture_r |
                                             pollution_r>quantiles_wege_pollution_r |
                                             invasives_r>quantiles_wege_invasives_r |
                                             climate_change_r>quantiles_wege_climate_change_r |
                                             urbanization_r>quantiles_wege_urbanization_r) 
                                           & wege2_r>quantiles_wege_r)
  any_threat_any_taxa_r = any_threat_any_taxa_r$layer
  
  any_threat_any_taxa_m=temp_df %>% filter(c(hunting_m>quantiles_wege_hunting_m |
                                             logging_m>quantiles_wege_logging_m |
                                             agriculture_m>quantiles_wege_mgriculture_m |
                                             pollution_m>quantiles_wege_pollution_m |
                                             invasives_m>quantiles_wege_invasives_m |
                                             climate_change_m>quantiles_wege_climate_change_m |
                                             urbanization_m>quantiles_wege_urbanization_m) 
                                           & wege2_m>quantiles_wege_m)
  any_threat_any_taxa_m = any_threat_any_taxa_m$layer
  
  any_threat_any_taxa_b=temp_df %>% filter(c(hunting_b>quantiles_wege_hunting_b |
                                             logging_b>quantiles_wege_logging_b |
                                             agriculture_b>quantiles_wege_bgriculture_b |
                                             pollution_b>quantiles_wege_pollution_b |
                                             invasives_b>quantiles_wege_invasives_b |
                                             climate_change_b>quantiles_wege_climate_change_b |
                                             urbanization_b>quantiles_wege_urbanization_b) 
                                           & wege2_b>quantiles_wege_b)
  any_threat_any_taxa_b = any_threat_any_taxa_b$layer
  
  any_threat_any_taxa=length(unique(c(any_threat_any_taxa_a,any_threat_any_taxa_r,any_threat_any_taxa_m,any_threat_any_taxa_b)))/number_cells_ecoregion
  
  #----
  df_temp = cbind.data.frame(ecoregion,number_cells_ecoregion,
                             hunting_a_temp,logging_a_temp,agriculture_a_temp,pollution_a_temp,invasives_a_temp,climate_change_a_temp,urbanization_a_temp,any_threat_a_temp,
                             hunting_r_temp,logging_r_temp,agriculture_r_temp,pollution_r_temp,invasives_r_temp,climate_change_r_temp,urbanization_r_temp,any_threat_r_temp,
                             hunting_m_temp,logging_m_temp,agriculture_m_temp,pollution_m_temp,invasives_m_temp,climate_change_m_temp,urbanization_m_temp,any_threat_m_temp,
                             hunting_b_temp,logging_b_temp,agriculture_b_temp,pollution_b_temp,invasives_b_temp,climate_change_b_temp,urbanization_b_temp,any_threat_b_temp,
                             any_threat_any_taxa)
  
  df_final = rbind(df_final,df_temp)
}


wwf_ecoregions_merged = merge(wwf_ecoregions,df_final,by.x = 'ECO_NAME',by.y = 'ecoregion',all.x = T)

pal = colorRampPalette(hcl.colors(5,'RdYlBu'))
p_test = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = any_threat_any_taxa,col = any_threat_any_taxa),size = 0.1)+
            theme_void()+
  theme(panel.grid.major = element_line(colour = 'grey50',linewidth = 0.1),
        legend.position = 'right',
        legend.key.width = unit(0.5,'cm'),
        legend.key.height = unit(1,'cm'),
        plot.subtitle = element_text(size = 30))+
  scale_fill_gradientn('',colours = rev(pal(5)))+
  scale_colour_gradientn('',colours = rev(pal(5)))+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))
  



ggsave(plot = p_test,filename = 'p_main_mapv2.png',width = 5.5,height = 2.5,dpi = 2000)


library(RColorBrewer)


library(gridExtra)
library(png)
library(grid)

color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens")[2:8],
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds")[2:8],
  "Pollution" = brewer.pal(8,"Blues")[2:8],
  "Invasives" = brewer.pal(8,"Purples")[2:8],
  "Climate change" = brewer.pal(8,"Oranges")[2:8],
  "Urbanization" = brewer.pal(8,"RdPu")[2:8])

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




p1 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = hunting,col = hunting),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        legend.background = element_blank(),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'e')+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)


legend.title.size = 10
annotate.size = 3
size_legend_text = 7

theme_harith = theme(panel.grid.major = element_line(colour = 'grey50',linewidth = 0.1),
                     legend.position = 'right',
                     legend.key.width = unit(0.5,'cm'),
                     legend.key.height = unit(1,'cm'),
                     plot.subtitle = element_text(size = 30),
                     legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size))

colnames(wwf_ecoregions_merged)

#amphibians
p_a_logging = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = logging_a_temp,col = logging_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  labs(tag = 'a')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

  
p_a_agriculture = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = agriculture_a_temp,col = agriculture_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  labs(tag = 'b')+
  guides(fill = guide_legend(title.position = "right"),
         colour = guide_legend(title.position = "right"))+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_a_hunting = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = hunting_a_temp,col = hunting_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  labs(tag = 'c')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)

p_a_pollution = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = pollution_a_temp,col = pollution_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  labs(tag = 'd')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_a_invasives = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = invasives_a_temp,col = invasives_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  labs(tag = 'e')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_a_c_change = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = climate_change_a_temp,col = climate_change_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  labs(tag = 'f')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_a_urbanization = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = urbanization_a_temp,col = urbanization_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  labs(tag = 'g')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_a_any_threat = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = any_threat_a_temp,col = any_threat_a_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Any threat',size = annotate.size)+
  labs(tag = 'h')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))

grid_a = arrangeGrob(p_a_logging,p_a_agriculture,p_a_hunting,p_a_pollution,p_a_invasives,p_a_c_change,p_a_urbanization,p_a_any_threat,ncol = 2)

ggsave(plot = grid_a,filename = 'p_a_all.png',width = 9,height = 9)

#reptiles
p_r_logging = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = logging_r_temp,col = logging_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  labs(tag = 'a')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p_r_agriculture = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = agriculture_r_temp,col = agriculture_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  labs(tag = 'b')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_r_hunting = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = hunting_r_temp,col = hunting_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  labs(tag = 'c')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)

p_r_pollution = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = pollution_r_temp,col = pollution_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  labs(tag = 'd')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_r_invasives = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = invasives_r_temp,col = invasives_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  labs(tag = 'e')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+  
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_r_c_change = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = climate_change_r_temp,col = climate_change_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  labs(tag = 'f')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_r_urbanization = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = urbanization_r_temp,col = urbanization_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  labs(tag = 'g')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_r_any_threat = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = any_threat_r_temp,col = any_threat_r_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Any threat',size = annotate.size)+
  labs(tag = 'h')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))
grid_r = arrangeGrob(p_r_logging,p_r_agriculture,p_r_hunting,p_r_pollution,p_r_invasives,p_r_c_change,p_r_urbanization,p_r_any_threat,ncol = 2)

ggsave(plot = grid_r,filename = 'p_r_all.png',width = 9,height = 9)

#mammals  
p_m_logging = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = logging_m_temp,col = logging_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  labs(tag = 'a')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p_m_agriculture = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = agriculture_m_temp,col = agriculture_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  labs(tag = 'b')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_m_hunting = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = hunting_m_temp,col = hunting_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  labs(tag = 'c')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)

p_m_pollution = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = pollution_m_temp,col = pollution_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  labs(tag = 'd')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_m_invasives = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = invasives_m_temp,col = invasives_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  labs(tag = 'e')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_m_c_change = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = climate_change_m_temp,col = climate_change_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  labs(tag = 'f')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_m_urbanization = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = urbanization_m_temp,col = urbanization_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  labs(tag = 'g')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_m_any_threat = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = any_threat_m_temp,col = any_threat_m_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Any threat',size = annotate.size)+
  labs(tag = 'h')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))
grid_m = arrangeGrob(p_m_logging,p_m_agriculture,p_m_hunting,p_m_pollution,p_m_invasives,p_m_c_change,p_m_urbanization,p_m_any_threat,ncol = 2)

ggsave(plot = grid_m,filename = 'p_m_all.png',width = 9,height = 9)
#birds
p_b_logging = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = logging_b_temp,col = logging_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
  labs(tag = 'a')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p_b_agriculture = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = agriculture_b_temp,col = agriculture_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
  labs(tag = 'b')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_b_hunting = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = hunting_b_temp,col = hunting_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Hunting,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Hunting',size = annotate.size)+
  labs(tag = 'c')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(hunting_img, xmin=-17182470, xmax=-2000000, ymin=-8853445, ymax= -3700000)

p_b_pollution = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = pollution_b_temp,col = pollution_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
  labs(tag = 'd')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_b_invasives = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = invasives_b_temp,col = invasives_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
  labs(tag = 'e')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_b_c_change = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = climate_change_b_temp,col = climate_change_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Climate change',size = annotate.size)+
  labs(tag = 'f')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p_b_urbanization = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = urbanization_b_temp,col = urbanization_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  labs(tag = 'g')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))+  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p_b_any_threat = ggplot()+
  geom_sf(data = wwf_ecoregions_merged,aes(fill = any_threat_b_temp,col = any_threat_b_temp),size = 0.1)+
  theme_void()+
  theme_harith+
  scale_fill_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Conservation priority',colours = rev(pal(5)),na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Any threat',size = annotate.size)+
  labs(tag = 'h')+
  guides(fill = guide_legend(title.position = "right",reverse = T),
         colour = guide_legend(title.position = "right",reverse = T))
grid_b = arrangeGrob(p_b_logging,p_b_agriculture,p_b_hunting,p_b_pollution,p_b_invasives,p_b_c_change,p_b_urbanization,p_b_any_threat,ncol = 2)

ggsave(plot = grid_b,filename = 'p_b_all.png',width = 9,height = 9)

save(wwf_ecoregions_merged,file = 'wwf_ecoregions_merged.Rdata')


