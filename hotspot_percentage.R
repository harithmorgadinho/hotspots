load('r_grid_all_fixed_17_Sep_2024.Rdata')
load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_DEC4.Rdata')
library(dplyr)
colnames(r_grid)
r_grid$biomes = r_grid_soc_eco$BIOME_NAME
r_grid$REALM = r_grid_soc_eco$REALM
r_grid$pop = r_grid_soc_eco$population_counts
r_grid$HOTSPOTS = r_grid_soc_eco$HOTSPOTS

df_global_hotspots_hotspots = r_grid %>% filter(hotspots_all %in% 1 & land %in%1) %>% group_by(HOTSPOTS) %>% summarise(hotspot_counts = n())
st_geometry(df_global_hotspots_hotspots) = NULL

df_global_hotspots_hotspots_total = r_grid %>% filter(land %in%1) %>% group_by(HOTSPOTS) %>% summarise(hotspot_counts_total = n())
st_geometry(df_global_hotspots_hotspots_total) = NULL

df_hotspots_percentage = merge(df_global_hotspots_hotspots,df_global_hotspots_hotspots_total, by = 'HOTSPOTS')

df_hotspots_percentage$percentage = round(100*(df_hotspots_percentage$hotspot_counts/df_hotspots_percentage$hotspot_counts_total),1)

df_hotspots_percentage = df_hotspots_percentage[order(df_hotspots_percentage$percentage,decreasing = T),]

library(openxlsx)
write.xlsx(df_hotspots_percentage,file = 'df_hotspots_percentage_Sep_23.xlsx')

100*df_hotspots_percentage[is.na(df_hotspots_percentage$HOTSPOTS),]$hotspot_counts/sum(na.omit(df_hotspots_percentage)$hotspot_counts)



df_hotspots_outside = r_grid %>% filter(hotspots_all %in% 1 & land %in%1 & is.na(HOTSPOTS)) %>% group_by(REALM,biomes) %>% summarise(hotspot_counts = n())
st_geometry(df_hotspots_outside) = NULL

df_hotspots_outside = df_hotspots_outside[order(df_hotspots_outside$hotspot_counts,decreasing = T),]

write.xlsx(df_hotspots_outside,file = 'df_hotspots_outside_27_sep.xlsx')

df_hotspots_outside = r_grid %>% filter(hotspots_all %in% 1 & land %in%1 & is.na(HOTSPOTS)) %>% group_by(REALM) %>% summarise(hotspot_counts = n())
st_geometry(df_hotspots_outside) = NULL

df_hotspots_outside = df_hotspots_outside[order(df_hotspots_outside$hotspot_counts,decreasing = T),]

387/sum(df_hotspots_outside$hotspot_counts)
356/sum(df_hotspots_outside$hotspot_counts)


ggplot()+
  geom_sf(data = r_grid[is.na(r_grid$REALM) & is.na(r_grid$HOTSPOTS) & r_grid$hotspots_all %in% 1,],aes(fill = biomes,col = biomes))








