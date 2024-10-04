library(sf)
sf_use_s2(F)
library(raster)
realm_biomes = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')


realm_biomes[realm_biomes$REALM == 8,]
unique(realm_biomes$REALM)

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()


r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

realm_biomes = st_transform(realm_biomes,  
                            crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

colnames(realm_biomes)
realm_biomes$REALM = as.factor(realm_biomes$REALM)
realm_raster = rasterize(realm_biomes,r,field = 'REALM',background=0)
plot(realm_raster)

realm_biomes$BIOME_NUM = as.factor(realm_biomes$BIOME_NUM)
realm_BIOME_NUM = rasterize(realm_biomes,r,field = 'BIOME_NUM',background=0)
plot(realm_BIOME_NUM)

realm_BIOME_NUM_sf = st_as_sf(rasterToPolygons(realm_BIOME_NUM))

realm_raster_sf = st_as_sf(rasterToPolygons(realm_raster))

load('r_grid2_all_threats_all_taxa.Rdata')
load('r_grid_final_March6.Rdata')


st_geometry(r_grid_final) = NULL
r_grid2 = merge(r_grid2,r_grid_final,by = 'layer',all.x = T)  

colnames(r_grid2)

ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],aes(fill = land_areas,col = land_areas))


ggplot()+
  geom_sf(data = realm_raster_sf,aes(fill = as.factor(layer),col = as.factor(layer)))

ggplot()+
  geom_sf(data = realm_BIOME_NUM_sf,aes(fill = as.factor(layer),col = as.factor(layer)))


r_grid2$realm = realm_raster_sf$layer
r_grid2$biome = realm_BIOME_NUM_sf$layer
r_grid2$realm_biome = paste0(r_grid2$realm,'_',r_grid2$biome)

na_sf_biomes=r_grid2[r_grid2$land_areas >0 & r_grid2$biome == 0,]

# ggplot()+
#   geom_sf(data = na_sf,fill = 'black')


library(dplyr)
r_grid2_biomes = r_grid2 %>% group_by(biome) %>% summarise()
ggplot()+geom_sf(data = r_grid2_biomes,aes(fill = as.factor(biome)))

r_grid2_biomes = r_grid2_biomes[r_grid2_biomes$biome !=0,]

r_grid2_realms = r_grid2 %>% group_by(realm) %>% summarise()
ggplot()+geom_sf(data = r_grid2_realms,aes(fill = as.factor(realm)))

r_grid2_realms = r_grid2_realms[r_grid2_realms$realm !=0,]

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

na_sf_realms=r_grid2[r_grid2$land_areas >0 & r_grid2$realm == 0,]


df_biomes=region_guesser(na.cells = na_sf_biomes, reference_regions = r_grid2_biomes, layer = 'layer')

df_realms=region_guesser(na.cells = na_sf_realms, reference_regions = r_grid2_realms, layer = 'layer')

table(df_biomes$region)
table(df_realms$region)


df_biomes_2 = df_biomes[df_biomes$minimum_distance<500000,]
df_realms_2 = df_realms[df_realms$minimum_distance<500000,]

df_biomes_2 = df_biomes
df_realms_2 = df_realms

nrow(df_realms_2)
nrow(df_biomes_2)
setdiff(df_realms_2$layer,df_biomes_2$layer)
setdiff(df_biomes_2$layer,df_realms_2$layer)


  
  df_realms_3 = df_realms_2[!duplicated(df_realms_2$layer),]
  df_biomes_3 = df_biomes_2[!duplicated(df_biomes_2$layer),]

  nrow(df_realms_3)
  nrow(df_biomes_3)
  
  

  r_grid2[r_grid2$layer %in% df_realms_3$layer,]$realm = df_realms_3$region
  r_grid2[r_grid2$layer %in% df_biomes_3$layer,]$biome = df_biomes_3$region

  ggplot()+geom_sf(data = r_grid2[r_grid2$land_areas>0 & r_grid2$realm_biome == '0_0',],aes(fill = as.factor(realm),col = as.factor(realm)))
  




colnames(r_grid2)
library(dplyr)
r_grid2 = r_grid2 %>% filter(names != 'Antarctica')
r_grid2 = r_grid2 %>% filter(names != 'Antartic')

ggplot()+geom_sf(data = r_grid2[r_grid2$land_areas>0 & r_grid2$realm_biome == '0_0',],aes(fill = as.factor(realm),col = as.factor(realm)))
r_grid2$realm_biome = paste0(r_grid2$realm,'_',r_grid2$biome)

ggplot()+geom_sf(data = r_grid2[r_grid2$land_areas>0 & r_grid2$realm_biome == '0_0',],aes(fill = as.factor(biome),col = as.factor(biome)))



groups = unique(r_grid2$realm_biome)
i=1;n=1;m=1

list_dfs = list() 
list_dfs2 = list() 

  for(n in seq_along(list_taxa)){
    print(list_taxa[n])
    
    for(m in seq_along(list_threats)){
      
      var1 = paste0('wege2',list_taxa[n])
      var2 = paste0(list_threats[m],list_taxa[n])
      richness = paste0('richness',list_taxa[n])
      na.grid = r_grid2[r_grid2[[richness]] <10,]
      labely = gsub(list_threats[m],pattern = '_',replacement = ' ')
      r_grid2_temp = r_grid2[r_grid2[[richness]] >9 ,]
  
      quantiles_x = r_grid2_temp[[var1]] %>%
    quantile(probs = seq(0, 1, length.out = 10),na.rm = T)
  
      quantiles_y = r_grid2_temp[[var2]] %>%
    quantile(probs = seq(0, 1, length.out = 10),na.rm = T)
  
      st_geometry(r_grid2_temp) = NULL
      
      df1 = r_grid2_temp[r_grid2_temp[[var1]]>quantiles_x[5] & r_grid2_temp[[var2]]>quantiles_y[5],][,c('layer','realm_biome','names')]
      df1$quantile = '5'
      df2 = r_grid2_temp[r_grid2_temp[[var1]]>quantiles_x[6] & r_grid2_temp[[var2]]>quantiles_y[6],][,c('layer','realm_biome','names')]
      df2$quantile = '6'
      df3 = r_grid2_temp[r_grid2_temp[[var1]]>quantiles_x[7] & r_grid2_temp[[var2]]>quantiles_y[7],][,c('layer','realm_biome','names')]
      df3$quantile = '7'
      df4 = r_grid2_temp[r_grid2_temp[[var1]]>quantiles_x[8] & r_grid2_temp[[var2]]>quantiles_y[8],][,c('layer','realm_biome','names')]
      df4$quantile = '8'
      df5 = r_grid2_temp[r_grid2_temp[[var1]]>quantiles_x[9] & r_grid2_temp[[var2]]>quantiles_y[9],][,c('layer','realm_biome','names')]
      df5$quantile = '9'
      df_all = rbind(df1,df2,df3,df4,df5)
      df_all$group = list_taxa[n] 
      df_all$threat = var2

      list_dfs[[m]] = df_all
    }
    list_dfs_temp = do.call(list_dfs,what = 'rbind')
    list_dfs2[[n]] = list_dfs_temp
  }



list_dfs3 = do.call(list_dfs2,what = 'rbind')

df_1 = list_dfs3 %>% group_by(quantile,realm_biome,group,threat) %>% tally()

df_1[order(df_1$n,decreasing = T),]

write.xlsx(df_1,'df_1.xlsx')

df_1$realm_biome_names = df_1$realm_biome
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_10',replacement = '_Montane Grasslands & Shrublands')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_11',replacement = '_Tundra')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_12',replacement = '_Mediterranean Forests, Woodlands & Scrub')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_13',replacement = '_Deserts & Xeric Shrublands')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_14',replacement = '_Mangroves')

df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_1',replacement = '_Tropical & Subtropical Moist Broadleaf Forests')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_2',replacement = '_Tropical & Subtropical Dry Broadleaf Forests')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_3',replacement = '_Tropical & Subtropical Coniferous Forests')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_4',replacement = '_Temperate Broadleaf & Mixed Forests')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_5',replacement = '_Temperate Conifer Forests')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_6',replacement = '_Boreal Forests/Taiga')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_7',replacement = '_Tropical & Subtropical Grasslands, Savannas & Shrublands')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_8',replacement = '_Temperate Grasslands, Savannas & Shrublands')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '_9',replacement = '_Flooded Grasslands & Savannas')


df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '1_',replacement = 'Afrotropic_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '2_',replacement = 'NA_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '3_',replacement = 'Australasia_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '4_',replacement = 'Indomalayan_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '5_',replacement = 'Antarctica_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '6_',replacement = 'Nearctic_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '7_',replacement = 'Neotropic_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '8_',replacement = 'Oceania_')
df_1$realm_biome_names = gsub(df_1$realm_biome_names,pattern = '9_',replacement = 'Palearctic_')

r_grid2$bi

write.xlsx(df_1,'df_1.xlsx')

colnames(r_grid2)
df_totals=r_grid2 %>% group_by(realm_biome) %>% tally()
st_geometry(df_totals) = NULL
colnames(df_totals)[2] = 'Total cells'
df_1 = merge(df_1,df_totals)
colnames(df_1)
colnames(df_totals)

write.xlsx(df_1,'df_2.xlsx')


quantile_9 = df_1 %>% filter(quantile == 9) %>% group_by(quantile,group,threat) %>% summarise(totals_threats = sum(n))
quantile_8 = df_1 %>% filter(quantile == 8) %>% group_by(quantile,group,threat) %>% summarise(totals_threats = sum(n))


list_df_top5_1 = list()
list_df_top5_2 = list()
for(i in seq_along(groups)){
  group = groups[i]
  for(m in seq_along(list_threats)){
    threat = list_threats[m]
    threat_taxa = paste0(threat,group)
    df_temp = df_1[df_1$quantile==8 & df_1$threat == threat_taxa,]
    df_temp = head(df_temp[order(df_temp$n,decreasing = T),],5)
    df_temp$order = 1:5
    df_temp = merge(df_temp,quantile_8[,-2],by.x = 'threat',by.y = 'threat')
    list_df_top5_1[[m]] = df_temp
    
  }
  list_df_top5_1_df = do.call(list_df_top5_1,what = 'rbind')
  list_df_top5_2[[i]] = list_df_top5_1_df
}

list_df_top5_2_df = do.call(list_df_top5_2,what = 'rbind')

list_df_top5_2_df$percentage = list_df_top5_2_df$n/list_df_top5_2_df$totals_threats


r_grid3 = r_grid2


r_grid3 = merge(r_grid3,list_df_top5_2_df,by = 'realm_biome')


threat = 'logging'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

p_logging = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$Logging))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())

threat = 'agriculture'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

p_agriculture = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$Agriculture))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())



threat = 'hunting'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

table(temp_r_grid[temp_r_grid$group == '_r',]$realm_biome_names)
plot(temp_r_grid[temp_r_grid$realm_biome_names == 'Indomalayan_Deserts & Xeric Shrublands',]$geometry)

ggplot()+ geom_sf(data = temp_r_grid[temp_r_grid$group == '_b',],aes(col = realm_biome_names))


p_hunting = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$Hunting))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())




threat = 'pollution'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

p_pollution = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$Pollution))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())


threat = 'invasives'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

p_invasives = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$Invasives))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())


threat = 'climate_change'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))

p_climate_change = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(color.schemes$`Climate change`))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())


threat = 'urbanization'
temp_r_grid = r_grid3[r_grid3$threat %in% paste0(threat,c('_a','_r','_m','_b')),]
temp_r_grid$threat = factor(temp_r_grid$threat ,levels = paste0(threat,c('_a','_r','_m','_b')))


colors_urbanization = c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177')

p_urbanization = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas>0,],col = 'grey90')+
  geom_sf(data = temp_r_grid, aes(col = order))+
  facet_wrap(~group,ncol = 4)+
  scale_colour_gradientn(colours = rev(colors_urbanization))+
  theme_void()+
  theme(legend.position = 'none',
        strip.text = element_blank())

library(gridExtra)

all_ps = arrangeGrob(p_logging,p_agriculture,p_hunting,p_pollution,p_invasives,p_climate_change,p_urbanization,ncol=1)

ggsave(filename = 'all_ps_quantile_8.png',width = 10,height = 10,plot = all_ps)


