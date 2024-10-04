library(sf)
sf_use_s2(F)
library(ggplot2)
load('r_grid_all_fixed_17_Sep_2024.Rdata')
#load('r_grid_metrics_run2_deciles_nov_27.Rdata')

load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_NOV29.Rdata')
library(dplyr)
colnames(r_grid)
r_grid$biomes = r_grid_soc_eco$BIOME_NAME
r_grid$pop = r_grid_soc_eco$population_counts

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'



r_grid$biomes = gsub(r_grid$biomes,pattern = 'N/A', replacement = 'Rock and Ice')

ggplot()+geom_sf(data = r_grid[!is.na(r_grid$biomes),],aes(fill = biomes),linewidth = 0)

ggplot()+geom_sf(data = r_grid[r_grid$biomes %in% 'N/A',],aes(fill = biomes),linewidth = 0)

ggplot()+geom_sf(data = r_grid[r_grid$biomes %in% 'Tundra',],aes(fill = biomes),linewidth = 0)

df_biomes_hotspots = r_grid %>% filter(hotspots_all %in% 1) %>% group_by(biomes) %>% summarise(people_hotspots = sum(pop))
st_geometry(df_biomes_hotspots) = NULL




df_biomes = r_grid %>% group_by(biomes) %>% summarise(people_biome = sum(pop))

st_geometry(df_biomes) = NULL


r_grid$area = st_area(r_grid)/1000000
units(r_grid$area) = NULL

df_biomes_areas = r_grid %>% group_by(biomes) %>% summarise(area_biome = sum(area))
st_geometry(df_biomes_areas) = NULL

df_biomes_areas_hotspots = r_grid %>% filter(hotspots_all %in% 1) %>% group_by(biomes) %>% summarise(area_hotspot = sum(area))
st_geometry(df_biomes_areas_hotspots) = NULL



sum(df_biomes$people_biome)


biome_all_df = merge(df_biomes,df_biomes_hotspots, by = 'biomes')
biome_all_df = merge(biome_all_df,df_biomes_areas, by = 'biomes')
biome_all_df = merge(biome_all_df,df_biomes_areas_hotspots, by = 'biomes')

biome_all_df$percentage_people_biome = round(100*(biome_all_df$people_hotspots/biome_all_df$people_biome),1)
biome_all_df$percentage_people_globally = round(100*(biome_all_df$people_hotspots/sum(biome_all_df$people_biome)),1)
biome_all_df$percentage_land_biome = round(100*(biome_all_df$area_hotspot/sum(biome_all_df$area_biome)),1)
biome_all_df$percentage_hotspot_biome = round(100*(biome_all_df$area_hotspot/biome_all_df$area_biome),1)

ggplot()+geom_bar(data = biome_all_df,aes(y = people_hotspots, x = biomes),
                  stat = 'identity',position = 'dodge')+
  coord_flip()
library(reshape2)
df_plot = melt(biome_all_df[,c('biomes','percentage_people_globally','percentage_hotspot_biome')])
unique(r_grid$biomes)
plot(r_grid[r_grid$biomes == "Tropical & Subtropical Coniferous Forests",]$geometry)

list_biomes=biome_all_df[order(biome_all_df$percentage_people_globally,decreasing = T),]$biomes
biome_all_df$biomes = factor(biome_all_df$biomes,levels = rev(list_biomes))

list_biomes=biome_all_df[order(biome_all_df$percentage_hotspot_biome,decreasing = T),]$biomes
df_plot$biomes = factor(biome_all_df$biomes,levels = rev(list_biomes))

library(openxlsx)
write.xlsx(biome_all_df,file = 'biome_all_df.xlsx')

pal = hcl.colors(11,'RdYlGn')
theme_harith = theme(legend.position = c(0.5,0.05),
                     legend.background = element_blank(),
                     legend.title = element_blank(),
                     axis.title.y = element_blank(),
                     axis.ticks = element_blank(),
                     axis.title.x = element_text(family = myfont),
                     panel.background = element_blank(),
                     axis.line = element_line(),
                     axis.text.x = element_text(colour = 'black',family = myfont,size = 9),
                     axis.text.y = element_text(colour = 'black',family = myfont,size = 9),
                     legend.key.height = unit(0.05,'cm'),
                     legend.key.width= unit(0.5,'cm'),
                     legend.text = element_text(size = 7,family = myfont))

p = ggplot()+geom_bar(data = df_plot,aes(y = biomes, x = value, fill = variable),
                      stat = 'identity',position = position_dodge(width = 0.6),width = 0.5)+
  theme_harith+
  scale_fill_manual(values = c(pal[2],pal[8]),labels = c('Percentage of global population in hotspots','Percentage of biome with hotspots'))+
  guides(fill = guide_legend(reverse = T))+
  xlab('Percentage of global land area or people')+
  scale_x_continuous(expand = c(0.025,0.01),limits = c(0,70),labels = function(x) paste0(x, "%"))+
  geom_point(data = df_plot,aes(y = biomes, x = value, fill = variable),
             stat = 'identity',position = position_dodge(width = 0.6),size = 6,shape=21,col = 'transparent',show.legend = F)+
  geom_text(data = df_plot,aes(y = biomes, x = value,label = paste0(round(value,1),'%')),size = 2,col = c(rep('white',15),rep('black',15)),
            position = position_dodge(width = 0.5),vjust = c(rep(1,15),rep(-0.25,15)),family = myfont)+
  annotate(geom = 'text',y = 14.5,x = df_plot$value[26]+4,label = '393 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 14.5,x =  df_plot$value[11]+5,label = '72 million people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 13.5,x = df_plot$value[29]+4,label = '1 million sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 13.5,x =  df_plot$value[14]+5,label = '1.7 billion people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 12.5,x = df_plot$value[19]+4,label = '755 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 12.5,x =  df_plot$value[4]+5,label = '64 million people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 11.5,x = df_plot$value[27]+4,label = '1.6 million sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 11.5,x =  df_plot$value[12]+5,label = '175 million people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 10.5,x = df_plot$value[20]+4,label = '1 million sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 10.5,x =  df_plot$value[5]+5,label = '152 million people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 9.5,x = df_plot$value[21]+4,label = '815 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 9.5,x =  df_plot$value[6]+5,label = '42 million people',family = myfont,size = 2)+
  annotate(geom = 'text',y = 8.5,x = df_plot$value[23]+7,label = '251 million people / 1.9 million sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 7.5,x = df_plot$value[24]+10,label = '17 million people / 290 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 6.5,x = df_plot$value[13]+13,label = '87 million people / 1 million sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 5.5,x = df_plot$value[17]+10,label = '79 million people / 895 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 4.5,x = df_plot$value[25]+10,label = '22 million people / 410 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 3.5,x = df_plot$value[18]+10,label = '7 million people / 30 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 2.5,x = df_plot$value[15]+10,label = '47 thousand people / 307 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 1.5,x = df_plot$value[16]+10,label = '15 thousand people / 18 thousand sqkm',family = myfont,size = 2)+
  annotate(geom = 'text',y = 0.7,x = df_plot$value[22]+10,label = '180 thousand people / 18 thousand sqkm',family = myfont,size = 2)












#ggsave(filename = 'Barchart_figure5.png',plot = p,width = 10,height = 4)

df_circular1 = cbind.data.frame(area_inside = sum(biome_all_df$area_hotspot),area_outside = sum(biome_all_df$area_biome) - sum(biome_all_df$area_hotspot))
df_circular1$area_inside = 100*(df_circular1$area_inside)/sum(df_circular1$area_inside,df_circular1$area_outside)
df_circular1$area_outside = 100 - df_circular1$area_inside

df_circular2 = cbind.data.frame(people_inside = sum(biome_all_df$people_hotspots),people_outside = sum(biome_all_df$people_biome) - sum(biome_all_df$people_hotspots))
df_circular2$people_inside = 100*(df_circular2$people_inside)/sum(df_circular2$people_inside,df_circular2$people_outside)
df_circular2$people_outside = 100 - df_circular2$people_inside
df_circular1 = melt(df_circular1)
df_circular2 = melt(df_circular2)


theme_harith2 = theme(legend.position = c(0.95,0.25),
                      legend.title = element_blank(),
                      plot.margin=unit(c(0,0,0,0),"mm"),
                      panel.spacing = unit(c(0,0,0,0),"mm"),
                      legend.key.size = unit(0.25,'cm'),
                      legend.text = element_text(size = 6,family = myfont))

p_circular1 = ggplot(df_circular1, aes(x = 1, y = value, fill = variable)) +
  geom_col(col = 'white',linewidth = 1) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 1 + 1))+
  theme_void()+
  theme_harith2+
  scale_fill_manual(values = c(pal[9],pal[7]),labels = c('Land area of hotspots','Land area outside hotspots'))+
  guides(fill = guide_legend(direction = 'vertical'))+
  geom_text(aes(x = 1,y = rev(value)+6,label = paste0(round(value,1),'%')),size = 2,col = 'white',family = myfont,fontface = 'bold')


pal_2 = hcl.colors(5,'Reds')
p_circular2 = ggplot(df_circular2, aes(x = 1, y = value, fill = variable)) +
  geom_col(col = 'white',linewidth =1) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 1 + 1))+
  theme_void()+
  theme_harith2+
  scale_fill_manual(values = c(pal[1],pal_2[4]),labels = c('People in hotspots','People outside hotspots'))+
  guides(fill = guide_legend(direction = 'vertical'))+
  geom_text(aes(x = 1,y = rev(value)+10,label = paste0(round(value,1),'%')),size = 2,col = 'white',family = myfont,fontface = 'bold')


#grid = arrangeGrob(p_circular1,p_circular2,nrow =1)

library(gridExtra)


#ggsave(grid,width = 8,height = 4,dpi = 1000,filename = 'circularplots_grid.png')


library(patchwork)
combined_plot <- p + 
  inset_element(p_circular2, left = 0.5, bottom = 0, right = 0.9, top = 0.5) +
  inset_element(p_circular1, left = 0.5, bottom = 0.3, right = 0.9, top = 0.8)


ggsave(filename = 'Figure5_people_23_sep.png',plot = combined_plot,width = 8,height = 4,dpi = 1000)

