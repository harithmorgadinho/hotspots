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

sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
sf_land = st_transform(sf_land,  
                       crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
sf_land$fake = 1
#sf_land = sf_land %>% group_by(fake) %>% summarise()


r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



#reptile_threats
part1 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/Copenhagen_postdoc/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)

#amphibian_threats
all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/threats.csv',stringsAsFactors = F)


#mammal_threats
all_threats = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/mammal_threats/threats.csv',stringsAsFactors = F)


#bird_threats
part1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/threats.csv',stringsAsFactors = F)
part2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/threats.csv',stringsAsFactors = F)
all_threats = rbind(part1,part2)



r = raster(st_zm(sf_land), resolution = 50000 ,
           crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")

load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_reptiles_summarized.Rdata')
unique(sf_reptiles$category)
sf_reptiles = sf_reptiles[sf_reptiles$category != 'EW',]
sf_reptiles = sf_reptiles[sf_reptiles$category != 'EX',]
sf_reptiles$category = gsub(sf_reptiles$category,pattern = "LR/cd",replacement = 'LC')

sf = sf_reptiles

load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_amphibians_summarized.Rdata')
unique(sf_amphibians$category)
sf_amphibians = sf_amphibians[sf_amphibians$category != 'EW',]
sf_amphibians = sf_amphibians[sf_amphibians$category != 'EX',]
sf = sf_amphibians


load('/Users/gdt366/Dropbox/Copenhagen_postdoc/sf_mammals_summarized.Rdata')
unique(sf_mammals$category)

sf_mammals = sf_mammals[sf_mammals$category != 'EW',]
sf_mammals = sf_mammals[sf_mammals$category != 'EX',]

sf = sf_mammals

load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/sf_birds_summarized_fixed.Rdata')
bird_cat1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/assessments.csv')
bird_cat2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/assessments.csv')
bird_cat = rbind(bird_cat1,bird_cat2)


colnames(sf_birds_cropped)
sf_birds = merge(sf_birds_cropped,bird_cat[,c(3,4)],by.x = 'sci_name',by.y = 'scientificName')
unique(sf_birds$redlistCategory)
sf_birds = sf_birds[sf_birds$redlistCategory != 'Extinct in the Wild',]
sf_birds = sf_birds[sf_birds$redlistCategory != 'Extinct',]



sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Least Concern',
                                replacement = 'LC')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Near Threatened',
                                replacement = 'NT')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Vulnerable',
                                replacement = 'VU')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Endangered',
                                replacement = 'EN')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Critically EN',
                                replacement = 'CR')
sf_birds$redlistCategory = gsub(x = sf_birds$redlistCategory,pattern = 'Data Deficient',
                                replacement = 'DD')

unique(sf_birds$redlistCategory)

colnames(sf_birds)[1:2] = c('binomial','category')

sf = sf_birds




sf = st_transform(sf,crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



#r_grid = st_as_sf(rasterToPolygons(r))
#r_grid$layer = 1:nrow(r_grid)


#cells_to_keep = sort(unique(unlist(sf_reptiles_intersected)))

hunting = c("5.1.1","5.1.2","5.1.3","5.1.4")
agriculture = c("2.1.1","2.1.2","2.1.3","2.1.4","2.2.1","2.2.2","2.2.3","2.3.1",
                "2.3.2","2.3.4","2.4.1","2.4.2","2.4.3")
logging = c("5.3.1", "5.3.2","5.3.3", "5.3.4", "5.3.5")
pollution = c("9.1.1" ,"9.1.2", "9.1.3" ,"9.2.1" ,"9.2.2", "9.2.3", "9.3.1", "9.3.2", "9.3.3", "9.3.4",
              "9.4","9.5.1", "9.5.2","9.5.3","9.5.4" ,"9.6.1","9.6.2","9.6.3","9.6.4")
invasive = c('8.1.1','8.1.2')
climate_change = c("11.1", "11.2" ,"11.3" ,"11.4", "11.5")
urbanization = c("1.1", "1.2" ,"1.3")


sf_intersected = st_intersects(sf,r_grid)



df_number_threatened = cbind.data.frame(logging = length(sf[sf$binomial %in% all_threats[all_threats$code %in% logging,]$scientificName,]$binomial),
                                        agriculture = length(sf[sf$binomial %in% all_threats[all_threats$code %in% agriculture,]$scientificName,]$binomial),
                                        pollution = length(sf[sf$binomial %in% all_threats[all_threats$code %in% pollution,]$scientificName,]$binomial),
                                        invasive = length(sf[sf$binomial %in% all_threats[all_threats$code %in% invasive,]$scientificName,]$binomial),
                                        hunting = length(sf[sf$binomial %in% all_threats[all_threats$code %in% hunting,]$scientificName,]$binomial),
                                        climate_change = length(sf[sf$binomial %in% all_threats[all_threats$code %in% climate_change,]$scientificName,]$binomial),
                                        urbanization = length(sf[sf$binomial %in% all_threats[all_threats$code %in% urbanization,]$scientificName,]$binomial),
                                        all = length(sf[sf$binomial %in% all_threats[all_threats$code %in% c(logging,agriculture,pollution,invasive,hunting,climate_change,urbanization),]$scientificName,]$binomial))

df_number_threatened=as.data.frame(t(df_number_threatened))

df_number_threatened$percentage = round(100*df_number_threatened$V1/nrow(sf),1)

#write.csv(df_number_threatened,'df_number_threatened.csv')

threats = list(hunting,agriculture,logging,pollution,invasive,climate_change,urbanization)


sf$spid = 1:nrow(sf)

sf$range_size = st_area(sf)
units(sf$range_size) = NULL

sf_weight = 1.0/(sf$range_size^(1/3))

ids_hunting = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[1]],]$scientificName,]$spid)
ids_agriculture = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[2]],]$scientificName,]$spid)
ids_logging = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[3]],]$scientificName,]$spid)
ids_pollution = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[4]],]$scientificName,]$spid)
ids_invasives = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[5]],]$scientificName,]$spid)
ids_climate_change = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[6]],]$scientificName,]$spid)
ids_urbanization = unique(sf[sf$binomial %in% all_threats[all_threats$code %in% threats[[7]],]$scientificName,]$spid)

ids_threatened = unique(sf[sf$category %in% c('CR','EN','VU'),]$spid)

#x = t(sf_reptiles_intersected)[[100000]]
#x = t(sf_reptiles_intersected)[100000:100010]

weight_layer = function(x,y){
  if(length(x) == 0){
    value = NA
  }else{
    list_zeros = rep(NA,length(sf_intersected))
    list_zeros[x] = 0
    list_zeros[intersect(x,y)] = 1
    
    value =  predict(glm(list_zeros ~ 1, weights = sf_weight,
                         family = binomial(link = 'logit')),type = 'response')[1]
  }
  return(value)
}

x = t(sf_intersected)


length(ids_hunting)
length(ids_agriculture)
length(ids_logging)
length(ids_pollution)
length(ids_invasives)
length(ids_climate_change)
length(ids_urbanization)
length(ids_threatened)

time0=Sys.time()
y = ids_hunting
probability_hunting <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_agriculture
probability_agriculture <- unlist(lapply(x, FUN = weight_layer,y = y))

y = ids_logging
probability_logging <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_pollution
probability_pollution <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_invasives
probability_invasives <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0

y = ids_climate_change
probability_climate_change <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

y = ids_urbanization
probability_urbanization <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final

ids_any_threat = sort(unique(c(ids_hunting,ids_agriculture,ids_logging,ids_pollution,ids_invasives,ids_climate_change,ids_urbanization)))
y = ids_any_threat
probability_any_threat <- unlist(lapply(x, FUN = weight_layer,y = y))
time_final = Sys.time() - time0
time_final


y = ids_threatened
probability_threatened <- unlist(lapply(x, FUN = weight_layer,y = y))


#richness = unlist(lapply(x, FUN = length))

#r_grid2 = r_grid

r_grid2$hunting_r = probability_hunting
r_grid2$agriculture_r = probability_agriculture
r_grid2$logging_r = probability_logging
r_grid2$pollution_r = probability_pollution
r_grid2$invasives_r = probability_invasives
r_grid2$climate_change_r = probability_climate_change
r_grid2$urbanization_r = probability_urbanization
r_grid2$threatened_r = probability_threatened
r_grid2$any_threat_r = probability_any_threat

hist(r_grid2$hunting_r)


save(r_grid2,file = 'reptiles_grid_probability_final_with_threatened_urbanization.Rdata')


save(r_grid2,file = 'reptiles_grid_probability_final_with_threatened_urbanization_any_threat.Rdata')



sf_land_intersected = st_intersects(sf_land,r_grid)
land_areas = unlist(lapply(t(sf_land_intersected), FUN = length))
r_grid2$land_areas = land_areas

library(ggplot2)


test1 = ggplot()+
  geom_sf(data = r_grid2, aes(fill = hunting),col = NA)+
  scale_fill_gradientn(colours = terrain.colors(10),na.value = 'black')




library(RColorBrewer)
library(scales)
urb_pal=colorRampPalette(c('white','black'))

color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens"),
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds"),
  "Pollution" = brewer.pal(8,"Blues"),
  "Invasives" = brewer.pal(8,"Purples"),
  "Climate change" = brewer.pal(8,"Oranges"),
  "Urbanization" = brewer.pal(8,"RdPu"))


legend.title.size = 10
annotate.size = 3
size_legend_text = 7

r_grid2[r_grid2$richness<10,]$hunting = NA
r_grid2[r_grid2$richness<10,]$agriculture = NA
r_grid2[r_grid2$richness<10,]$logging = NA
r_grid2[r_grid2$richness<10,]$pollution = NA
r_grid2[r_grid2$richness<10,]$invasives = NA
r_grid2[r_grid2$richness<10,]$climate_change = NA
r_grid2[r_grid2$richness<10,]$urbanization = NA
r_grid2[r_grid2$richness<10,]$any_threat = NA

save(r_grid2,file = 'r_grid2_all_threats_all_taxa.Rdata')



r_grid3$land_areas

r_grid3[!is.na(r_grid3$land_areas),]

unique(r_grid3$land_areas)

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


# pdf('test1.pdf')
# print(p1)
# dev.off()


p2 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = agriculture,col=agriculture),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Agriculture,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Agriculture',size = annotate.size)+
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
  labs(tag = 'c')+
  annotation_custom(agriculture_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p3 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = logging,col = logging),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Logging,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Logging',size = annotate.size)+
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
  labs(tag = 'a')+
  annotation_custom(saw_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)


p4 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = pollution,col = pollution),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Pollution,na.value = NA,limits = c(0,1))+
  annotate('text',x = 16382470,y = -7053445,label = 'Pollution',size = annotate.size)+
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
  labs(tag = 'b')+
  annotation_custom(factory_img, xmin=-16782470, xmax=-3000000, ymin=-7753445, ymax= -3700000)

p5 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = invasives,col = invasives),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Invasives,na.value = NA,limits = c(0,1))+
  #theme_void()+
  annotate('text',x = 16382470,y = -7053445,label = 'Invasives',size = annotate.size)+
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
  labs(tag = 'd')+
  annotation_custom(cat_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p6 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = climate_change,col = climate_change),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$`Climate change`,na.value = NA,limits = c(0,1))+
  annotate('text',x = 15082470,y = -7053445,label = 'Climate Change',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        legend.background = element_rect(fill = "transparent",colour = "transparent"),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'f')+
  annotation_custom(climate_c_img, xmin=-17182470, xmax=-3000000, ymin=-8853445, ymax= -3700000)

p7 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = urbanization,col = urbanization),linewidth = 0.0001) +
  scale_fill_gradientn('Impact probability',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  scale_colour_gradientn('Impact probability',colours = color.schemes$Urbanization,na.value = NA,limits = c(0,1))+
  annotate('text',x = 15682470,y = -7053445,label = 'Urbanization',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.75, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))+
  guides(fill = guide_colorbar(title.position = "right"))+
  labs(tag = 'g')+
  annotation_custom(urbanization_img, xmin=-17182470, xmax=-3000000, ymin=-7753445, ymax= -3700000)



library(gridExtra)
pdf('figure1v6.pdf',width = 11,height = 9)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,ncol=2)
dev.off()




r_grid2_df = r_grid2
st_geometry(r_grid2_df) = NULL



highest_threat = function(input){
  
  if(is.infinite(max(na.omit(t(input[,3:9]))))){
    threat = NA
  }else{
    threat = names(which(t(input[,3:9])[,1] == max(na.omit(input[,3:9]))))
    if(length(threat)>1){
      threat = 'Many'
    }
  }
  return(threat)
}

list_higher_threats = list()
for(i in 1:nrow(r_grid2_df)){
  print(i)
  list_higher_threats[[i]] = highest_threat(r_grid2_df[i,])
}

unique(unlist(list_higher_threats))
r_grid2_df$higher_threat = unlist(list_higher_threats)

r_grid2$higher_threat = r_grid2_df$higher_threat

r_grid2$higher_threat = factor(r_grid2$higher_threat,levels = c('hunting','agriculture','logging','pollution','invasives','climate_change','urbanization','Many'))

threats_pal = c(color.schemes$Hunting[7],
                color.schemes$Agriculture[3],
                color.schemes$Logging[5],
                color.schemes$Pollution[5],
                color.schemes$Invasives[5],
                color.schemes$`Climate change`[5],
                color.schemes$Urbanization[7],'black')
unique(r_grid2$higher_threat)

labels_threats = c('Hunting','Agriculture','Logging','Pollution','Invasives','Climate change','Urbanization','Numerous')
p8 = ggplot() +
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],linewidth = 0.0001,bg = 'grey90',col = 'grey90') +
  geom_sf(data = r_grid2,aes(fill = higher_threat, col = higher_threat),linewidth = 0.0001) +
  scale_fill_manual('',values = threats_pal,na.value = NA,labels = labels_threats,na.translate = F)+
  scale_colour_manual('',values = threats_pal,na.value = NA,labels = labels_threats,na.translate = F)+
  annotate('text',x = 15682470,y = -7053445,label = 'Highest threat',size = annotate.size)+
  theme(legend.title = element_text(angle = -90,hjust = 0.5,size = legend.title.size),
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(0.5, 'cm'),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(size = size_legend_text),
        plot.tag = element_text(face='bold'),
        plot.background=element_rect(fill="transparent",colour=NA),
        legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = 'none')+
  labs(tag = 'h')

pdf('figure1v7.pdf',width = 10,height = 9)
grid.arrange(p3,p4,p2,p5,p1,p6,p7,p8,ncol=2)
dev.off()





