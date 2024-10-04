
library(rnaturalearth)
moz_shapefile = ne_countries(scale = 10,country = 'Mozambique',returnclass = 'sf')
moz_shapefile = st_transform(moz_shapefile,crs = st_crs(r_grid2)[[1]])

mozambique_maps = st_intersection(r_grid2,moz_shapefile)
colnames(r_grid2)

pal_moz = colorRampPalette(c('steelblue','khaki','indianred'))

p_moz_a = ggplot()+
  geom_sf(data = mozambique_maps,aes(fill = log10(wege2_a)),size = 0.1,col=NA)+
  scale_fill_gradientn('',colours =  pal_moz(10))+
  theme_void()+
  theme(legend.position = 'none')+
  labs(subtitle = 'A.')
  
p_moz_r = ggplot()+
  geom_sf(data = mozambique_maps,aes(fill = log10(wege2_r)),size = 0.1,col=NA)+
  scale_fill_gradientn('',colours =  pal_moz(10))+
  theme_void()+
  theme(legend.position = 'none')+
  labs(subtitle = 'B.')

p_moz_m = ggplot()+
  geom_sf(data = mozambique_maps,aes(fill = log10(wege2_m)),size = 0.1,col=NA)+
  scale_fill_gradientn('',colours =  pal_moz(10))+
  theme_void()+
  theme(legend.position = 'none')+
  labs(subtitle = 'C.')

p_moz_b = ggplot()+
  geom_sf(data = mozambique_maps,aes(fill = log10(wege2_b)),size = 0.1,col=NA)+
  scale_fill_gradientn('',colours =  pal_moz(10))+
  theme_void()+
  theme(legend.position = 'none')+
  labs(subtitle = 'D.')

p_moz_all = arrangeGrob(ncol = 4,p_moz_a,p_moz_r,p_moz_m,p_moz_b)
ggsave(filename = 'wege_maps_moz.png',width = 10,height = 4,plot = p_moz_all)


ggplot()+
  geom_sf(data = r_grid2[r_grid2$richness>9,],aes(fill = urbanization_m),size = 0.1,col=NA)


