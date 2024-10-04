#plotting supplementary figures

wwf_regions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')


load('/Users/gdt366/Dropbox/sorens_project/sf_land_sum.Rdata')
library(raster)
r = raster(extent(wwf_regions),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
background_r_grid = st_as_sf(rasterToPolygons(r))
background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")


background_map = st_transform(wwf_regions, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")


library(ggplot2)
theme_harith = theme_void()+
  theme(legend.position = c(0.93,0.8),
        legend.title.position = 'left',
        legend.key.width = unit(0.15,'cm'),
        legend.key.height = unit(0.2,'cm'),
        legend.text = element_text(size = 4,margin = margin(0, 0, 0, 0.05, unit = "cm")),
        legend.title = element_text(size = 6,hjust = 0.5,angle = 90,margin = margin(0, 0, 0, 0.1, unit = "cm")),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin=margin(0,0,0,0, unit='cm'),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.key = element_rect(color="black",size = 1))


# Richness----
pal1 = colorRampPalette(hcl.colors(10,'zissou1'))

breaks = seq(range(r_grid[r_grid$richness_a>0,]$richness_a)[1], range(r_grid[r_grid$richness_a>0,]$richness_a)[2], length.out = 3)
breaks = round(breaks,0)
p1_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$richness_a>0 & r_grid$land>0,],aes(fill = richness_a,col = richness_a),linewidth = 0)+
  scale_fill_gradientn('Richness (a)',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('Richness (a)',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(r_grid[r_grid$richness_m>0,]$richness_m)[1], range(r_grid[r_grid$richness_m>0,]$richness_m)[2], length.out = 3)
breaks = round(breaks,0)
p1_m = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$richness_m>0 & r_grid$land>0,],aes(fill = richness_m,col = richness_m),linewidth = 0)+
  scale_fill_gradientn('Richness (m)',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('Richness (m)',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(r_grid[r_grid$richness_b>0,]$richness_b)[1], range(r_grid[r_grid$richness_b>0,]$richness_b)[2], length.out = 3)
breaks = round(breaks,0)

p1_b = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$richness_b>0 & r_grid$land>0,],aes(fill = richness_b,col = richness_b),linewidth = 0)+
  scale_fill_gradientn('Richness (b)',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('Richness (b)',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(r_grid[r_grid$richness_r>0,]$richness_r)[1], range(r_grid[r_grid$richness_r>0,]$richness_r)[2], length.out = 3)
breaks = round(breaks,0)

p1_r = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$richness_r>0 & r_grid$land>0,],aes(fill = richness_r,col = richness_r),linewidth = 0)+
  scale_fill_gradientn('Richness (r)',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('Richness (r)',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

library(gridExtra)
grid1 = arrangeGrob(p1_a,p1_b,
                    p1_m,p1_r,ncol = 2)

ggsave(plot = grid1,filename = 'grid1_richness.png',width = 6,height = 3.4,dpi = 1000)


# WEGE----
pal1 = colorRampPalette(hcl.colors(10,'inferno'))

breaks = seq(range(log10(r_grid[r_grid$wege2_a>0,]$wege2_a))[1], range(log10(r_grid[r_grid$wege2_a>0,]$wege2_a))[2], length.out = 2)

p2_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_a>0 & r_grid$land>0,],aes(fill = log10(wege2_a),col = log10(wege2_a)),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(log10(r_grid[r_grid$wege2_m>0,]$wege2_m))[1], range(log10(r_grid[r_grid$wege2_m>0,]$wege2_m))[2], length.out = 2)
p2_m = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_m>0 & r_grid$land>0,],aes(fill = log10(wege2_m),col = log10(wege2_m)),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(log10(r_grid[r_grid$wege2_b>0,]$wege2_b))[1], range(log10(r_grid[r_grid$wege2_b>0,]$wege2_b))[2], length.out = 2)

p2_b = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_b>0 & r_grid$land>0,],aes(fill = log10(wege2_b),col = log10(wege2_b)),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

breaks = seq(range(log10(r_grid[r_grid$wege2_r>0,]$wege2_r))[1], range(log10(r_grid[r_grid$wege2_r>0,]$wege2_r))[2], length.out = 2)

p2_r = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_r>0 & r_grid$land>0,],aes(fill = log10(wege2_r),col = log10(wege2_r)),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks,labels = c('Low','High'))+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))

library(gridExtra)
grid2 = arrangeGrob(p2_a,p2_b,
                    p2_m,p2_r,ncol = 2)

ggsave(plot = grid2,filename = 'grid2_WEGE.png',width = 6,height = 3.4,dpi = 1000)







# WEGE deciles----

theme_harith = theme_void()+
  theme(legend.position = c(0.93,0.8),
        legend.title.position = 'left',
        legend.key.width = unit(0.15,'cm'),
        legend.key.height = unit(0.2,'cm'),
        legend.text = element_text(size = 3,margin = margin(0, 0, 0, 0.05, unit = "cm")),
        legend.title = element_text(size = 5,hjust = 0.5,angle = 90,margin = margin(0, 0, 0, 0.1, unit = "cm")),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin=margin(0,0,0,0, unit='cm'),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.key = element_rect(color="black",size = 1))
pal1 = colorRampPalette(hcl.colors(10,'inferno'))

breaks = c(1:10)

p3_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_a_d>0 & r_grid$land>0,],aes(fill = wege2_a_d,col = wege2_a_d),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p3_m = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_m_d>0 & r_grid$land>0,],aes(fill = wege2_m_d,col = wege2_m_d),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')


p3_b = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_b_d>0 & r_grid$land>0,],aes(fill = wege2_b_d,col = wege2_b_d),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  scale_colour_gradientn('WEGE',colours = pal1(100),breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')


p3_r = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$wege2_r_d>0 & r_grid$land>0,],aes(fill = wege2_r_d,col = wege2_r_d),linewidth = 0)+
  scale_fill_gradientn('WEGE',colours = pal1(10),breaks = breaks)+
  scale_colour_gradientn('WEGE',colours = pal1(10),breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

# ggsave(plot = p3_r,file = 'p3_r.png')

library(gridExtra)
grid3 = arrangeGrob(p3_a,p3_b,
                    p3_m,p3_r,ncol = 2)

ggsave(plot = grid3,filename = 'grid2_WEGE_d.png',width = 6,height = 3.4,dpi = 1000)







# Threat deciles----

theme_harith = theme_void()+
  theme(legend.position = c(0.93,0.8),
        legend.title.position = 'left',
        legend.key.width = unit(0.15,'cm'),
        legend.key.height = unit(0.2,'cm'),
        legend.text = element_text(size = 3,margin = margin(0, 0, 0, 0.05, unit = "cm")),
        legend.title = element_text(size = 5,hjust = 0.5,angle = 90,margin = margin(0, 0, 0, 0.1, unit = "cm")),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.margin=margin(0,0,0,0, unit='cm'),
        legend.spacing.x = unit(0, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.box.spacing = unit(0, "cm"),
        legend.key = element_rect(color="black",size = 1))
pal1 = colorRampPalette(hcl.colors(10,'inferno'))

breaks = c(1:10)

library(RColorBrewer)
color.schemes<-list(
  "Logging" = brewer.pal(8,"Greens"),
  "Agriculture" = rev(brewer.pal(11,"BrBG")[1:5]),
  "Hunting" = brewer.pal(8,"Reds"),
  "Pollution" = brewer.pal(8,"Blues"),
  "Invasives" = brewer.pal(8,"Purples"),
  "Climate change" = brewer.pal(8,"Oranges"),
  "Urbanization" = brewer.pal(8,"RdPu"))

breaks = 1:10
#amphibians
p4_a_l = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$logging_a_d>0 & r_grid$land>0,],aes(fill = logging_a_d,col = logging_a_d),linewidth = 0)+
  scale_fill_gradientn('Logging (a)',colours = color.schemes$Logging,breaks = breaks)+
  scale_colour_gradientn('Logging (a)',colours = color.schemes$Logging,breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_p = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$pollution_a_d>0 & r_grid$land>0,],aes(fill = pollution_a_d,col = pollution_a_d),linewidth = 0)+
  scale_fill_gradientn('Pollution (a)',colours = color.schemes$Pollution,breaks = breaks)+
  scale_colour_gradientn('Pollution (a)',colours = color.schemes$Pollution,breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$agriculture_a_d>0 & r_grid$land>0,],aes(fill = agriculture_a_d,col = agriculture_a_d),linewidth = 0)+
  scale_fill_gradientn('Agriculture (a)',colours = color.schemes$Agriculture,breaks = breaks)+
  scale_colour_gradientn('Agriculture (a)',colours = color.schemes$Agriculture,breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_i = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$invasives_a_d>0 & r_grid$land>0,],aes(fill = invasives_a_d,col = invasives_a_d),linewidth = 0)+
  scale_fill_gradientn('Invasives (a)',colours = color.schemes$Invasives,breaks = breaks)+
  scale_colour_gradientn('Invasives (a)',colours = color.schemes$Invasives,breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_h = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$hunting_a_d>0 & r_grid$land>0,],aes(fill = hunting_a_d,col = hunting_a_d),linewidth = 0)+
  scale_fill_gradientn('Hunting (a)',colours = color.schemes$Hunting,breaks = breaks)+
  scale_colour_gradientn('Hunting (a)',colours = color.schemes$Hunting,breaks = breaks)+
  theme_harith+
  labs(tag = "E")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_c = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$climate_change_a_d>0 & r_grid$land>0,],aes(fill = climate_change_a_d,col = climate_change_a_d),linewidth = 0)+
  scale_fill_gradientn('Climate Change (a)',colours = color.schemes$`Climate change`,breaks = breaks)+
  scale_colour_gradientn('Climate Change (a)',colours = color.schemes$`Climate change`,breaks = breaks)+
  theme_harith+
  labs(tag = "F")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_a_u = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$urbanization_a_d>0 & r_grid$land>0,],aes(fill = urbanization_a_d,col = urbanization_a_d),linewidth = 0)+
  scale_fill_gradientn('Urbanization (a)',colours = color.schemes$Urbanization,breaks = breaks)+
  scale_colour_gradientn('Urbanization (a)',colours = color.schemes$Urbanization,breaks = breaks)+
  theme_harith+
  labs(tag = "G")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

pal_batlow = colorRampPalette(hcl.colors(10,'batlow'))
p4_a_ct = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$a_cum_threats>0 & r_grid$land>0,],aes(fill = a_cum_threats,col = a_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Cumulative threats (a)',colours = pal_batlow(5))+
  scale_colour_gradientn('Cumulative threats (a)',colours = pal_batlow(5))+
  theme_harith+
  labs(tag = "H")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')




library(gridExtra)
grid4_a = arrangeGrob(p4_a_l,p4_a_p,
                    p4_a_a,p4_a_i,
                    p4_a_h,p4_a_c,
                    p4_a_u,p4_a_ct,ncol = 2)

ggsave(plot = grid4_a,filename = 'grid4_a_threats_d.png',width = 6,height = 6.8,dpi = 1000)

#birds

p4_b_l = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$logging_b_d>0 & r_grid$land>0,],aes(fill = logging_b_d,col = logging_b_d),linewidth = 0)+
  scale_fill_gradientn('Logging (b)',colours = color.schemes$Logging,breaks = breaks)+
  scale_colour_gradientn('Logging (b)',colours = color.schemes$Logging,breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_p = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$pollution_b_d>0 & r_grid$land>0,],aes(fill = pollution_b_d,col = pollution_b_d),linewidth = 0)+
  scale_fill_gradientn('Pollution (b)',colours = color.schemes$Pollution,breaks = breaks)+
  scale_colour_gradientn('Pollution (b)',colours = color.schemes$Pollution,breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$agriculture_b_d>0 & r_grid$land>0,],aes(fill = agriculture_b_d,col = agriculture_b_d),linewidth = 0)+
  scale_fill_gradientn('Agriculture (b)',colours = color.schemes$Agriculture,breaks = breaks)+
  scale_colour_gradientn('Agriculture (b)',colours = color.schemes$Agriculture,breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_i = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$invasives_b_d>0 & r_grid$land>0,],aes(fill = invasives_b_d,col = invasives_b_d),linewidth = 0)+
  scale_fill_gradientn('Invasives (b)',colours = color.schemes$Invasives,breaks = breaks)+
  scale_colour_gradientn('Invasives (b)',colours = color.schemes$Invasives,breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_h = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$hunting_b_d>0 & r_grid$land>0,],aes(fill = hunting_b_d,col = hunting_b_d),linewidth = 0)+
  scale_fill_gradientn('Hunting (b)',colours = color.schemes$Hunting,breaks = breaks)+
  scale_colour_gradientn('Hunting (b)',colours = color.schemes$Hunting,breaks = breaks)+
  theme_harith+
  labs(tag = "E")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_c = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$climate_change_b_d>0 & r_grid$land>0,],aes(fill = climate_change_b_d,col = climate_change_b_d),linewidth = 0)+
  scale_fill_gradientn('Climate Change (b)',colours = color.schemes$`Climate change`,breaks = breaks)+
  scale_colour_gradientn('Climate Change (b)',colours = color.schemes$`Climate change`,breaks = breaks)+
  theme_harith+
  labs(tag = "F")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_b_u = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$urbanization_b_d>0 & r_grid$land>0,],aes(fill = urbanization_b_d,col = urbanization_b_d),linewidth = 0)+
  scale_fill_gradientn('Urbanization (b)',colours = color.schemes$Urbanization,breaks = breaks)+
  scale_colour_gradientn('Urbanization (b)',colours = color.schemes$Urbanization,breaks = breaks)+
  theme_harith+
  labs(tag = "G")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

pal_batlow = colorRampPalette(hcl.colors(10,'batlow'))
p4_b_ct = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$a_cum_threats>0 & r_grid$land>0,],aes(fill = a_cum_threats,col = a_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Cumulative threats (b)',colours = pal_batlow(5))+
  scale_colour_gradientn('Cumulative threats (b)',colours = pal_batlow(5))+
  theme_harith+
  labs(tag = "H")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')




library(gridExtra)
grid4_a = arrangeGrob(p4_b_l,p4_b_p,
                      p4_b_a,p4_b_i,
                      p4_b_h,p4_b_c,
                      p4_b_u,p4_b_ct,ncol = 2)

ggsave(plot = grid4_a,filename = 'grid4_b_threats_d.png',width = 6,height = 6.8,dpi = 1000)


#mammals

p4_m_l = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$logging_m_d>0 & r_grid$land>0,],aes(fill = logging_m_d,col = logging_m_d),linewidth = 0)+
  scale_fill_gradientn('Logging (m)',colours = color.schemes$Logging,breaks = breaks)+
  scale_colour_gradientn('Logging (m)',colours = color.schemes$Logging,breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_p = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$pollution_m_d>0 & r_grid$land>0,],aes(fill = pollution_m_d,col = pollution_m_d),linewidth = 0)+
  scale_fill_gradientn('Pollution (m)',colours = color.schemes$Pollution,breaks = breaks)+
  scale_colour_gradientn('Pollution (m)',colours = color.schemes$Pollution,breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$agriculture_m_d>0 & r_grid$land>0,],aes(fill = agriculture_m_d,col = agriculture_m_d),linewidth = 0)+
  scale_fill_gradientn('Agriculture (m)',colours = color.schemes$Agriculture,breaks = breaks)+
  scale_colour_gradientn('Agriculture (m)',colours = color.schemes$Agriculture,breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_i = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$invasives_m_d>0 & r_grid$land>0,],aes(fill = invasives_m_d,col = invasives_m_d),linewidth = 0)+
  scale_fill_gradientn('Invasives (m)',colours = color.schemes$Invasives,breaks = breaks)+
  scale_colour_gradientn('Invasives (m)',colours = color.schemes$Invasives,breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_h = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$hunting_m_d>0 & r_grid$land>0,],aes(fill = hunting_m_d,col = hunting_m_d),linewidth = 0)+
  scale_fill_gradientn('Hunting (m)',colours = color.schemes$Hunting,breaks = breaks)+
  scale_colour_gradientn('Hunting (m)',colours = color.schemes$Hunting,breaks = breaks)+
  theme_harith+
  labs(tag = "E")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_c = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$climate_change_m_d>0 & r_grid$land>0,],aes(fill = climate_change_m_d,col = climate_change_m_d),linewidth = 0)+
  scale_fill_gradientn('Climate Change (m)',colours = color.schemes$`Climate change`,breaks = breaks)+
  scale_colour_gradientn('Climate Change (m)',colours = color.schemes$`Climate change`,breaks = breaks)+
  theme_harith+
  labs(tag = "F")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_m_u = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$urbanization_m_d>0 & r_grid$land>0,],aes(fill = urbanization_m_d,col = urbanization_m_d),linewidth = 0)+
  scale_fill_gradientn('Urbanization (m)',colours = color.schemes$Urbanization,breaks = breaks)+
  scale_colour_gradientn('Urbanization (m)',colours = color.schemes$Urbanization,breaks = breaks)+
  theme_harith+
  labs(tag = "G")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

pal_batlow = colorRampPalette(hcl.colors(10,'batlow'))
p4_m_ct = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$a_cum_threats>0 & r_grid$land>0,],aes(fill = a_cum_threats,col = a_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Cumulative threats (m)',colours = pal_batlow(5))+
  scale_colour_gradientn('Cumulative threats (m)',colours = pal_batlow(5))+
  theme_harith+
  labs(tag = "H")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')




library(gridExtra)
grid4_a = arrangeGrob(p4_m_l,p4_m_p,
                      p4_m_a,p4_m_i,
                      p4_m_h,p4_m_c,
                      p4_m_u,p4_m_ct,ncol = 2)

ggsave(plot = grid4_a,filename = 'grid4_m_threats_d.png',width = 6,height = 6.8,dpi = 1000)


#reptiles

p4_r_l = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$logging_r_d>0 & r_grid$land>0,],aes(fill = logging_r_d,col = logging_r_d),linewidth = 0)+
  scale_fill_gradientn('Logging (r)',colours = color.schemes$Logging,breaks = breaks)+
  scale_colour_gradientn('Logging (r)',colours = color.schemes$Logging,breaks = breaks)+
  theme_harith+
  labs(tag = "A")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_p = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$pollution_r_d>0 & r_grid$land>0,],aes(fill = pollution_r_d,col = pollution_r_d),linewidth = 0)+
  scale_fill_gradientn('Pollution (r)',colours = color.schemes$Pollution,breaks = breaks)+
  scale_colour_gradientn('Pollution (r)',colours = color.schemes$Pollution,breaks = breaks)+
  theme_harith+
  labs(tag = "B")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_a = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$agriculture_r_d>0 & r_grid$land>0,],aes(fill = agriculture_r_d,col = agriculture_r_d),linewidth = 0)+
  scale_fill_gradientn('Agriculture (r)',colours = color.schemes$Agriculture,breaks = breaks)+
  scale_colour_gradientn('Agriculture (r)',colours = color.schemes$Agriculture,breaks = breaks)+
  theme_harith+
  labs(tag = "C")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_i = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$invasives_r_d>0 & r_grid$land>0,],aes(fill = invasives_r_d,col = invasives_r_d),linewidth = 0)+
  scale_fill_gradientn('Invasives (r)',colours = color.schemes$Invasives,breaks = breaks)+
  scale_colour_gradientn('Invasives (r)',colours = color.schemes$Invasives,breaks = breaks)+
  theme_harith+
  labs(tag = "D")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_h = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$hunting_r_d>0 & r_grid$land>0,],aes(fill = hunting_r_d,col = hunting_r_d),linewidth = 0)+
  scale_fill_gradientn('Hunting (r)',colours = color.schemes$Hunting,breaks = breaks)+
  scale_colour_gradientn('Hunting (r)',colours = color.schemes$Hunting,breaks = breaks)+
  theme_harith+
  labs(tag = "E")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_c = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$climate_change_r_d>0 & r_grid$land>0,],aes(fill = climate_change_r_d,col = climate_change_r_d),linewidth = 0)+
  scale_fill_gradientn('Climate Change (r)',colours = color.schemes$`Climate change`,breaks = breaks)+
  scale_colour_gradientn('Climate Change (r)',colours = color.schemes$`Climate change`,breaks = breaks)+
  theme_harith+
  labs(tag = "F")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

p4_r_u = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$urbanization_r_d>0 & r_grid$land>0,],aes(fill = urbanization_r_d,col = urbanization_r_d),linewidth = 0)+
  scale_fill_gradientn('Urbanization (r)',colours = color.schemes$Urbanization,breaks = breaks)+
  scale_colour_gradientn('Urbanization (r)',colours = color.schemes$Urbanization,breaks = breaks)+
  theme_harith+
  labs(tag = "G")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')

pal_batlow = colorRampPalette(hcl.colors(10,'batlow'))
p4_r_ct = ggplot()+
  geom_sf(data = background_r_grid,bg = 'grey95',linewidth = 0.05,col = 'grey95')+
  geom_sf(data = background_map,bg = 'grey80',col = 'grey80',linewidth = 0)+
  geom_sf(data = r_grid[r_grid$a_cum_threats>0 & r_grid$land>0,],aes(fill = a_cum_threats,col = a_cum_threats),linewidth = 0)+
  scale_fill_gradientn('Cumulative threats (r)',colours = pal_batlow(5))+
  scale_colour_gradientn('Cumulative threats (r)',colours = pal_batlow(5))+
  theme_harith+
  labs(tag = "H")+
  theme(plot.tag = element_text(colour = 'black'))+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0))+
  guides(size = 'none')




library(gridExtra)
grid4_a = arrangeGrob(p4_r_l,p4_r_p,
                      p4_r_a,p4_r_i,
                      p4_r_h,p4_r_c,
                      p4_r_u,p4_r_ct,ncol = 2)

ggsave(plot = grid4_a,filename = 'grid4_r_threats_d.png',width = 6,height = 6.8,dpi = 1000)



