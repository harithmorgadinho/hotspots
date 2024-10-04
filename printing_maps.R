#visualizations

library(ggplot2)

r_grid2 = r_grid
#r_grid2[r_grid2$land_areas == 0,]$wege_b = NA
#r_grid2[r_grid2$land_areas == 0,]$wege2_b = NA

pal = colorRampPalette(c('steelblue','gold','indianred'))

colnames(r_grid)
list_metrics = c('richness_b','wege_b','wege2_b','we_b','ge_b',
                 'richness_m','wege_m','wege2_m','we_m','ge_m',
                 'richness_r','wege_r','wege2_r','we_r','ge_r',
                 'richness_a','wege_a','wege2_a','we_a','ge_a')

r_grid2$log_wege_b = log10(r_grid2$wege_b)
r_grid2$log_wege2_b = log10(r_grid2$wege2_b)
r_grid2$log_we_b = log10(r_grid2$we_b)

r_grid2$log_wege_m = log10(r_grid2$wege_m)
r_grid2$log_wege2_m = log10(r_grid2$wege2_m)
r_grid2$log_we_m = log10(r_grid2$we_m)

r_grid2$log_wege_r = log10(r_grid2$wege_r)
r_grid2$log_wege2_r = log10(r_grid2$wege2_r)
r_grid2$log_we_r = log10(r_grid2$we_r)

r_grid2$log_wege_a = log10(r_grid2$wege_a)
r_grid2$log_wege2_a = log10(r_grid2$wege2_a)
r_grid2$log_we_a = log10(r_grid2$we_a)

list_metrics = c('richness_b','log_wege_b','log_wege2_b','log_we_b','ge_b',
                 'richness_m','log_wege_m','log_wege2_m','log_we_m','ge_m',
                 'richness_r','log_wege_r','log_wege2_r','log_we_r','ge_r',
                 'richness_a','log_wege_a','log_wege2_a','log_we_a','ge_a')


pdf('all_metrics.pdf',width = 50,height = 30)

for(i in seq_along(list_metrics))  {

  print(i)
r_grid2$print_now = r_grid2[[list_metrics[i]]]
r_grid2[r_grid2$land_areas == 0,]$print_now = NA


p1 = ggplot()+
  geom_sf(data = r_grid2[r_grid2$land_areas == 1,],bg = 'grey80',col = NA,size = 0.1)+
  geom_sf(data = r_grid2,aes(fill = print_now,colour = print_now),size = 0.1)+
  scale_fill_gradientn('',colours = pal(20),na.value = NA)+
  scale_colour_gradientn('',colours = pal(20),na.value = NA)+
  theme_void()+
  theme(panel.grid.major = element_line(colour = 'grey50',linewidth = 0.1),
        legend.position = 'bottom',
        legend.key.width = unit(4,'cm'),
        plot.subtitle = element_text(size = 30))+
  labs(subtitle = list_metrics[i])
  
print(p1)

}

dev.off()





