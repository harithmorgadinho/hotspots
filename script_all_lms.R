#r_grid2

library(sf)
sf_use_s2(F)
library(ggplot2)

ggplot(data = r_grid_final,aes(x = wege_a,y = climate_change))+
  geom_point()+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm',col = 'black')
  
  
st_geometry(r_grid_final) = NULL
r_grid2 = merge(r_grid2,r_grid_final,by = 'layer',all.x = T)  


summary(r_grid2)

  
colnames(r_grid2)
library(dplyr)
r_grid2 = r_grid2 %>% filter(names != 'Antarctica')
r_grid2 = r_grid2 %>% filter(names != 'Antartic')

threats = c('hunting','agriculture','logging','pollution','invasives','climate_change','urbanization','threatened','any_threat')
group = '_a'
r
pdf('amphibians.pdf')
r_grid2_plotting = r_grid2

for (i in 1:length(threats)){
  print(i)
  var_to_use = paste0(threats[i],group)
  richness = paste0('richness',group)
  r_grid2_plotting$plotting = r_grid2_plotting[[var_to_use]]
  r_grid2_plotting[r_grid2_plotting[[richness]]<10,]$plotting = NA
  
  p1 = ggplot(data = r_grid2_plotting,aes(x = wege2_a,y = plotting))+
    geom_point(col = 'darkred',alpha = 0.5)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm',col = 'black')+
    facet_wrap(~names)+
    ylab(var_to_use)
  print(p1)
  
  }
dev.off()



pdf('reptiles.pdf')
r_grid2_plotting = r_grid2
group = '_r'

for (i in 1:length(threats)){
  print(i)
  var_to_use = paste0(threats[i],group)
  richness = paste0('richness',group)
  r_grid2_plotting$plotting = r_grid2_plotting[[var_to_use]]
  r_grid2_plotting[r_grid2_plotting[[richness]]<10,]$plotting = NA
  
  p1 = ggplot(data = r_grid2_plotting,aes(x = wege2_r,y = plotting))+
    geom_point(col = 'darkred',alpha = 0.5)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm',col = 'black')+
    facet_wrap(~names)+
    ylab(var_to_use)
  print(p1)
  
}
dev.off()

pdf('mammals.pdf')
r_grid2_plotting = r_grid2
group = '_m'

for (i in 1:length(threats)){
  print(i)
  var_to_use = paste0(threats[i],group)
  richness = paste0('richness',group)
  r_grid2_plotting$plotting = r_grid2_plotting[[var_to_use]]
  r_grid2_plotting[r_grid2_plotting[[richness]]<10,]$plotting = NA
  
  p1 = ggplot(data = r_grid2_plotting,aes(x = wege2_m,y = plotting))+
    geom_point(col = 'darkred',alpha = 0.5)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm',col = 'black')+
    facet_wrap(~names)+
    ylab(var_to_use)
  print(p1)
  
}
dev.off()


pdf('birds.pdf')
r_grid2_plotting = r_grid2
group = '_b'

for (i in 1:length(threats)){
  print(i)
  var_to_use = paste0(threats[i],group)
  richness = paste0('richness',group)
  r_grid2_plotting$plotting = r_grid2_plotting[[var_to_use]]
  r_grid2_plotting[r_grid2_plotting[[richness]]<10,]$plotting = NA
  
  p1 = ggplot(data = r_grid2_plotting,aes(x = wege2_b,y = plotting))+
    geom_point(col = 'darkred',alpha = 0.5)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm',col = 'black')+
    facet_wrap(~names)+
    ylab(var_to_use)
  print(p1)
  
}
dev.off()

