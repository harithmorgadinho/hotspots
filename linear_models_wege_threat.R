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

list_regions = c('North America','Mesoamerica','Caribbean Islands','South America',
                 'Europe','North Africa','Sub-Saharan Africa', 'West and Central Asia', 
                 'North Asia','East Asia','South and Southeast Asia','Oceania')

library(ggplot2)
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
  "Urbanization" = urb_pal(8))

size_axis = 8
size_axis_text = 7
threats_pal = c(color.schemes$Hunting[5],
                color.schemes$Agriculture[3],
                color.schemes$Logging[5],
                color.schemes$Pollution[5],
                color.schemes$Invasives[5],
                color.schemes$`Climate change`[5],
                'black')
list_regions2 = list()
list_regions2[[1]] = c(list_regions[1])
list_regions2[[2]] = c(list_regions[2],list_regions[3],list_regions[4])
list_regions2[[3]] = c(list_regions[5],list_regions[6],list_regions[8],list_regions[9],list_regions[10])
list_regions2[[4]] = c(list_regions[7])
list_regions2[[5]] = c(list_regions[11],list_regions[12])





pdf('amphibians.pdf')

r_grid2_coefficients = r_grid2
r_grid2_plotting = r_grid2
group = '_a'
lm_list2 = list()
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
  #print(p1)
  
  
  lm_list = list()
  for(m in seq_along(unique(r_grid2$names))){
  temp_grid = r_grid2_plotting[r_grid2_plotting$names %in% unique(r_grid2$names)[m] & r_grid2_plotting[[richness]]>9,]
  name_1=lm(data = temp_grid,wege2_a ~ temp_grid[[var_to_use]])
  name_1 = summary(name_1)
  name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
  row.names(name_1) = NULL
  lm_list[[m]] = name_1
  
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
  
}
lm_list_final = do.call(lm_list2,what = 'rbind')

dev.off()

p=arrangeGrob(p1,p2,p3,p4,p5,p6,p7,ncol = 2)

ggsave(filename = 'amphibians_coefficients.png',plot = p,width = 10,height = 10)











regression_symbols = function(x){
  x[x<0.001] = '***'
  x[x>0.05] = ' '
  x[x<0.05 & x>0.01] = '*'
  x[x<0.01 & x>0.001] = '**'
  return(x)
}

lm_list_final$regression_symbols = regression_symbols(lm_list_final$`Pr(>|t|)`)

library(openxlsx)
write.xlsx(lm_list_final,file = "lm_list_final_amphibians.xlsx")

unique(r_grid2_plotting$names)

pdf('reptiles.pdf')
r_grid2_plotting = r_grid2
group = '_r'
lm_list2 = list()

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
  #print(p1)
  
  lm_list = list()
  for(m in seq_along(unique(r_grid2$names))){
    temp_grid = r_grid2_plotting[r_grid2_plotting$names %in% unique(r_grid2$names)[m] & r_grid2_plotting[[richness]]>9,]
    name_1=lm(data = temp_grid,wege2_a ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
}

lm_list_final = do.call(lm_list2,what = 'rbind')

dev.off()

lm_list_final$regression_symbols = regression_symbols(lm_list_final$`Pr(>|t|)`)

write.xlsx(lm_list_final,file = "lm_list_final_reptiles.xlsx")




pdf('mammals.pdf')
r_grid2_plotting = r_grid2
group = '_m'
lm_list2 = list()

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
  #print(p1)
  
  lm_list = list()
  for(m in seq_along(unique(r_grid2$names))){
    temp_grid = r_grid2_plotting[r_grid2_plotting$names %in% unique(r_grid2$names)[m] & r_grid2_plotting[[richness]]>9,]
    name_1=lm(data = temp_grid,wege2_a ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
}

lm_list_final = do.call(lm_list2,what = 'rbind')

dev.off()

lm_list_final$regression_symbols = regression_symbols(lm_list_final$`Pr(>|t|)`)

write.xlsx(lm_list_final,file = "lm_list_final_mammals.xlsx")


pdf('birds.pdf')
r_grid2_plotting = r_grid2
group = '_b'
lm_list2 = list()

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
  #print(p1)
  
  lm_list = list()
  for(m in seq_along(unique(r_grid2$names))){
    temp_grid = r_grid2_plotting[r_grid2_plotting$names %in% unique(r_grid2$names)[m] & r_grid2_plotting[[richness]]>9,]
    name_1=lm(data = temp_grid,wege2_a ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
}

lm_list_final = do.call(lm_list2,what = 'rbind')

dev.off()

lm_list_final$regression_symbols = regression_symbols(lm_list_final$`Pr(>|t|)`)

write.xlsx(lm_list_final,file = "lm_list_final_birds.xlsx")


