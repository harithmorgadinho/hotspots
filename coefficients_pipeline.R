#pdf('amphibians.pdf')

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
  
  
  for(n in seq_along(unique(r_grid2_coefficients$names))){
    r_grid2_coefficients[r_grid2_coefficients$names %in%  unique(r_grid2_coefficients$names)[n] & !is.na(r_grid2_coefficients[[var_to_use]]),][[var_to_use]]=
      lm_list2[[i]][lm_list2[[i]]$name %in% unique(r_grid2_coefficients$names)[n],]$Estimate[2]
    
  }
  p1 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = hunting_a,col =hunting_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')
  p2 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = agriculture_a,col =agriculture_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')
  p3 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = logging_a,col =logging_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Logging,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Logging,na.value = 'grey90')
  p4 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = pollution_a,col =pollution_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')
  p5 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = invasives_a,col =invasives_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')
  p6 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = climate_change_a,col =climate_change_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')
  p7 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = urbanization_a,col =urbanization_a))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')
}

lm_list_final = do.call(lm_list2,what = 'rbind')

#dev.off()

p=arrangeGrob(p1,p2,p3,p4,p5,p6,p7,ncol = 2)

ggsave(filename = 'amphibians_coefficients.png',plot = p,width = 10,height = 10)

#pdf('reptiles.pdf')

r_grid2_coefficients = r_grid2
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
    name_1=lm(data = temp_grid,wege2_r ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
  
  for(n in seq_along(unique(r_grid2_coefficients$names))){
    r_grid2_coefficients[r_grid2_coefficients$names %in%  unique(r_grid2_coefficients$names)[n] & !is.na(r_grid2_coefficients[[var_to_use]]),][[var_to_use]]=
      lm_list2[[i]][lm_list2[[i]]$name %in% unique(r_grid2_coefficients$names)[n],]$Estimate[2]
    
  }
  p1 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = hunting_r,col =hunting_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')
  p2 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = agriculture_r,col =agriculture_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')
  p3 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = logging_r,col =logging_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Logging,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Logging,na.value = 'grey90')
  p4 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = pollution_r,col =pollution_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')
  p5 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = invasives_r,col =invasives_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')
  p6 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = climate_change_r,col =climate_change_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')
  p7 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = urbanization_r,col =urbanization_r))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')
}

lm_list_final = do.call(lm_list2,what = 'rbind')

#dev.off()

p=arrangeGrob(p1,p2,p3,p4,p5,p6,p7,ncol = 2)

ggsave(filename = 'reptiles_coefficients.png',plot = p,width = 10,height = 10)



#pdf('mammals.pdf')

r_grid2_coefficients = r_grid2
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
    name_1=lm(data = temp_grid,wege2_m ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
  
  for(n in seq_along(unique(r_grid2_coefficients$names))){
    r_grid2_coefficients[r_grid2_coefficients$names %in%  unique(r_grid2_coefficients$names)[n] & !is.na(r_grid2_coefficients[[var_to_use]]),][[var_to_use]]=
      lm_list2[[i]][lm_list2[[i]]$name %in% unique(r_grid2_coefficients$names)[n],]$Estimate[2]
    
  }
  p1 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = hunting_m,col =hunting_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')
  p2 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = agriculture_m,col =agriculture_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')
  p3 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = logging_m,col =logging_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Logging,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Logging,na.value = 'grey90')
  p4 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = pollution_m,col =pollution_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')
  p5 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = invasives_m,col =invasives_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')
  p6 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = climate_change_m,col =climate_change_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')
  p7 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = urbanization_m,col =urbanization_m))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')
}

lm_list_final = do.call(lm_list2,what = 'rbind')

#dev.off()

p=arrangeGrob(p1,p2,p3,p4,p5,p6,p7,ncol = 2)

ggsave(filename = 'mammals_coefficients.png',plot = p,width = 10,height = 10)


#pdf('birds.pdf')

r_grid2_coefficients = r_grid2
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
    name_1=lm(data = temp_grid,wege2_b ~ temp_grid[[var_to_use]])
    name_1 = summary(name_1)
    name_1= cbind.data.frame(name = unique(r_grid2$names)[m],var1 = var_to_use,var2 = c('Intercept','Variable'),name_1$coefficients)
    row.names(name_1) = NULL
    lm_list[[m]] = name_1
    
  }
  lm_list2[[i]] = do.call(lm_list,what = 'rbind')
  
  
  for(n in seq_along(unique(r_grid2_coefficients$names))){
    r_grid2_coefficients[r_grid2_coefficients$names %in%  unique(r_grid2_coefficients$names)[n] 
                         & !is.na(r_grid2_coefficients[[var_to_use]]),][[var_to_use]]=
      lm_list2[[i]][lm_list2[[i]]$name %in% unique(r_grid2_coefficients$names)[n],]$Estimate[2]
    
  }
  
  
  
  
  p1 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = hunting_b,col =hunting_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Hunting,na.value = 'grey90')
  p2 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = agriculture_b,col =agriculture_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Agriculture,na.value = 'grey90')
  p3 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = logging_b,col =logging_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Logging,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Logging,na.value = 'grey90')
  p4 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = pollution_b,col =pollution_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Pollution,na.value = 'grey90')
  p5 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = invasives_b,col =invasives_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Invasives,na.value = 'grey90')
  p6 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = climate_change_b,col =climate_change_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$`Climate change`,na.value = 'grey90')
  p7 = ggplot()+
    geom_sf(data = r_grid2_coefficients,aes(fill = urbanization_b,col =urbanization_b))+
    theme_void()+
    scale_fill_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')+
    scale_colour_gradientn(colours = color.schemes$Urbanization,na.value = 'grey90')
}

lm_list_final = do.call(lm_list2,what = 'rbind')

#dev.off()

p=arrangeGrob(p1,p2,p3,p4,p5,p6,p7,ncol = 2)

ggsave(filename = 'birds_coefficients.png',plot = p,width = 10,height = 10)



