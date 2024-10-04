
amphibians_out_deciles_wege = read.csv('sp_list_out_wege2_a_d.csv')
amphibians_out_deciles_wege_df = as.data.frame(table(amphibians_out_deciles_wege$category))
colnames(amphibians_out_deciles_wege_df) = c('Category','a_wege')
amphibians_out_irreplaceability = read.csv('sp_list_out_a_hotspot_ferrier_a.csv')
amphibians_out_irreplaceability_df = as.data.frame(table(amphibians_out_irreplaceability$category))
colnames(amphibians_out_irreplaceability_df) = c('Category','a_irr')
amphibians_out_irreplaceability_hot = read.csv('sp_list_out_a_hotspot_ferrier_a_hot.csv')
amphibians_out_irreplaceability_hot_df = as.data.frame(table(amphibians_out_irreplaceability_hot$category))
colnames(amphibians_out_irreplaceability_hot_df) = c('Category','a_irr_h')

reptiles_out_deciles_wege = read.csv('sp_list_out_wege2_r_d.csv')
reptiles_out_deciles_wege_df = as.data.frame(table(reptiles_out_deciles_wege$category))
colnames(reptiles_out_deciles_wege_df) = c('Category','r_wege')

reptiles_out_irreplaceability = read.csv('sp_list_out_r_hotspot_ferrier_r.csv')
reptiles_out_irreplaceability_df = as.data.frame(table(reptiles_out_irreplaceability$category))
colnames(reptiles_out_irreplaceability_df) = c('Category','r_irr')

reptiles_out_irreplaceability_hot = read.csv('sp_list_out_r_hotspot_ferrier_r_hot.csv')
reptiles_out_irreplaceability_hot_df = as.data.frame(table(reptiles_out_irreplaceability_hot$category))
colnames(reptiles_out_irreplaceability_hot_df) = c('Category','r_irr_h')


mammals_out_deciles_wege = read.csv('sp_list_out_wege2_m_d.csv')
mammals_out_deciles_wege_df = as.data.frame(table(mammals_out_deciles_wege$category))
colnames(mammals_out_deciles_wege_df) = c('Category','m_wege')

mammals_out_irreplaceability = read.csv('sp_list_out_m_hotspot_ferrier_m.csv')
mammals_out_irreplaceability_df = as.data.frame(table(mammals_out_irreplaceability$category))
colnames(mammals_out_irreplaceability_df) = c('Category','m_irr')

mammals_out_irreplaceability_hot = read.csv('sp_list_out_m_hotspot_ferrier_m_hot.csv')
mammals_out_irreplaceability_hot_df = as.data.frame(table(mammals_out_irreplaceability_hot$category))
colnames(mammals_out_irreplaceability_hot_df) = c('Category','m_irr_h')


birds_out_deciles_wege = read.csv('sp_list_out_wege2_b_d.csv')
birds_out_deciles_wege_df = as.data.frame(table(birds_out_deciles_wege$category))
colnames(birds_out_deciles_wege_df) = c('Category','b_wege')

birds_out_irreplaceability = read.csv('sp_list_out_b_hotspot_ferrier_b.csv')
birds_out_irreplaceability_df = as.data.frame(table(birds_out_irreplaceability$category))
colnames(birds_out_irreplaceability_df) = c('Category','b_irr')

birds_out_irreplaceability_hot = read.csv('sp_list_out_b_hotspot_ferrier_b_hot.csv')
birds_out_irreplaceability_hot_df = as.data.frame(table(birds_out_irreplaceability_hot$category))
colnames(birds_out_irreplaceability_hot_df) = c('Category','b_irr_h')


nrow(r_grid[!is.na(r_grid$ferrier_a_hot) & r_grid$ferrier_a_hot>0,])/nrow(r_grid[r_grid$land == 1,])

library(ggplot2)

pal = colorRampPalette(hcl.colors(5,'Viridis'))


p_a = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_a),fill = log10(ferrier_a)),linewidth = 0)+
  scale_fill_gradientn('ferrier_a',colours = pal(10))+
  scale_colour_gradientn('ferrier_a',colours = pal(10))+
  theme_void()

p_a_h = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_a_hot),fill = log10(ferrier_a_hot)),linewidth = 0)+
  scale_fill_gradientn('ferrier_a_hot',colours = pal(10))+
  scale_colour_gradientn('ferrier_a_hot',colours = pal(10))+
  theme_void()


p_a_w = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],bg = 'grey70',col = 'grey70')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$wege2_a_d %in%10,],aes(col = log10(wege2_a),fill = log10(wege2_a)),linewidth = 0)+
  scale_fill_gradientn('wege_a_d',colours = pal(10))+
  scale_colour_gradientn('wege_a_d',colours = pal(10))+
  theme_void()

  
p_r = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_r),fill = log10(ferrier_r)),linewidth = 0)+
  scale_fill_gradientn('ferrier_r',colours = pal(10))+
  scale_colour_gradientn('ferrier_r',colours = pal(10))+
  theme_void()

p_r_h = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_r_hot),fill = log10(ferrier_r_hot)),linewidth = 0)+
  scale_fill_gradientn('ferrier_r_hot',colours = pal(10))+
  scale_colour_gradientn('ferrier_r_hot',colours = pal(10))+
  theme_void()

p_r_w = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],bg = 'grey70',col = 'grey70')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$wege2_r_d %in%10,],aes(col = log10(wege2_r),fill = log10(wege2_r)),linewidth = 0)+
  scale_fill_gradientn('wege_r_d',colours = pal(10))+
  scale_colour_gradientn('wege_r_d',colours = pal(10))+
  theme_void()

p_m = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_m),fill = log10(ferrier_m)),linewidth = 0)+
  scale_fill_gradientn('ferrier_m',colours = pal(10))+
  scale_colour_gradientn('ferrier_m',colours = pal(10))+
  theme_void()

p_m_h = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_m_hot),fill = log10(ferrier_m_hot)),linewidth = 0)+
  scale_fill_gradientn('ferrier_m_hot',colours = pal(10))+
  scale_colour_gradientn('ferrier_m_hot',colours = pal(10))+
  theme_void()

p_m_w = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],bg = 'grey70',col = 'grey70')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$wege2_m_d %in%10,],aes(col = log10(wege2_m),fill = log10(wege2_m)),linewidth = 0)+
  scale_fill_gradientn('wege_m_d',colours = pal(10))+
  scale_colour_gradientn('wege_m_d',colours = pal(10))+
  theme_void()

p_b = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_b),fill = log10(ferrier_b)),linewidth = 0)+
  scale_fill_gradientn('ferrier_b',colours = pal(10))+
  scale_colour_gradientn('ferrier_b',colours = pal(10))+
  theme_void()

p_b_h = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],aes(col = log10(ferrier_b_hot),fill = log10(ferrier_b_hot)),linewidth = 0)+
  scale_fill_gradientn('ferrier_b_hot',colours = pal(10))+
  scale_colour_gradientn('ferrier_b_hot',colours = pal(10))+
  theme_void()

p_b_w = ggplot()+
  geom_sf(data = r_grid[r_grid$land==1,],bg = 'grey70',col = 'grey70')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$wege2_b_d %in%10,],aes(col = log10(wege2_b),fill = log10(wege2_b)),linewidth = 0)+
  scale_fill_gradientn('wege_b_d',colours = pal(10))+
  scale_colour_gradientn('wege_b_d',colours = pal(10))+
  theme_void()

library(gridExtra)


grid = arrangeGrob(p_a,p_a_h,p_a_w,
                   p_r,p_r_h,p_r_w,
                   p_m,p_m_h,p_m_w,
                   p_b,p_b_h,p_b_w,
                   ncol = 3)


ggsave(filename = 'ferrier_comparison.png',plot = grid,width = 12,height = 8,dpi = 2000)







  

