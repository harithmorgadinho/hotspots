library(sf)
sf_use_s2(F)
library(ggplot2)
load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/r_grid_metrics_run2_deciles_wallace_dec12.Rdata')


p1 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_a,col = agriculture_a),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_a',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_a',colours = rainbow(10))


r_grid$agriculture_a_unc_med = r_grid$agriculture_a_unc_med/max(na.omit(r_grid$agriculture_a_unc_med))
  
  
p2 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_a_unc_med,col = agriculture_a_unc_med),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_a_unc_med',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_a_unc_med',colours = rainbow(10))


r_grid$richness_a_normalized = r_grid$richness_a/max(na.omit(r_grid$richness_a))

p3 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = richness_a_normalized,col = richness_a_normalized),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('richness_a_normalized',colours = rainbow(10))+
  scale_colour_gradientn('richness_a_normalized',colours = rainbow(10))



p4 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1 & r_grid$richness_a_normalized <0.25 & r_grid$agriculture_a_unc_med > 0.75,],aes(fill = agriculture_a,col = agriculture_a),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('test1',colours = rainbow(10))+
scale_colour_gradientn('test1',colours = rainbow(10))


library(gridExtra)
grid = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(plot = grid,filename = 'grid_test_unce.png',width = 10,height = 8)

### mammals

p1 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_m,col = agriculture_m),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_m',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_m',colours = rainbow(10))


r_grid$agriculture_m_unc_med = r_grid$agriculture_m_unc_med/max(na.omit(r_grid$agriculture_m_unc_med))


p2 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_m_unc_med,col = agriculture_m_unc_med),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_m_unc_med',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_m_unc_med',colours = rainbow(10))


r_grid$richness_m_normalized = r_grid$richness_m/max(na.omit(r_grid$richness_m))

p3 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = richness_m_normalized,col = richness_m_normalized),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('richness_m_normalized',colours = rainbow(10))+
  scale_colour_gradientn('richness_m_normalized',colours = rainbow(10))



p4 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1 & r_grid$richness_m_normalized <0.25 & r_grid$agriculture_m_unc_med > 0.75,],aes(fill = agriculture_m,col = agriculture_m),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('test1',colours = rainbow(10))+
  scale_colour_gradientn('test1',colours = rainbow(10))


library(gridExtra)
grid = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(plot = grid,filename = 'grid_test_unce_m.png',width = 10,height = 8)

###
### reptiles

p1 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_r,col = agriculture_r),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_r',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_r',colours = rainbow(10))


r_grid$agriculture_r_unc_med = r_grid$agriculture_r_unc_med/max(na.omit(r_grid$agriculture_r_unc_med))


p2 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = agriculture_r_unc_sum,col = agriculture_r_unc_sum),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('agriculture_r_unc_med',colours = rainbow(10))+
  scale_colour_gradientn('agriculture_r_unc_med',colours = rainbow(10))


r_grid$richness_r_normalized = r_grid$richness_r/max(na.omit(r_grid$richness_r))

p3 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1,],aes(fill = richness_r_normalized,col = richness_r_normalized),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('richness_r_normalized',colours = rainbow(10))+
  scale_colour_gradientn('richness_r_normalized',colours = rainbow(10))


p4 = ggplot()+
  geom_sf(data = r_grid[r_grid$land%in%1, ],aes(fill = agriculture_r_unc_med/richness_r,col = agriculture_r_unc_med/richness_r),linewidth = 0)+
  theme_void()+
  scale_fill_gradientn('test1',colours = rainbow(10))+
  scale_colour_gradientn('test1',colours = rainbow(10))


library(gridExtra)
grid = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(plot = grid,filename = 'grid_test_unce_r.png',width = 10,height = 8)

