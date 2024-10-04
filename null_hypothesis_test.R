colnames(r_grid)

load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_DEC4.Rdata')

load('r_grid_all_fixed_17_Sep_2024.Rdata')


table(r_grid_soc_eco$HOTSPOT_numeric)
colnames(r_grid_soc_eco)

hotspot_cells_original=r_grid_soc_eco[r_grid_soc_eco$land%in% 1 & r_grid_soc_eco$HOTSPOT_SUMM_numeric >0,]$layer

table(r_grid$all_cum_threats)

r_grid$hotspot_cells = ifelse(r_grid$all_cum_threats>0,1,0)

table(r_grid$hotspots_all)

all_terrestrial_cells = r_grid[r_grid$land %in% 1,]$layer
hotspot_cells = r_grid[r_grid$land %in% 1 & r_grid$hotspot_cells %in% 1,]$layer


list_intersects = list()
for(i in seq_along(1:1000)){
  temp=sample(x = all_terrestrial_cells,replace = F,size = length(hotspot_cells))
  temp=length(intersect(temp,hotspot_cells_original))
  list_intersects[i] = temp
}

null_hypothesis = unlist(list_intersects)

hist(null_hypothesis)

your_intersection_count <- length(intersect(hotspot_cells_original, hotspot_cells))

z_score <- (your_intersection_count - mean(null_hypothesis)) / sd(null_hypothesis)

library(ggplot2)

df = cbind.data.frame(x = 1:1000,y = null_hypothesis)
ggplot()+
  geom_boxplot(data = df, aes(x = 1,y = null_hypothesis))+
  geom_hline(yintercept = your_intersection_count)

ggplot()+
  geom_histogram(aes(x = null_hypothesis),bins = 100)+
  geom_vline(xintercept = your_intersection_count)


length(hotspot_cells)/length(hotspot_cells_original)

length(setdiff(hotspot_cells,hotspot_cells_original))

mean(null_hypothesis)/length(hotspot_cells)

length(hotspot_cells)/length(all_terrestrial_cells)

length(hotspot_cells_original)/length(all_terrestrial_cells)

colnames(r_grid)

nrow(r_grid[r_grid$a_cum_threats %in% 1:7,])/length(hotspot_cells)
nrow(r_grid[r_grid$b_cum_threats %in% 1:7,])/length(hotspot_cells)
nrow(r_grid[r_grid$m_cum_threats %in% 1:7,])/length(hotspot_cells)
nrow(r_grid[r_grid$r_cum_threats %in% 1:7,])/length(hotspot_cells)

table(r_grid$a_cum_threats)
table(r_grid$b_cum_threats)
table(r_grid$m_cum_threats)
table(r_grid$r_cum_threats)

table(r_grid$cum_agriculture)
table(r_grid$cum_climate_change)
table(r_grid$cum_hunting)
table(r_grid$cum_pollution)
table(r_grid$cum_invasives)
table(r_grid$cum_urbanization)
table(r_grid$cum_logging)



nrow(r_grid[r_grid$cum_agriculture %in% 2:4,])/nrow(r_grid[r_grid$cum_agriculture %in% 1:4,])
nrow(r_grid[r_grid$cum_climate_change %in% 2:4,])/nrow(r_grid[r_grid$cum_climate_change %in% 1:4,])
nrow(r_grid[r_grid$cum_hunting %in% 2:4,])/nrow(r_grid[r_grid$cum_hunting %in% 1:4,])
nrow(r_grid[r_grid$cum_pollution %in% 2:4,])/nrow(r_grid[r_grid$cum_pollution %in% 1:4,])
nrow(r_grid[r_grid$cum_invasives %in% 2:4,])/nrow(r_grid[r_grid$cum_invasives %in% 1:4,])
nrow(r_grid[r_grid$cum_logging %in% 2:4,])/nrow(r_grid[r_grid$cum_logging %in% 1:4,])
nrow(r_grid[r_grid$cum_urbanization %in% 2:4,])/nrow(r_grid[r_grid$cum_urbanization %in% 1:4,])

nrow(r_grid[r_grid$cum_logging %in% 1:4,])/length(hotspot_cells)
nrow(r_grid[r_grid$cum_agriculture %in% 1:4,])/length(hotspot_cells)
nrow(r_grid[r_grid$cum_urbanization %in% 1:4,])/length(hotspot_cells)

nrow(r_grid[r_grid$cum_invasives %in% 1:4,])/length(hotspot_cells)
nrow(r_grid[r_grid$cum_hunting %in% 1:4,])/length(hotspot_cells)
nrow(r_grid[r_grid$cum_climate_change %in% 1:4,])/length(hotspot_cells)
nrow(r_grid[r_grid$cum_pollution %in% 1:4,])/length(hotspot_cells)

r_grid %>% filter(cum_logging %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_agriculture %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_urbanization %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_invasives %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_hunting %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_climate_change %in% 1:4) %>% st_area() %>% sum()/1000000
r_grid %>% filter(cum_pollution %in% 1:4) %>% st_area() %>% sum()/1000000



table(r_grid$cum_urbanization)
table(r_grid$cum_agriculture)

table(r_grid$agriculture_a_hot)
table(r_grid$agriculture_m_hot)
table(r_grid$agriculture_b_hot)
table(r_grid$agriculture_r_hot)




