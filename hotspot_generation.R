#hotspot generation

#amphibians----
r_grid$agriculture_a_hot = NA
r_grid[r_grid$agriculture_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$agriculture_a_hot = 1

r_grid$logging_a_hot = NA
r_grid[r_grid$logging_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$logging_a_hot = 1

r_grid$hunting_a_hot = NA
r_grid[r_grid$hunting_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$hunting_a_hot = 1

r_grid$pollution_a_hot = NA
r_grid[r_grid$pollution_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$pollution_a_hot = 1

r_grid$invasives_a_hot = NA
r_grid[r_grid$invasives_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$invasives_a_hot = 1

r_grid$climate_change_a_hot = NA
r_grid[r_grid$climate_change_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$climate_change_a_hot = 1

r_grid$urbanization_a_hot = NA
r_grid[r_grid$urbanization_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$urbanization_a_hot = 1

#reptiles----
r_grid$agriculture_r_hot = NA
r_grid[r_grid$agriculture_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$agriculture_r_hot = 1

r_grid$logging_r_hot = NA
r_grid[r_grid$logging_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$logging_r_hot = 1

r_grid$hunting_r_hot = NA
r_grid[r_grid$hunting_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$hunting_r_hot = 1

r_grid$pollution_r_hot = NA
r_grid[r_grid$pollution_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$pollution_r_hot = 1

r_grid$invasives_r_hot = NA
r_grid[r_grid$invasives_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$invasives_r_hot = 1

r_grid$climate_change_r_hot = NA
r_grid[r_grid$climate_change_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$climate_change_r_hot = 1

r_grid$urbanization_r_hot = NA
r_grid[r_grid$urbanization_r_d %in% 10 & r_grid$wege2_r_d %in% 10,]$urbanization_r_hot = 1

#mammals----
r_grid$agriculture_m_hot = NA
r_grid[r_grid$agriculture_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$agriculture_m_hot = 1

r_grid$logging_m_hot = NA
r_grid[r_grid$logging_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$logging_m_hot = 1

r_grid$hunting_m_hot = NA
r_grid[r_grid$hunting_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$hunting_m_hot = 1

r_grid$pollution_m_hot = NA
r_grid[r_grid$pollution_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$pollution_m_hot = 1

r_grid$invasives_m_hot = NA
r_grid[r_grid$invasives_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$invasives_m_hot = 1

r_grid$climate_change_m_hot = NA
r_grid[r_grid$climate_change_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$climate_change_m_hot = 1

r_grid$urbanization_m_hot = NA
r_grid[r_grid$urbanization_m_d %in% 10 & r_grid$wege2_m_d %in% 10,]$urbanization_m_hot = 1

#birds ----
r_grid$agriculture_b_hot = NA
r_grid[r_grid$agriculture_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$agriculture_b_hot = 1

r_grid$logging_b_hot = NA
r_grid[r_grid$logging_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$logging_b_hot = 1

r_grid$hunting_b_hot = NA
r_grid[r_grid$hunting_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$hunting_b_hot = 1

r_grid$pollution_b_hot = NA
r_grid[r_grid$pollution_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$pollution_b_hot = 1

r_grid$invasives_b_hot = NA
r_grid[r_grid$invasives_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$invasives_b_hot = 1

r_grid$climate_change_b_hot = NA
r_grid[r_grid$climate_change_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$climate_change_b_hot = 1

r_grid$urbanization_b_hot = NA
r_grid[r_grid$urbanization_b_d %in% 10 & r_grid$wege2_b_d %in% 10,]$urbanization_b_hot = 1







r_grid$hotspot_any_threat_a = NA
r_grid[r_grid$agriculture_a_hot %in% 1 | r_grid$logging_a_hot %in% 1 
       |r_grid$hunting_a_hot %in% 1 |r_grid$pollution_a_hot %in% 1 
       |r_grid$invasives_a_hot %in% 1 |r_grid$climate_change_a_hot %in% 1 
       |r_grid$urbanization_a_hot %in% 1 ,]$hotspot_any_threat_a = 1

r_grid$hotspot_any_threat_r = NA
r_grid[r_grid$agriculture_r_hot %in% 1 | r_grid$logging_r_hot %in% 1 
       |r_grid$hunting_r_hot %in% 1 |r_grid$pollution_r_hot %in% 1 
       |r_grid$invasives_r_hot %in% 1 |r_grid$climate_change_r_hot %in% 1 
       |r_grid$urbanization_r_hot %in% 1 ,]$hotspot_any_threat_r = 1

r_grid$hotspot_any_threat_m = NA
r_grid[r_grid$agriculture_m_hot %in% 1 | r_grid$logging_m_hot %in% 1 
       |r_grid$hunting_m_hot %in% 1 |r_grid$pollution_m_hot %in% 1 
       |r_grid$invasives_m_hot %in% 1 |r_grid$climate_change_m_hot %in% 1 
       |r_grid$urbanization_m_hot %in% 1 ,]$hotspot_any_threat_m = 1

r_grid$hotspot_any_threat_b = NA
r_grid[r_grid$agriculture_b_hot %in% 1 | r_grid$logging_b_hot %in% 1 
       |r_grid$hunting_b_hot %in% 1 |r_grid$pollution_b_hot %in% 1 
       |r_grid$invasives_b_hot %in% 1 |r_grid$climate_change_b_hot %in% 1 
       |r_grid$urbanization_b_hot %in% 1 ,]$hotspot_any_threat_b = 1



r_grid$hotspots_all = NA
r_grid[r_grid$hotspot_any_threat_a %in% 1 | r_grid$hotspot_any_threat_r %in% 1 
       |r_grid$hotspot_any_threat_m %in% 1 |r_grid$hotspot_any_threat_b %in% 1,]$hotspots_all = 1


save(r_grid,file = 'r_grid_metrics_run2_deciles.Rdata')


nrow(r_grid[r_grid$hotspots_all %in%1 & r_grid$land%in%1,])/nrow(r_grid[r_grid$land%in%1,])

plot(r_grid[r_grid$hotspots_all %in%1 ,]$geometry)


plot(r_grid[r_grid$ferrier_b >0 ,]$geometry)



save(r_grid,file = 'r_grid_metrics_run2_deciles_nov_27.Rdata')


save(r_grid, file = 'r_grid_all_fixed_19_Jan.Rdata')
 


