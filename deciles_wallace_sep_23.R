load('r_grid_all_fixed_17_Sep_2024.Rdata')
#plotting wallace regions
#load('r_grid_metrics_run2_deciles_nov_27.Rdata')
load('/Users/gdt366/Dropbox/Postdoc_socioeconomic/r_grid_soc_eco_NOV26.Rdata')




# Load the necessary library
library(dplyr)


r_grid$NEW_REALM = r_grid_soc_eco$NEW_REALM

# p = ggplot()+
#   geom_sf(data = r_grid,aes(fill = NEW_REALM,col = NEW_REALM))
# 
# ggsave('p_test.png')

get_decile_group <- function(x) {
  if(is.na(x) || x == 0) return(NA)
  sum(x > deciles) # This returns the decile group
}


#wege----



realm_list = unique(na.omit(r_grid_soc_eco$NEW_REALM))
r_grid$wege2_a_d_NEW_REALM = 0
r_grid$wege2_r_d_NEW_REALM = 0
r_grid$wege2_m_d_NEW_REALM = 0
r_grid$wege2_b_d_NEW_REALM = 0

for(i in seq_along(realm_list)){
  print(realm_list[i])
  valid_x = r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_a) & r_grid$wege2_a != 0 & r_grid$land==1,]$wege2_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_a) & r_grid$wege2_a != 0 & r_grid$land==1,]$wege2_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  valid_x = r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_r) & r_grid$wege2_r != 0 & r_grid$land==1,]$wege2_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_r) & r_grid$wege2_r != 0 & r_grid$land==1,]$wege2_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x = r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_m) & r_grid$wege2_m != 0 & r_grid$land==1,]$wege2_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_m) & r_grid$wege2_m != 0 & r_grid$land==1,]$wege2_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  valid_x = r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_b) & r_grid$wege2_b != 0 & r_grid$land==1,]$wege2_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$wege2_b) & r_grid$wege2_b != 0 & r_grid$land==1,]$wege2_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
}



r_grid$agriculture_a_d_NEW_REALM = 0
r_grid$logging_a_d_NEW_REALM = 0
r_grid$hunting_a_d_NEW_REALM = 0
r_grid$pollution_a_d_NEW_REALM =   0
r_grid$invasives_a_d_NEW_REALM = 0
r_grid$climate_change_a_d_NEW_REALM = 0
r_grid$urbanization_a_d_NEW_REALM =   0

r_grid$agriculture_r_d_NEW_REALM = 0
r_grid$logging_r_d_NEW_REALM = 0
r_grid$hunting_r_d_NEW_REALM = 0
r_grid$pollution_r_d_NEW_REALM =   0
r_grid$invasives_r_d_NEW_REALM = 0
r_grid$climate_change_r_d_NEW_REALM = 0
r_grid$urbanization_r_d_NEW_REALM =   0

r_grid$agriculture_m_d_NEW_REALM = 0
r_grid$logging_m_d_NEW_REALM = 0
r_grid$hunting_m_d_NEW_REALM = 0
r_grid$pollution_m_d_NEW_REALM =   0
r_grid$invasives_m_d_NEW_REALM = 0
r_grid$climate_change_m_d_NEW_REALM = 0
r_grid$urbanization_m_d_NEW_REALM =   0

r_grid$agriculture_b_d_NEW_REALM = 0
r_grid$logging_b_d_NEW_REALM = 0
r_grid$hunting_b_d_NEW_REALM = 0
r_grid$pollution_b_d_NEW_REALM =   0
r_grid$invasives_b_d_NEW_REALM = 0
r_grid$climate_change_b_d_NEW_REALM = 0
r_grid$urbanization_b_d_NEW_REALM =   0



for(i in seq_along(realm_list)){
  print(realm_list[i])
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_a) & r_grid$agriculture_a != 0,]$agriculture_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_a) & r_grid$agriculture_a != 0,]$agriculture_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_a) & r_grid$logging_a != 0,]$logging_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_a) & r_grid$logging_a != 0,]$logging_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_a) & r_grid$hunting_a != 0,]$hunting_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_a) & r_grid$hunting_a != 0,]$hunting_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_a) & r_grid$pollution_a != 0,]$pollution_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_a) & r_grid$pollution_a != 0,]$pollution_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_a) & r_grid$invasives_a != 0,]$invasives_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_a) & r_grid$invasives_a != 0,]$invasives_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_a) & r_grid$climate_change_a != 0,]$climate_change_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_a) & r_grid$climate_change_a != 0,]$climate_change_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$urbanization_a) & r_grid$urbanization_a != 0,]$urbanization_a
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_a) & r_grid$climate_change_a != 0,]$urbanization_a_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  # threats reptiles ----
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_r) & r_grid$agriculture_r != 0,]$agriculture_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_r) & r_grid$agriculture_r != 0,]$agriculture_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_r) & r_grid$logging_r != 0,]$logging_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_r) & r_grid$logging_r != 0,]$logging_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_r) & r_grid$hunting_r != 0,]$hunting_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_r) & r_grid$hunting_r != 0,]$hunting_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_r) & r_grid$pollution_r != 0,]$pollution_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_r) & r_grid$pollution_r != 0,]$pollution_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_r) & r_grid$invasives_r != 0,]$invasives_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_r) & r_grid$invasives_r != 0,]$invasives_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_r) & r_grid$climate_change_r != 0,]$climate_change_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_r) & r_grid$climate_change_r != 0,]$climate_change_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$urbanization_r) & r_grid$urbanization_r != 0,]$urbanization_r
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_r) & r_grid$climate_change_r != 0,]$urbanization_r_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  # threats mammals ----
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_m) & r_grid$agriculture_m != 0,]$agriculture_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_m) & r_grid$agriculture_m != 0,]$agriculture_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_m) & r_grid$logging_m != 0,]$logging_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_m) & r_grid$logging_m != 0,]$logging_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_m) & r_grid$hunting_m != 0,]$hunting_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_m) & r_grid$hunting_m != 0,]$hunting_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_m) & r_grid$pollution_m != 0,]$pollution_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_m) & r_grid$pollution_m != 0,]$pollution_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_m) & r_grid$invasives_m != 0,]$invasives_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_m) & r_grid$invasives_m != 0,]$invasives_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_m) & r_grid$climate_change_m != 0,]$climate_change_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_m) & r_grid$climate_change_m != 0,]$climate_change_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$urbanization_m) & r_grid$urbanization_m != 0,]$urbanization_m
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_m) & r_grid$climate_change_m != 0,]$urbanization_m_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  # threats birds ----
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_b) & r_grid$agriculture_b != 0,]$agriculture_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] & !is.na(r_grid$agriculture_b) & r_grid$agriculture_b != 0,]$agriculture_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_b) & r_grid$logging_b != 0,]$logging_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$logging_b) & r_grid$logging_b != 0,]$logging_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_b) & r_grid$hunting_b != 0,]$hunting_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$hunting_b) & r_grid$hunting_b != 0,]$hunting_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_b) & r_grid$pollution_b != 0,]$pollution_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$pollution_b) & r_grid$pollution_b != 0,]$pollution_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_b) & r_grid$invasives_b != 0,]$invasives_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$invasives_b) & r_grid$invasives_b != 0,]$invasives_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_b) & r_grid$climate_change_b != 0,]$climate_change_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_b) & r_grid$climate_change_b != 0,]$climate_change_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
  
  valid_x <- r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$urbanization_b) & r_grid$urbanization_b != 0,]$urbanization_b
  deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
  r_grid[r_grid$NEW_REALM %in% realm_list[i] &!is.na(r_grid$climate_change_b) & r_grid$climate_change_b != 0,]$urbanization_b_d_NEW_REALM = sapply(valid_x, get_decile_group)
  
}






save(r_grid,file = 'r_grid_metrics_run2_deciles_wallace_sep_23.Rdata')


load('r_grid_metrics_run2_deciles_wallace_sep_23.Rdata')
#amphibians----
r_grid$agriculture_a_hot_NEW_REALM = NA
r_grid[r_grid$agriculture_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$agriculture_a_hot_NEW_REALM = 1

r_grid$logging_a_hot_NEW_REALM = NA
r_grid[r_grid$logging_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$logging_a_hot_NEW_REALM = 1

r_grid$hunting_a_hot_NEW_REALM = NA
r_grid[r_grid$hunting_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$hunting_a_hot_NEW_REALM = 1

r_grid$pollution_a_hot_NEW_REALM = NA
r_grid[r_grid$pollution_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$pollution_a_hot_NEW_REALM = 1

r_grid$invasives_a_hot_NEW_REALM = NA
r_grid[r_grid$invasives_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$invasives_a_hot_NEW_REALM = 1

r_grid$climate_change_a_hot_NEW_REALM = NA
r_grid[r_grid$climate_change_a_d_NEW_REALM %in% 10 & r_grid$wege2_a_d_NEW_REALM %in% 10,]$climate_change_a_hot_NEW_REALM = 1

r_grid$urbanization_a_hot_NEW_REALM = NA
r_grid[r_grid$urbanization_a_d %in% 10 & r_grid$wege2_a_d %in% 10,]$urbanization_a_hot_NEW_REALM = 1



#reptiles----
r_grid$agriculture_r_hot_NEW_REALM = NA
r_grid[r_grid$agriculture_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$agriculture_r_hot_NEW_REALM = 1

r_grid$logging_r_hot_NEW_REALM = NA
r_grid[r_grid$logging_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$logging_r_hot_NEW_REALM = 1

r_grid$hunting_r_hot_NEW_REALM = NA
r_grid[r_grid$hunting_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$hunting_r_hot_NEW_REALM = 1

r_grid$pollution_r_hot_NEW_REALM = NA
r_grid[r_grid$pollution_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$pollution_r_hot_NEW_REALM = 1

r_grid$invasives_r_hot_NEW_REALM = NA
r_grid[r_grid$invasives_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$invasives_r_hot_NEW_REALM = 1

r_grid$climate_change_r_hot_NEW_REALM = NA
r_grid[r_grid$climate_change_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$climate_change_r_hot_NEW_REALM = 1

r_grid$urbanization_r_hot_NEW_REALM = NA
r_grid[r_grid$urbanization_r_d_NEW_REALM %in% 10 & r_grid$wege2_r_d_NEW_REALM %in% 10,]$urbanization_r_hot_NEW_REALM = 1

#mammals----
r_grid$agriculture_m_hot_NEW_REALM = NA
r_grid[r_grid$agriculture_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$agriculture_m_hot_NEW_REALM = 1

r_grid$logging_m_hot_NEW_REALM = NA
r_grid[r_grid$logging_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$logging_m_hot_NEW_REALM = 1

r_grid$hunting_m_hot_NEW_REALM = NA
r_grid[r_grid$hunting_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$hunting_m_hot_NEW_REALM = 1

table(r_grid$pollution_m_d_NEW_REALM)
table(r_grid$wege2_m_d_NEW_REALM)

r_grid$pollution_m_hot_NEW_REALM = NA
r_grid[r_grid$pollution_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$pollution_m_hot_NEW_REALM = 1

r_grid$invasives_m_hot_NEW_REALM = NA
r_grid[r_grid$invasives_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$invasives_m_hot_NEW_REALM = 1

r_grid$climate_change_m_hot_NEW_REALM = NA
r_grid[r_grid$climate_change_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$climate_change_m_hot_NEW_REALM = 1

r_grid$urbanization_m_hot_NEW_REALM = NA
r_grid[r_grid$urbanization_m_d_NEW_REALM %in% 10 & r_grid$wege2_m_d_NEW_REALM %in% 10,]$urbanization_m_hot_NEW_REALM = 1

#birds ----
r_grid$agriculture_b_hot_NEW_REALM = NA
r_grid[r_grid$agriculture_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$agriculture_b_hot_NEW_REALM = 1

r_grid$logging_b_hot_NEW_REALM = NA
r_grid[r_grid$logging_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$logging_b_hot_NEW_REALM = 1

r_grid$hunting_b_hot_NEW_REALM = NA
r_grid[r_grid$hunting_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$hunting_b_hot_NEW_REALM = 1

r_grid$pollution_b_hot_NEW_REALM = NA
r_grid[r_grid$pollution_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$pollution_b_hot_NEW_REALM = 1

r_grid$invasives_b_hot_NEW_REALM = NA
r_grid[r_grid$invasives_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$invasives_b_hot_NEW_REALM = 1

r_grid$climate_change_b_hot_NEW_REALM = NA
r_grid[r_grid$climate_change_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$climate_change_b_hot_NEW_REALM = 1

r_grid$urbanization_b_hot_NEW_REALM = NA
r_grid[r_grid$urbanization_b_d_NEW_REALM %in% 10 & r_grid$wege2_b_d_NEW_REALM %in% 10,]$urbanization_b_hot_NEW_REALM = 1



r_grid$hotspot_any_threat_a_NEW_REALM = NA
r_grid[r_grid$agriculture_a_hot_NEW_REALM %in% 1 | r_grid$logging_a_hot_NEW_REALM %in% 1 
       |r_grid$hunting_a_hot_NEW_REALM %in% 1 |r_grid$pollution_a_hot_NEW_REALM %in% 1 
       |r_grid$invasives_a_hot_NEW_REALM %in% 1 |r_grid$climate_change_a_hot_NEW_REALM %in% 1 
       |r_grid$urbanization_a_hot_NEW_REALM %in% 1 ,]$hotspot_any_threat_a_NEW_REALM = 1



r_grid$hotspot_any_threat_r_NEW_REALM = NA
r_grid[r_grid$agriculture_r_hot_NEW_REALM %in% 1 | r_grid$logging_r_hot_NEW_REALM %in% 1 
       |r_grid$hunting_r_hot_NEW_REALM %in% 1 |r_grid$pollution_r_hot_NEW_REALM %in% 1 
       |r_grid$invasives_r_hot_NEW_REALM %in% 1 |r_grid$climate_change_r_hot_NEW_REALM %in% 1 
       |r_grid$urbanization_r_hot_NEW_REALM %in% 1 ,]$hotspot_any_threat_r_NEW_REALM = 1

r_grid$hotspot_any_threat_m_NEW_REALM = NA
r_grid[r_grid$agriculture_m_hot_NEW_REALM %in% 1 | r_grid$logging_m_hot_NEW_REALM %in% 1 
       |r_grid$hunting_m_hot_NEW_REALM %in% 1 |r_grid$pollution_m_hot_NEW_REALM %in% 1 
       |r_grid$invasives_m_hot_NEW_REALM %in% 1 |r_grid$climate_change_m_hot_NEW_REALM %in% 1 
       |r_grid$urbanization_m_hot_NEW_REALM %in% 1 ,]$hotspot_any_threat_m_NEW_REALM = 1

r_grid$hotspot_any_threat_b_NEW_REALM = NA
r_grid[r_grid$agriculture_b_hot_NEW_REALM %in% 1 | r_grid$logging_b_hot_NEW_REALM %in% 1 
       |r_grid$hunting_b_hot_NEW_REALM %in% 1 |r_grid$pollution_b_hot_NEW_REALM %in% 1 
       |r_grid$invasives_b_hot_NEW_REALM %in% 1 |r_grid$climate_change_b_hot_NEW_REALM %in% 1 
       |r_grid$urbanization_b_hot_NEW_REALM %in% 1 ,]$hotspot_any_threat_b_NEW_REALM = 1



r_grid$hotspots_all_NEW_REALM = NA
r_grid[r_grid$hotspot_any_threat_a_NEW_REALM %in% 1 | r_grid$hotspot_any_threat_r_NEW_REALM %in% 1 
       |r_grid$hotspot_any_threat_m_NEW_REALM %in% 1 |r_grid$hotspot_any_threat_b_NEW_REALM %in% 1,]$hotspots_all_NEW_REALM = 1


#amphibians
r_grid[r_grid$land ==1 & !r_grid$agriculture_a_hot_NEW_REALM%in%1,]$agriculture_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$logging_a_hot_NEW_REALM%in%1,]$logging_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_a_hot_NEW_REALM%in%1,]$hunting_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_a_hot_NEW_REALM%in%1,]$pollution_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_a_hot_NEW_REALM%in%1,]$invasives_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_a_hot_NEW_REALM%in%1,]$climate_change_a_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_a_hot_NEW_REALM%in%1,]$urbanization_a_hot_NEW_REALM = 0
r_grid$a_cum_threats_NEW_REALM = NA
r_grid$a_cum_threats_NEW_REALM = r_grid$agriculture_a_hot_NEW_REALM+r_grid$logging_a_hot_NEW_REALM+r_grid$hunting_a_hot_NEW_REALM+r_grid$pollution_a_hot_NEW_REALM+r_grid$invasives_a_hot_NEW_REALM+
  r_grid$climate_change_a_hot_NEW_REALM +r_grid$urbanization_a_hot_NEW_REALM

#reptiles
r_grid[r_grid$land ==1 & !r_grid$agriculture_r_hot_NEW_REALM%in%1,]$agriculture_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$logging_r_hot_NEW_REALM%in%1,]$logging_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_r_hot_NEW_REALM%in%1,]$hunting_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_r_hot_NEW_REALM%in%1,]$pollution_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_r_hot_NEW_REALM%in%1,]$invasives_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_r_hot_NEW_REALM%in%1,]$climate_change_r_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_r_hot_NEW_REALM%in%1,]$urbanization_r_hot_NEW_REALM = 0

r_grid$r_cum_threats_NEW_REALM = NA
r_grid$r_cum_threats_NEW_REALM = r_grid$agriculture_r_hot_NEW_REALM+r_grid$logging_r_hot_NEW_REALM+r_grid$hunting_r_hot_NEW_REALM+r_grid$pollution_r_hot_NEW_REALM+r_grid$invasives_r_hot_NEW_REALM+
  r_grid$climate_change_r_hot_NEW_REALM+r_grid$urbanization_r_hot_NEW_REALM

table(r_grid$r_cum_threats_NEW_REALM)

#mammals
r_grid[r_grid$land ==1 & !r_grid$agriculture_m_hot_NEW_REALM%in%1,]$agriculture_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$logging_m_hot_NEW_REALM%in%1,]$logging_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_m_hot_NEW_REALM%in%1,]$hunting_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_m_hot_NEW_REALM%in%1,]$pollution_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_m_hot_NEW_REALM%in%1,]$invasives_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_m_hot_NEW_REALM%in%1,]$climate_change_m_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_m_hot_NEW_REALM%in%1,]$urbanization_m_hot_NEW_REALM = 0

r_grid$m_cum_threats_NEW_REALM = NA
r_grid$m_cum_threats_NEW_REALM = r_grid$agriculture_m_hot_NEW_REALM+r_grid$logging_m_hot_NEW_REALM+r_grid$hunting_m_hot_NEW_REALM+r_grid$pollution_m_hot_NEW_REALM+r_grid$invasives_m_hot_NEW_REALM+
  r_grid$climate_change_m_hot_NEW_REALM+r_grid$urbanization_m_hot_NEW_REALM

#birds
r_grid[r_grid$land ==1 & !r_grid$agriculture_b_hot_NEW_REALM%in%1,]$agriculture_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$logging_b_hot_NEW_REALM%in%1,]$logging_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$hunting_b_hot_NEW_REALM%in%1,]$hunting_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$pollution_b_hot_NEW_REALM%in%1,]$pollution_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$invasives_b_hot_NEW_REALM%in%1,]$invasives_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$climate_change_b_hot_NEW_REALM%in%1,]$climate_change_b_hot_NEW_REALM = 0
r_grid[r_grid$land ==1 & !r_grid$urbanization_b_hot_NEW_REALM%in%1,]$urbanization_b_hot_NEW_REALM = 0

r_grid$b_cum_threats_NEW_REALM = NA
r_grid$b_cum_threats_NEW_REALM = r_grid$agriculture_b_hot_NEW_REALM+r_grid$logging_b_hot_NEW_REALM+r_grid$hunting_b_hot_NEW_REALM+r_grid$pollution_b_hot_NEW_REALM+r_grid$invasives_b_hot_NEW_REALM+
  r_grid$climate_change_b_hot_NEW_REALM+r_grid$urbanization_b_hot_NEW_REALM




library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'






pal_threats = colorRampPalette(hcl.colors(5,'Inferno',rev = T))
pal_a = colorRampPalette(hcl.colors(5,'BluYl',rev = T))
pal_r = colorRampPalette(hcl.colors(5,'YlGn',rev = T))
pal_m = colorRampPalette(hcl.colors(5,'ag_Sunset',rev = T))
pal_b = colorRampPalette(hcl.colors(5,'PinkYl',rev = T))

pal_m = colorRampPalette(hcl.colors(5,'Viridis',rev = T))


library(ggplot2)
library(sf)
sf_use_s2(F)
library(raster)
library(dplyr)

# sf_land = st_read('/Users/gdt366/Dropbox/african_snakes/land-polygons-complete-4326/land_polygons.shp')
# r = raster(extent(sf_land),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
# background_r_grid = st_as_sf(rasterToPolygons(r))
# background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
# background_r_grid$layer = 1
# background_r_grid_summ = background_r_grid %>% group_by(layer) %>% summarise()

wwf_regions = st_read('/Users/gdt366/Dropbox/Ecoregions2017/Ecoregions2017.shp')
r = raster(extent(wwf_regions),crs = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0',res = 0.5)
background_r_grid = st_as_sf(rasterToPolygons(r))
background_r_grid = st_transform(background_r_grid, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")
background_map = st_transform(wwf_regions, crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0")



# 
# sf_land = st_transform(sf_land,"+proj=moll +lon_0=0 +x_0=0 +y_0=0")
# 
# sf_land_low_res = st_simplify(sf_land,dTolerance = 0.01)

theme_harith = theme(legend.position = c(0.93,0.81),
                     legend.key.height  = unit(0.3,'cm'),
                     legend.key.width  = unit(0.3,'cm'),
                     legend.title = element_text(size = 10,hjust = 0.5,angle = 90,family = myfont),
                     legend.text = element_text(size = 10,hjust = 0.5,family = myfont))

theme_2 = theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  panel.spacing = unit(c(0,0,0,0),"mm"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank())

guides_harith = guides(fill = guide_legend(label.position = 'right',title.position = 'left',direction = 'vertical'),
                       colour = guide_legend(label.position = 'right',title.position = 'left',direction = 'vertical'))

colnames(r_grid)


p_1 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$a_cum_threats_NEW_REALM>0,],aes(col = a_cum_threats_NEW_REALM,fill = a_cum_threats_NEW_REALM),linewidth = 0)+
  scale_fill_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+
  theme_harith+
  guides_harith+
  theme_2+
  annotate(geom = 'text',label = 'Amphibians',x = 16040050, y= -7517985,size = 5,family = myfont)

p_2 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$r_cum_threats_NEW_REALM>0,],aes(col = r_cum_threats_NEW_REALM,fill = r_cum_threats_NEW_REALM),linewidth = 0)+
  scale_fill_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+
  theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Reptiles',x = 16040050, y= -7517985,size = 5,family = myfont)


p_3 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$m_cum_threats_NEW_REALM>0,],aes(col = m_cum_threats_NEW_REALM,fill = m_cum_threats_NEW_REALM),linewidth = 0)+
  scale_fill_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Mammals',x = 16040050, y= -7517985,size = 5,family = myfont)

p_4 = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$b_cum_threats_NEW_REALM>0,],aes(col = b_cum_threats_NEW_REALM,fill = b_cum_threats_NEW_REALM),linewidth = 0)+
  scale_fill_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  scale_colour_gradientn('Threats by realm',colours = pal_m(10),breaks = c(1:7))+
  theme_void()+theme_harith+
  guides_harith+theme_2+
  annotate(geom = 'text',label = 'Birds',x = 16040050, y= -7517985,size = 5,family = myfont)


grid = arrangeGrob(p_1,p_2,p_3,p_4,ncol=2)


ggsave(filename = 'cum_threats_NEW_REALM_sep_23.png',plot = grid,width = 10,height = 5,dpi = 500)

r_grid$all_cum_threats_NEW_REALM = NA

r_grid$all_cum_threats_NEW_REALM = r_grid$a_cum_threats_NEW_REALM + r_grid$r_cum_threats_NEW_REALM + r_grid$m_cum_threats_NEW_REALM + r_grid$b_cum_threats_NEW_REALM

hotspots = st_read('/Users/gdt366/Dropbox/hotspots_2016_1/hotspots_2016_1.shp')
hotspots = hotspots[-40,]
hotspot40 = st_read('/Users/gdt366/Dropbox/postdoc_KU_paper_2/hotspot_fixed.shp')
hotspots = rbind(hotspots,hotspot40)

hotspots = st_crop(hotspots,extent(c(-179,179,-90,90)))

hotspots_area = st_transform(hotspots,crs = st_crs(r_grid)[[1]])

# load('/Users/gdt366/Dropbox/disaster_project/sf_land_sum.Rdata')
# nrow(sf_land)


theme_harith_plot2 = theme(legend.position = c(0.5,-0.04),
                           legend.key.height  = unit(0.75,'cm'),
                           legend.key.width  = unit(1.5,'cm'),
                           legend.title = element_text(size = 12,hjust = 0.5,family = myfont),
                           legend.text = element_text(size = 12,hjust = 0.5,family = myfont),
                           legend.spacing.x = unit(0, 'cm'))

theme_2_plot2 = theme(
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.margin=unit(c(0,0,0,0),"mm"),
  panel.spacing = unit(c(0,0,0,0),"mm"),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.line = element_blank())

guides_harith_plot2 = guides(fill = guide_legend(label.position = 'bottom',title.position = 'top',direction = 'horizontal',nrow = 1),
                             colour = guide_legend(label.position = 'bottom',title.position = 'top',direction = 'horizontal',nrow = 1))

CMEC_newrealms = st_read('/Users/gdt366/Dropbox/postdoc_KU_paper_2/CMEC regions & realms/newRealms.shp')
CMEC_newrealms = st_crop(CMEC_newrealms,extent(c(-179,179,-90,90)))
CMEC_newrealms = st_transform(CMEC_newrealms,"+proj=moll +lon_0=0 +x_0=0 +y_0=0")

library(RColorBrewer)
pal_wallace = brewer.pal(11,'Paired')
p_all = ggplot()+
  geom_sf(data = background_r_grid,bg = 'lightblue',col = 'lightblue',linewidth = 0)+
  geom_sf(data = background_map,bg = 'grey90',linewidth = 0.05,col = 'grey90')+
  geom_sf(data = CMEC_newrealms,col = 'black',linewidth = 0.1,fill = 'grey90')+
  geom_sf(data = r_grid[r_grid$land==1 & r_grid$all_cum_threats_NEW_REALM>0,],aes(col = all_cum_threats_NEW_REALM,fill = all_cum_threats_NEW_REALM),linewidth = 0)+
  geom_sf(data = CMEC_newrealms,col = 'black',linewidth = 0.1,fill = NA)+
  scale_fill_gradientn('Threats by taxa',colours = pal_m(10),breaks = c(1:14))+
  scale_colour_gradientn('Threats by taxa',colours = pal_m(10),breaks = c(1:14))+
  theme_void()+theme_harith_plot2+theme_2_plot2+
  guides_harith_plot2+
  #geom_sf(data = background_r_grid_summ,bg = NA,col = 'black',linewidth = 0.1)+
  annotate(geom = 'text',label = 'Vertebrate hotspots',x = 15040050, y= -7917985,size = 5,family = myfont)+
  theme(legend.key.spacing.x = unit(0,'cm'))


ggsave(filename = 'cum_threats_all_NEW_REALM_23_sep.png',plot = p_all,width = 12,height = 8,dpi = 2000)
# 
# ggsave(filename = 'cum_threats_all.png',plot = p_all,width = 6,height = 5,dpi = 2000)

library(patchwork)
bottom_grid <- p_1 + p_4 + p_3 + p_2 + 
  plot_layout(nrow = 2, ncol = 2)

combined_plot <- p_all + bottom_grid + 
  plot_layout(heights = c(1.1, 0.9))

#ggsave(filename = 'Figure1_NEWREALMS.png',plot = combined_plot,width = 12,height = 14,dpi = 2000)
ggsave(filename = 'Figure1_1500_NEWREALMS_23_sep.png',plot = combined_plot,width = 12,height = 14,dpi = 1500)

p_all = ggplot()+geom_sf(data = CMEC_newrealms,col = 'black',linewidth = 0.1,fill = NA)+
  theme_void()

library(raster)
CMEC_newrealms = st_read('/Users/gdt366/Dropbox/postdoc_KU_paper_2/CMEC regions & realms/newRealms.shp')
CMEC_newrealms = st_crop(CMEC_newrealms,extent(c(-179,179,-90,90)))
CMEC_newrealms = st_transform(CMEC_newrealms,"+proj=moll +lon_0=0 +x_0=0 +y_0=0")
p = ggplot()+geom_sf(data = CMEC_newrealms,aes(fill = Realm),linewidth = 0.25,col = 'white')+
  theme_void()+
  theme(legend.position = 'none')+
  scale_fill_manual(values = pal_wallace)

ggsave('inset_wallace_23_sep.png',width = 5,height = 3,dpi = 500,plot = p)  



