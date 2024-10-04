# Load the necessary library
library(dplyr)
library(sf)
sf_use_s2(F)

get_decile_group <- function(x) {
  if(is.na(x) || x == 0) return(NA)
  sum(x > deciles) # This returns the decile group
}

load('r_grid_metrics_run2_deciles.Rdata')

load('r_grid_metrics_run2_deciles_wallace_dec12.Rdata')


#wege----
valid_x <- r_grid[!is.na(r_grid$wege2_a) & r_grid$wege2_a != 0 & r_grid$land==1,]$wege2_a
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(wege2_a_d = ifelse(land==1,sapply(wege2_a, get_decile_group),NA))

valid_x <- r_grid[!is.na(r_grid$wege2_r) & r_grid$wege2_r != 0 & r_grid$land==1,]$wege2_r
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(wege2_r_d = ifelse(land==1,sapply(wege2_r, get_decile_group),NA))

valid_x <- r_grid[!is.na(r_grid$wege2_m) & r_grid$wege2_m != 0 & r_grid$land==1,]$wege2_m
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(wege2_m_d = ifelse(land==1,sapply(wege2_m, get_decile_group),NA))

valid_x <- r_grid[!is.na(r_grid$wege2_b) & r_grid$wege2_b != 0 & r_grid$land==1,]$wege2_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(wege2_b_d = ifelse(land==1,sapply(wege2_b, get_decile_group),NA))


# threats amphibians ----
colnames(r_grid)

valid_x <- r_grid$agriculture_a[!is.na(r_grid$agriculture_a) & r_grid$agriculture_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(agriculture_a_d = ifelse(land==1,sapply(agriculture_a, get_decile_group),NA))


valid_x <- r_grid$logging_a[!is.na(r_grid$logging_a) & r_grid$logging_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(logging_a_d = ifelse(land==1,sapply(logging_a, get_decile_group),NA))


valid_x <- r_grid$hunting_a[!is.na(r_grid$hunting_a) & r_grid$hunting_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(hunting_a_d = ifelse(land==1,sapply(hunting_a, get_decile_group),NA))


valid_x <- r_grid$pollution_a[!is.na(r_grid$pollution_a) & r_grid$pollution_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(pollution_a_d = ifelse(land==1,sapply(pollution_a, get_decile_group),NA))


valid_x <- r_grid$invasives_a[!is.na(r_grid$invasives_a) & r_grid$invasives_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(invasives_a_d = ifelse(land==1,sapply(invasives_a, get_decile_group),NA))


valid_x <- r_grid$climate_change_a[!is.na(r_grid$climate_change_a) & r_grid$climate_change_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(climate_change_a_d = ifelse(land==1,sapply(climate_change_a, get_decile_group),NA))


valid_x <- r_grid$urbanization_a[!is.na(r_grid$urbanization_a) & r_grid$urbanization_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(urbanization_a_d = ifelse(land==1,sapply(urbanization_a, get_decile_group),NA))



# threats reptiles ----
colnames(r_grid)

valid_x <- r_grid$agriculture_r[!is.na(r_grid$agriculture_r) & r_grid$agriculture_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(agriculture_r_d = ifelse(land==1,sapply(agriculture_r, get_decile_group),NA))


valid_x <- r_grid$logging_r[!is.na(r_grid$logging_r) & r_grid$logging_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(logging_r_d = ifelse(land==1,sapply(logging_r, get_decile_group),NA))


valid_x <- r_grid$hunting_r[!is.na(r_grid$hunting_r) & r_grid$hunting_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(hunting_r_d = ifelse(land==1,sapply(hunting_r, get_decile_group),NA))


valid_x <- r_grid$pollution_r[!is.na(r_grid$pollution_r) & r_grid$pollution_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(pollution_r_d = ifelse(land==1,sapply(pollution_r, get_decile_group),NA))


valid_x <- r_grid$invasives_r[!is.na(r_grid$invasives_r) & r_grid$invasives_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(invasives_r_d = ifelse(land==1,sapply(invasives_r, get_decile_group),NA))


valid_x <- r_grid$climate_change_r[!is.na(r_grid$climate_change_r) & r_grid$climate_change_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(climate_change_r_d = ifelse(land==1,sapply(climate_change_r, get_decile_group),NA))


valid_x <- r_grid$urbanization_r[!is.na(r_grid$urbanization_r) & r_grid$urbanization_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(urbanization_r_d = ifelse(land==1,sapply(urbanization_r, get_decile_group),NA))






# threats mammals ----
colnames(r_grid)


valid_x <- r_grid$agriculture_m[!is.na(r_grid$agriculture_m) & r_grid$agriculture_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(agriculture_m_d = ifelse(land==1,sapply(agriculture_m, get_decile_group),NA))


valid_x <- r_grid$logging_m[!is.na(r_grid$logging_m) & r_grid$logging_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(logging_m_d = ifelse(land==1,sapply(logging_m, get_decile_group),NA))


valid_x <- r_grid$hunting_m[!is.na(r_grid$hunting_m) & r_grid$hunting_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(hunting_m_d = ifelse(land==1,sapply(hunting_m, get_decile_group),NA))


valid_x <- r_grid$pollution_m[!is.na(r_grid$pollution_m) & r_grid$pollution_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(pollution_m_d = ifelse(land==1,sapply(pollution_m, get_decile_group),NA))


valid_x <- r_grid$invasives_m[!is.na(r_grid$invasives_m) & r_grid$invasives_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(invasives_m_d = ifelse(land==1,sapply(invasives_m, get_decile_group),NA))


valid_x <- r_grid$climate_change_m[!is.na(r_grid$climate_change_m) & r_grid$climate_change_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(climate_change_m_d = ifelse(land==1,sapply(climate_change_m, get_decile_group),NA))


valid_x <- r_grid$urbanization_m[!is.na(r_grid$urbanization_m) & r_grid$urbanization_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(urbanization_m_d = ifelse(land==1,sapply(urbanization_m, get_decile_group),NA))





# threats birds ----
colnames(r_grid)


valid_x <- r_grid[!is.na(r_grid$agriculture_b) & r_grid$agriculture_b != 0 & r_grid$land > 0,]$agriculture_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(agriculture_b_d = ifelse(land==1,sapply(agriculture_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$logging_b) & r_grid$logging_b != 0 & r_grid$land > 0,]$logging_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(logging_b_d = ifelse(land==1,sapply(logging_b, get_decile_group),NA))


valid_x <- r_gridX[!is.na(r_grid$hunting_b) & r_grid$hunting_b != 0 & r_grid$land > 0,]& r_grid$land > 0
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(hunting_b_d = ifelse(land==1,sapply(hunting_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$pollution_b) & r_grid$pollution_b != 0 & r_grid$land > 0,]$pollution_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(pollution_b_d = ifelse(land==1,sapply(pollution_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$invasives_b) & r_grid$invasives_b != 0 & r_grid$land > 0,]$invasives_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(invasives_b_d = ifelse(land==1,sapply(invasives_b, get_decile_group),NA))


valid_x <- r_grid[!is.na(r_grid$climate_change_b) & r_grid$climate_change_b != 0 & r_grid$land > 0,]$climate_change_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(climate_change_b_d = ifelse(land==1,sapply(climate_change_b, get_decile_group),NA))


valid_x <- r_grid$urbanization_b[!is.na(r_grid$urbanization_b) & r_grid$urbanization_b != 0 & r_grid$land > 0,]$urbanization_b
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(urbanization_b_d = ifelse(land==1,sapply(urbanization_b, get_decile_group),NA))





save(r_grid,file = 'r_grid_metrics_run2_deciles_wallace_Jan19_fixed_agriculture.Rdata')





#we----
valid_x <- r_grid$we2_a[!is.na(r_grid$we2_a) & r_grid$we2_a != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(we2_a_d = ifelse(land==1,sapply(we2_a, get_decile_group),NA))


valid_x <- r_grid$we2_r[!is.na(r_grid$we2_r) & r_grid$we2_r != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(we2_r_d = ifelse(land==1,sapply(we2_r, get_decile_group),NA))


valid_x <- r_grid$we2_m[!is.na(r_grid$we2_m) & r_grid$we2_m != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(we2_m_d = ifelse(land==1,sapply(we2_m, get_decile_group),NA))


valid_x <- r_grid$we2_b[!is.na(r_grid$we2_b) & r_grid$we2_b != 0]
deciles <- quantile(valid_x, probs = seq(0, 1, by = 0.1))
r_grid <- r_grid %>% mutate(we2_b_d = ifelse(land==1,sapply(we2_b, get_decile_group),NA))



save(r_grid,file = 'r_grid_metrics_run2_deciles.Rdata')

nrow(r_grid[r_grid$wege2_a_d %in% 10,])/nrow(r_grid[r_grid$land %in% 1,])
nrow(r_grid[r_grid$wege2_r_d %in% 10,])/nrow(r_grid[r_grid$land %in% 1,])
nrow(r_grid[r_grid$wege2_m_d %in% 10,])/nrow(r_grid[r_grid$land %in% 1,])
nrow(r_grid[r_grid$wege2_b_d %in% 10,])/nrow(r_grid[r_grid$land %in% 1,])
table(r_grid$wege2_r_d)




