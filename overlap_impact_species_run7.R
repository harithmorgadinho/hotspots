#
library(tidyverse)
library(sf)
sf_use_s2(F)

library(extrafont)

font_import()
y
loadfonts()
myfont = 'Avenir Next Condensed'

load('r_grid_vulcanos_earthquakes_tsunamis_hurricanes_run6.Rdata')

load('amphibians_filtered.Rdata')
nrow(amphibians)

load('reptiles_filtered.Rdata')
nrow(reptiles)

load('mammals_filtered.Rdata')
nrow(mammals)

load('birds_breeding_resident_filtered_fixed.Rdata')

birds_breeding = birds_breeding_resident
nrow(birds_breeding)


###

r_grid_final = r_grid

par(mfrow = c(2,2))
hist(r_grid_final$vulcanos_prob_max)
hist(r_grid_final$hurricanes_prob_max)
hist(r_grid_final$earthquakes_prob_max)
hist(r_grid_final$tsunamis_prob_max)

hist(r_grid_final$vulcanos_prob_sum)
hist(r_grid_final$hurricanes_prob_sum)
hist(r_grid_final$earthquakes_prob_sum)
hist(r_grid_final$tsunamis_prob_sum)

hist(r_grid_final$vulcanos_prob_max_2)
hist(r_grid_final$hurricanes_prob_max_2)
hist(r_grid_final$earthquakes_prob_max_2)
hist(r_grid_final$tsunamis_prob_max_2)

hist(r_grid_final$vulcanos_prob_sum_2)
hist(r_grid_final$hurricanes_prob_sum_2)
hist(r_grid_final$earthquakes_prob_sum_2)
hist(r_grid_final$tsunamis_prob_sum_2)

hist(r_grid_final$vulcanos_prob_max_3)
hist(r_grid_final$hurricanes_prob_max_3)
hist(r_grid_final$earthquakes_prob_max_3)
hist(r_grid_final$tsunamis_prob_max_3)

hist(r_grid_final$vulcanos_prob_sum_3)
hist(r_grid_final$hurricanes_prob_sum_3)
hist(r_grid_final$earthquakes_prob_sum_3)
hist(r_grid_final$tsunamis_prob_sum_3)

hist(r_grid_final$vulcanos_prob_sum_final)
hist(r_grid_final$hurricanes_prob_sum_final)
hist(r_grid_final$earthquakes_prob_sum_final)
hist(r_grid_final$tsunamis_prob_sum_final)



r_grid_final[is.na(r_grid_final$vulcanos_prob_max),]$vulcanos_prob_max = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_max),]$hurricanes_prob_max = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_max),]$earthquakes_prob_max = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_max),]$tsunamis_prob_max = 0

r_grid_final[is.na(r_grid_final$vulcanos_prob_sum),]$vulcanos_prob_sum = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_sum),]$hurricanes_prob_sum = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_sum),]$earthquakes_prob_sum = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_sum),]$tsunamis_prob_sum = 0

r_grid_final[is.na(r_grid_final$vulcanos_prob_max_2),]$vulcanos_prob_max_2 = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_max_2),]$hurricanes_prob_max_2 = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_max_2),]$earthquakes_prob_max_2 = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_max_2),]$tsunamis_prob_max_2 = 0

r_grid_final[is.na(r_grid_final$vulcanos_prob_sum_2),]$vulcanos_prob_sum_2 = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_sum_2),]$hurricanes_prob_sum_2 = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_sum_2),]$earthquakes_prob_sum_2 = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_sum_2),]$tsunamis_prob_sum_2 = 0

r_grid_final[is.na(r_grid_final$vulcanos_prob_max_3),]$vulcanos_prob_max_3 = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_max_3),]$hurricanes_prob_max_3 = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_max_3),]$earthquakes_prob_max_3 = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_max_3),]$tsunamis_prob_max_3 = 0

r_grid_final[is.na(r_grid_final$vulcanos_prob_sum_3),]$vulcanos_prob_sum_3 = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_sum_3),]$hurricanes_prob_sum_3 = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_sum_3),]$earthquakes_prob_sum_3 = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_sum_3),]$tsunamis_prob_sum_3 = 0


r_grid_final[is.na(r_grid_final$vulcanos_prob_sum_final),]$vulcanos_prob_sum_final = 0
r_grid_final[is.na(r_grid_final$hurricanes_prob_sum_final),]$hurricanes_prob_sum_final = 0
r_grid_final[is.na(r_grid_final$earthquakes_prob_sum_final),]$earthquakes_prob_sum_final = 0
r_grid_final[is.na(r_grid_final$tsunamis_prob_sum_final),]$tsunamis_prob_sum_final = 0




amphibians$range_size = st_area(amphibians)/1000000
units(amphibians$range_size) = NULL
nrow(amphibians)
reptiles$range_size = st_area(reptiles)/1000000
units(reptiles$range_size) = NULL

mammals$range_size = st_area(mammals)/1000000
units(mammals$range_size) = NULL

birds_breeding$range_size = st_area(birds_breeding)/1000000
units(birds_breeding$range_size) = NULL

st_crs(r_grid_final) = st_crs(amphibians)

amphibians_grid = st_intersection(amphibians,r_grid_final)
reptiles = st_buffer(reptiles,dist = 0)
reptiles_grid = st_intersection(reptiles,r_grid_final)
mammals_grid = st_intersection(mammals,r_grid_final)
#birds_grid = st_intersection(birds,r_grid_final)
birds_breeding = st_buffer(birds_breeding,dist = 0)
birds_breeding_grid = st_intersection(birds_breeding,r_grid_final)


amphibians_grid$range_size_intersect = st_area(amphibians_grid)/1000000
units(amphibians_grid$range_size_intersect) = NULL
reptiles_grid$range_size_intersect = st_area(reptiles_grid)/1000000
units(reptiles_grid$range_size_intersect) = NULL

mammals_grid$range_size_intersect = st_area(mammals_grid)/1000000
units(mammals_grid$range_size_intersect) = NULL

birds_breeding_grid$range_size_intersect = st_area(birds_breeding_grid)/1000000
units(birds_breeding_grid$range_size_intersect) = NULL


amphibians_grid$percent_range = 100*amphibians_grid$range_size_intersect/amphibians_grid$range_size
reptiles_grid$percent_range = 100*reptiles_grid$range_size_intersect/reptiles_grid$range_size
mammals_grid$percent_range = 100*mammals_grid$range_size_intersect/mammals_grid$range_size
birds_breeding_grid$percent_range = 100*birds_breeding_grid$range_size_intersect/birds_breeding_grid$range_size


# amphibians_grid$square_area = st_area(amphibians_grid)/1000000
# units(amphibians_grid$square_area) = NULL
# hist(amphibians_grid$square_area)
# 
# reptiles_grid$square_area = st_area(reptiles_grid)/1000000
# units(reptiles_grid$square_area) = NULL
# hist(reptiles_grid$square_area)
# 
# mammals_grid$square_area = st_area(mammals_grid)/1000000
# units(mammals_grid$square_area) = NULL
# hist(mammals_grid$square_area)
# 
birds_breeding_grid$square_area = st_area(birds_breeding_grid)/1000000
units(birds_breeding_grid$square_area) = NULL
hist(birds_breeding_grid$square_area)


amphibians_grid2 = amphibians_grid
reptiles_grid2 = reptiles_grid
mammals_grid2 = mammals_grid
birds_breeding_grid2 = birds_breeding_grid
st_geometry(amphibians_grid2) = NULL
st_geometry(reptiles_grid2) = NULL
st_geometry(mammals_grid2) = NULL
st_geometry(birds_breeding_grid2) = NULL




# amphibians_grid2$weighted_range_size_intersect = amphibians_grid2$range_size_intersect/amphibians_grid2$square_area
# reptiles_grid2$weighted_range_size_intersect = reptiles_grid2$range_size_intersect/reptiles_grid2$square_area
# mammals_grid2$weighted_range_size_intersect = mammals_grid2$range_size_intersect/mammals_grid2$square_area
birds_breeding_grid2$weighted_range_size_intersect = birds_breeding_grid2$range_size_intersect/birds_breeding_grid2$square_area

hist(amphibians_grid2$percent_range)



# save(amphibians_grid2,file='amphibians_grid2_run6.Rdata')
# save(reptiles_grid2,file='reptiles_grid2_run6.Rdata')
# save(mammals_grid2,file='mammals_grid2_run6.Rdata')
save(birds_breeding_grid2,file='birds_breeding_grid2_run6_fixed.Rdata')


## beginning ----
load('amphibians_grid2_run6.Rdata')
load('reptiles_grid2_run6.Rdata')
load('mammals_grid2_run6.Rdata')
load('birds_breeding_grid2_run6_fixed.Rdata')




amphibians_grid_dfs = amphibians_grid2 %>% group_by(binomial) %>% summarise(range_n_cells = n())
reptiles_grid_dfs = reptiles_grid2 %>% group_by(binomial) %>% summarise(range_n_cells = n())
mammals_grid_dfs = mammals_grid2 %>% group_by(binomial) %>% summarise(range_n_cells = n())
birds_grid_dfs = birds_breeding_grid2 %>% group_by(binomial) %>% summarise(range_n_cells = n())



amphibians_grid2 = merge(amphibians_grid2,amphibians_grid_dfs,by = 'binomial')
reptiles_grid2 = merge(reptiles_grid2,reptiles_grid_dfs,by = 'binomial')
mammals_grid2 = merge(mammals_grid2,mammals_grid_dfs,by = 'binomial')
birds_breeding_grid2 = merge(birds_breeding_grid2,birds_grid_dfs,by = 'binomial')

amphibians_grid_dfs_vul = amphibians_grid2 %>% group_by(binomial) %>% filter(vulcanos_freq>0) %>% summarise(n_cells_vulcanos = n())
reptiles_grid_dfs_vul = reptiles_grid2 %>% group_by(binomial)%>% filter(vulcanos_freq>0) %>% summarise(n_cells_vulcanos = n())
mammals_grid_dfs_vul = mammals_grid2 %>% group_by(binomial)%>% filter(vulcanos_freq>0) %>% summarise(n_cells_vulcanos = n())
birds_grid_dfs_vul = birds_breeding_grid2 %>% group_by(binomial)%>% filter(vulcanos_freq>0) %>% summarise(n_cells_vulcanos = n())

amphibians_grid_dfs_tsu = amphibians_grid2 %>% group_by(binomial) %>% filter(tsunamis_freq>0) %>% summarise(n_cells_tsunamis = n())
reptiles_grid_dfs_tsu = reptiles_grid2 %>% group_by(binomial)%>% filter(tsunamis_freq>0) %>% summarise(n_cells_tsunamis = n())
mammals_grid_dfs_tsu = mammals_grid2 %>% group_by(binomial)%>% filter(tsunamis_freq>0) %>% summarise(n_cells_tsunamis = n())
birds_grid_dfs_tsu = birds_breeding_grid2 %>% group_by(binomial)%>% filter(tsunamis_freq>0) %>% summarise(n_cells_tsunamis = n())

amphibians_grid_dfs_ear = amphibians_grid2 %>% group_by(binomial) %>% filter(earthquakes_freq>0) %>% summarise(n_cells_earthquakes = n())
reptiles_grid_dfs_ear = reptiles_grid2 %>% group_by(binomial)%>% filter(earthquakes_freq>0) %>% summarise(n_cells_earthquakes = n())
mammals_grid_dfs_ear = mammals_grid2 %>% group_by(binomial)%>% filter(earthquakes_freq>0) %>% summarise(n_cells_earthquakes = n())
birds_grid_dfs_ear = birds_breeding_grid2 %>% group_by(binomial)%>% filter(earthquakes_freq>0) %>% summarise(n_cells_earthquakes = n())

amphibians_grid_dfs_hur = amphibians_grid2 %>% group_by(binomial) %>% filter(hurricanes_freq>0) %>% summarise(n_cells_hurricanes = n())
reptiles_grid_dfs_hur = reptiles_grid2 %>% group_by(binomial)%>% filter(hurricanes_freq>0) %>% summarise(n_cells_hurricanes = n())
mammals_grid_dfs_hur = mammals_grid2 %>% group_by(binomial)%>% filter(hurricanes_freq>0) %>% summarise(n_cells_hurricanes = n())
birds_grid_dfs_hur = birds_breeding_grid2 %>% group_by(binomial)%>% filter(hurricanes_freq>0) %>% summarise(n_cells_hurricanes = n())


amphibians_grid2 = merge(amphibians_grid2,amphibians_grid_dfs_vul,by = 'binomial',all.x = T)
reptiles_grid2 = merge(reptiles_grid2,reptiles_grid_dfs_vul,by = 'binomial',all.x = T)
mammals_grid2 = merge(mammals_grid2,mammals_grid_dfs_vul,by = 'binomial',all.x = T)
birds_breeding_grid2 = merge(birds_breeding_grid2,birds_grid_dfs_vul,by = 'binomial',all.x = T)

amphibians_grid2[is.na(amphibians_grid2$n_cells_vulcanos),]$n_cells_vulcanos = 0
reptiles_grid2[is.na(reptiles_grid2$n_cells_vulcanos),]$n_cells_vulcanos = 0
mammals_grid2[is.na(mammals_grid2$n_cells_vulcanos),]$n_cells_vulcanos = 0
birds_breeding_grid2[is.na(birds_breeding_grid2$n_cells_vulcanos),]$n_cells_vulcanos = 0

amphibians_grid2 = merge(amphibians_grid2,amphibians_grid_dfs_tsu,by = 'binomial',all.x = T)
reptiles_grid2 = merge(reptiles_grid2,reptiles_grid_dfs_tsu,by = 'binomial',all.x = T)
mammals_grid2 = merge(mammals_grid2,mammals_grid_dfs_tsu,by = 'binomial',all.x = T)
birds_breeding_grid2 = merge(birds_breeding_grid2,birds_grid_dfs_tsu,by = 'binomial',all.x = T)

amphibians_grid2[is.na(amphibians_grid2$n_cells_tsunamis),]$n_cells_tsunamis = 0
reptiles_grid2[is.na(reptiles_grid2$n_cells_tsunamis),]$n_cells_tsunamis = 0
mammals_grid2[is.na(mammals_grid2$n_cells_tsunamis),]$n_cells_tsunamis = 0
birds_breeding_grid2[is.na(birds_breeding_grid2$n_cells_tsunamis),]$n_cells_tsunamis = 0


amphibians_grid2 = merge(amphibians_grid2,amphibians_grid_dfs_ear,by = 'binomial',all.x = T)
reptiles_grid2 = merge(reptiles_grid2,reptiles_grid_dfs_ear,by = 'binomial',all.x = T)
mammals_grid2 = merge(mammals_grid2,mammals_grid_dfs_ear,by = 'binomial',all.x = T)
birds_breeding_grid2 = merge(birds_breeding_grid2,birds_grid_dfs_ear,by = 'binomial',all.x = T)

amphibians_grid2[is.na(amphibians_grid2$n_cells_earthquakes),]$n_cells_earthquakes = 0
reptiles_grid2[is.na(reptiles_grid2$n_cells_earthquakes),]$n_cells_earthquakes = 0
mammals_grid2[is.na(mammals_grid2$n_cells_earthquakes),]$n_cells_earthquakes = 0
birds_breeding_grid2[is.na(birds_breeding_grid2$n_cells_earthquakes),]$n_cells_earthquakes = 0


amphibians_grid2 = merge(amphibians_grid2,amphibians_grid_dfs_hur,by = 'binomial',all.x = T)
reptiles_grid2 = merge(reptiles_grid2,reptiles_grid_dfs_hur,by = 'binomial',all.x = T)
mammals_grid2 = merge(mammals_grid2,mammals_grid_dfs_hur,by = 'binomial',all.x = T)
birds_breeding_grid2 = merge(birds_breeding_grid2,birds_grid_dfs_hur,by = 'binomial',all.x = T)

amphibians_grid2[is.na(amphibians_grid2$n_cells_hurricanes),]$n_cells_hurricanes = 0
reptiles_grid2[is.na(reptiles_grid2$n_cells_hurricanes),]$n_cells_hurricanes = 0
mammals_grid2[is.na(mammals_grid2$n_cells_hurricanes),]$n_cells_hurricanes = 0
birds_breeding_grid2[is.na(birds_breeding_grid2$n_cells_hurricanes),]$n_cells_hurricanes = 0


library(ggplot2)




###vulcanos----
amphibians_grid2$weighted_vulcanos_prob_max = (amphibians_grid2$vulcanos_prob_max)
reptiles_grid2$weighted_vulcanos_prob_max = (reptiles_grid2$vulcanos_prob_max)
mammals_grid2$weighted_vulcanos_prob_max = (mammals_grid2$vulcanos_prob_max)
birds_breeding_grid2$weighted_vulcanos_prob_max = (birds_breeding_grid2$vulcanos_prob_max)

amphibians_grid2$weighted_vulcanos_prob_sum = (amphibians_grid2$vulcanos_prob_sum)
reptiles_grid2$weighted_vulcanos_prob_sum = (reptiles_grid2$vulcanos_prob_sum)
mammals_grid2$weighted_vulcanos_prob_sum = (mammals_grid2$vulcanos_prob_sum)
birds_breeding_grid2$weighted_vulcanos_prob_sum = (birds_breeding_grid2$vulcanos_prob_sum)

amphibians_grid2$weighted_vulcanos_prob_max_2 = (amphibians_grid2$vulcanos_prob_max_2)
reptiles_grid2$weighted_vulcanos_prob_max_2 = (reptiles_grid2$vulcanos_prob_max_2)
mammals_grid2$weighted_vulcanos_prob_max_2 = (mammals_grid2$vulcanos_prob_max_2)
birds_breeding_grid2$weighted_vulcanos_prob_max_2 = (birds_breeding_grid2$vulcanos_prob_max_2)

amphibians_grid2$weighted_vulcanos_prob_sum_2 = (amphibians_grid2$vulcanos_prob_sum_2)
reptiles_grid2$weighted_vulcanos_prob_sum_2 = (reptiles_grid2$vulcanos_prob_sum_2)
mammals_grid2$weighted_vulcanos_prob_sum_2 = (mammals_grid2$vulcanos_prob_sum_2)
birds_breeding_grid2$weighted_vulcanos_prob_sum_2 = (birds_breeding_grid2$vulcanos_prob_sum_2)

amphibians_grid2$weighted_vulcanos_prob_max_3 = (amphibians_grid2$vulcanos_prob_max_3)
reptiles_grid2$weighted_vulcanos_prob_max_3 = (reptiles_grid2$vulcanos_prob_max_3)
mammals_grid2$weighted_vulcanos_prob_max_3 = (mammals_grid2$vulcanos_prob_max_3)
birds_breeding_grid2$weighted_vulcanos_prob_max_3 = (birds_breeding_grid2$vulcanos_prob_max_3)

amphibians_grid2$weighted_vulcanos_prob_sum_3 = (amphibians_grid2$vulcanos_prob_sum_3)
reptiles_grid2$weighted_vulcanos_prob_sum_3 = (reptiles_grid2$vulcanos_prob_sum_3)
mammals_grid2$weighted_vulcanos_prob_sum_3 = (mammals_grid2$vulcanos_prob_sum_3)
birds_breeding_grid2$weighted_vulcanos_prob_sum_3 = (birds_breeding_grid2$vulcanos_prob_sum_3)

amphibians_grid2$weighted_vulcanos_prob_sum_final = (amphibians_grid2$vulcanos_prob_sum_final)
reptiles_grid2$weighted_vulcanos_prob_sum_final = (reptiles_grid2$vulcanos_prob_sum_final)
mammals_grid2$weighted_vulcanos_prob_sum_final = (mammals_grid2$vulcanos_prob_sum_final)
birds_breeding_grid2$weighted_vulcanos_prob_sum_final = (birds_breeding_grid2$vulcanos_prob_sum_final)



# run with all cells

amphibians_grid2$weighted_vulcanos_prob_max_pixel = amphibians_grid2$vulcanos_prob_max
reptiles_grid2$weighted_vulcanos_prob_max_pixel = reptiles_grid2$vulcanos_prob_max
mammals_grid2$weighted_vulcanos_prob_max_pixel = mammals_grid2$vulcanos_prob_max
birds_breeding_grid2$weighted_vulcanos_prob_max_pixel = birds_breeding_grid2$vulcanos_prob_max

amphibians_grid2$weighted_vulcanos_prob_sum_pixel = amphibians_grid2$vulcanos_prob_sum
reptiles_grid2$weighted_vulcanos_prob_sum_pixel = reptiles_grid2$vulcanos_prob_sum
mammals_grid2$weighted_vulcanos_prob_sum_pixel = mammals_grid2$vulcanos_prob_sum
birds_breeding_grid2$weighted_vulcanos_prob_sum_pixel = birds_breeding_grid2$vulcanos_prob_sum

amphibians_grid2$weighted_vulcanos_prob_max_2_pixel = amphibians_grid2$vulcanos_prob_max_2
reptiles_grid2$weighted_vulcanos_prob_max_2_pixel = reptiles_grid2$vulcanos_prob_max_2
mammals_grid2$weighted_vulcanos_prob_max_2_pixel = mammals_grid2$vulcanos_prob_max_2
birds_breeding_grid2$weighted_vulcanos_prob_max_2_pixel = birds_breeding_grid2$vulcanos_prob_max_2

amphibians_grid2$weighted_vulcanos_prob_sum_2_pixel = amphibians_grid2$vulcanos_prob_sum_2
reptiles_grid2$weighted_vulcanos_prob_sum_2_pixel = reptiles_grid2$vulcanos_prob_sum_2
mammals_grid2$weighted_vulcanos_prob_sum_2_pixel = mammals_grid2$vulcanos_prob_sum_2
birds_breeding_grid2$weighted_vulcanos_prob_sum_2_pixel = birds_breeding_grid2$vulcanos_prob_sum_2

amphibians_grid2$weighted_vulcanos_prob_max_3_pixel = amphibians_grid2$vulcanos_prob_max_3
reptiles_grid2$weighted_vulcanos_prob_max_3_pixel = reptiles_grid2$vulcanos_prob_max_3
mammals_grid2$weighted_vulcanos_prob_max_3_pixel = mammals_grid2$vulcanos_prob_max_3
birds_breeding_grid2$weighted_vulcanos_prob_max_3_pixel = birds_breeding_grid2$vulcanos_prob_max_3

amphibians_grid2$weighted_vulcanos_prob_sum_3_pixel = amphibians_grid2$vulcanos_prob_sum_3
reptiles_grid2$weighted_vulcanos_prob_sum_3_pixel = reptiles_grid2$vulcanos_prob_sum_3
mammals_grid2$weighted_vulcanos_prob_sum_3_pixel = mammals_grid2$vulcanos_prob_sum_3
birds_breeding_grid2$weighted_vulcanos_prob_sum_3_pixel = birds_breeding_grid2$vulcanos_prob_sum_3

amphibians_grid2$weighted_vulcanos_prob_sum_final_pixel = amphibians_grid2$vulcanos_prob_sum_final
reptiles_grid2$weighted_vulcanos_prob_sum_final_pixel = reptiles_grid2$vulcanos_prob_sum_final
mammals_grid2$weighted_vulcanos_prob_sum_final_pixel = mammals_grid2$vulcanos_prob_sum_final
birds_breeding_grid2$weighted_vulcanos_prob_sum_final_pixel = birds_breeding_grid2$vulcanos_prob_sum_final



amphibians_grid2_vul_0 = amphibians_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & vulcanos_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_vulcanos)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_vulcanos_prob_max),avg_impact_sum =  median(weighted_vulcanos_prob_sum),
            avg_impact_max_2 =  median(weighted_vulcanos_prob_max_2),avg_impact_sum_2 =  median(weighted_vulcanos_prob_sum_2),
            avg_impact_max_3 =  median(weighted_vulcanos_prob_max_3),avg_impact_sum_3 =  median(weighted_vulcanos_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_vulcanos_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_vulcanos_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_vulcanos_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_vulcanos_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_vulcanos_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_vulcanos_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_vulcanos_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_vulcanos_prob_sum_final),
            vulcanos_freq = sum(vulcanos_freq))

reptiles_grid2_vul_0 = reptiles_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & vulcanos_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_vulcanos)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_vulcanos_prob_max),avg_impact_sum =  median(weighted_vulcanos_prob_sum),
            avg_impact_max_2 =  median(weighted_vulcanos_prob_max_2),avg_impact_sum_2 =  median(weighted_vulcanos_prob_sum_2),
            avg_impact_max_3 =  median(weighted_vulcanos_prob_max_3),avg_impact_sum_3 =  median(weighted_vulcanos_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_vulcanos_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_vulcanos_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_vulcanos_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_vulcanos_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_vulcanos_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_vulcanos_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_vulcanos_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_vulcanos_prob_sum_final),
            
            vulcanos_freq = sum(vulcanos_freq))

mammals_grid2_vul_0 = mammals_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & vulcanos_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_vulcanos)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_vulcanos_prob_max),avg_impact_sum =  median(weighted_vulcanos_prob_sum),
            avg_impact_max_2 =  median(weighted_vulcanos_prob_max_2),avg_impact_sum_2 =  median(weighted_vulcanos_prob_sum_2),
            avg_impact_max_3 =  median(weighted_vulcanos_prob_max_3),avg_impact_sum_3 =  median(weighted_vulcanos_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_vulcanos_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_vulcanos_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_vulcanos_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_vulcanos_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_vulcanos_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_vulcanos_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_vulcanos_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_vulcanos_prob_sum_final),
            
            vulcanos_freq = sum(vulcanos_freq))

birds_breeding_grid2_vul_0 = birds_breeding_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & vulcanos_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_vulcanos)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_vulcanos_prob_max),avg_impact_sum =  median(weighted_vulcanos_prob_sum),
            avg_impact_max_2 =  median(weighted_vulcanos_prob_max_2),avg_impact_sum_2 =  median(weighted_vulcanos_prob_sum_2),
            avg_impact_max_3 =  median(weighted_vulcanos_prob_max_3),avg_impact_sum_3 =  median(weighted_vulcanos_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_vulcanos_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_vulcanos_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_vulcanos_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_vulcanos_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_vulcanos_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_vulcanos_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_vulcanos_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_vulcanos_prob_sum_final),
            
            vulcanos_freq = sum(vulcanos_freq))




##tsunamis----

amphibians_grid2$weighted_tsunamis_prob_max = (amphibians_grid2$tsunamis_prob_max)
reptiles_grid2$weighted_tsunamis_prob_max = (reptiles_grid2$tsunamis_prob_max)
mammals_grid2$weighted_tsunamis_prob_max = (mammals_grid2$tsunamis_prob_max)
birds_breeding_grid2$weighted_tsunamis_prob_max = (birds_breeding_grid2$tsunamis_prob_max)

amphibians_grid2$weighted_tsunamis_prob_sum = (amphibians_grid2$tsunamis_prob_sum)
reptiles_grid2$weighted_tsunamis_prob_sum = (reptiles_grid2$tsunamis_prob_sum)
mammals_grid2$weighted_tsunamis_prob_sum = (mammals_grid2$tsunamis_prob_sum)
birds_breeding_grid2$weighted_tsunamis_prob_sum = (birds_breeding_grid2$tsunamis_prob_sum)

amphibians_grid2$weighted_tsunamis_prob_max_2 = (amphibians_grid2$tsunamis_prob_max_2)
reptiles_grid2$weighted_tsunamis_prob_max_2 = (reptiles_grid2$tsunamis_prob_max_2)
mammals_grid2$weighted_tsunamis_prob_max_2 = (mammals_grid2$tsunamis_prob_max_2)
birds_breeding_grid2$weighted_tsunamis_prob_max_2 = (birds_breeding_grid2$tsunamis_prob_max_2)

amphibians_grid2$weighted_tsunamis_prob_sum_2 = (amphibians_grid2$tsunamis_prob_sum_2)
reptiles_grid2$weighted_tsunamis_prob_sum_2 = (reptiles_grid2$tsunamis_prob_sum_2)
mammals_grid2$weighted_tsunamis_prob_sum_2 = (mammals_grid2$tsunamis_prob_sum_2)
birds_breeding_grid2$weighted_tsunamis_prob_sum_2 = (birds_breeding_grid2$tsunamis_prob_sum_2)

amphibians_grid2$weighted_tsunamis_prob_max_3 = (amphibians_grid2$tsunamis_prob_max_3)
reptiles_grid2$weighted_tsunamis_prob_max_3 = (reptiles_grid2$tsunamis_prob_max_3)
mammals_grid2$weighted_tsunamis_prob_max_3 = (mammals_grid2$tsunamis_prob_max_3)
birds_breeding_grid2$weighted_tsunamis_prob_max_3 = (birds_breeding_grid2$tsunamis_prob_max_3)

amphibians_grid2$weighted_tsunamis_prob_sum_3 = (amphibians_grid2$tsunamis_prob_sum_3)
reptiles_grid2$weighted_tsunamis_prob_sum_3 = (reptiles_grid2$tsunamis_prob_sum_3)
mammals_grid2$weighted_tsunamis_prob_sum_3 = (mammals_grid2$tsunamis_prob_sum_3)
birds_breeding_grid2$weighted_tsunamis_prob_sum_3 = (birds_breeding_grid2$tsunamis_prob_sum_3)

amphibians_grid2$weighted_tsunamis_prob_sum_final = (amphibians_grid2$tsunamis_prob_sum_final)
reptiles_grid2$weighted_tsunamis_prob_sum_final = (reptiles_grid2$tsunamis_prob_sum_final)
mammals_grid2$weighted_tsunamis_prob_sum_final = (mammals_grid2$tsunamis_prob_sum_final)
birds_breeding_grid2$weighted_tsunamis_prob_sum_final = (birds_breeding_grid2$tsunamis_prob_sum_final)

# run with all cells

amphibians_grid2$weighted_tsunamis_prob_max_pixel = amphibians_grid2$tsunamis_prob_max
reptiles_grid2$weighted_tsunamis_prob_max_pixel = reptiles_grid2$tsunamis_prob_max
mammals_grid2$weighted_tsunamis_prob_max_pixel = mammals_grid2$tsunamis_prob_max
birds_breeding_grid2$weighted_tsunamis_prob_max_pixel = birds_breeding_grid2$tsunamis_prob_max

amphibians_grid2$weighted_tsunamis_prob_sum_pixel = amphibians_grid2$tsunamis_prob_sum
reptiles_grid2$weighted_tsunamis_prob_sum_pixel = reptiles_grid2$tsunamis_prob_sum
mammals_grid2$weighted_tsunamis_prob_sum_pixel = mammals_grid2$tsunamis_prob_sum
birds_breeding_grid2$weighted_tsunamis_prob_sum_pixel = birds_breeding_grid2$tsunamis_prob_sum

amphibians_grid2$weighted_tsunamis_prob_max_2_pixel = amphibians_grid2$tsunamis_prob_max_2
reptiles_grid2$weighted_tsunamis_prob_max_2_pixel = reptiles_grid2$tsunamis_prob_max_2
mammals_grid2$weighted_tsunamis_prob_max_2_pixel = mammals_grid2$tsunamis_prob_max_2
birds_breeding_grid2$weighted_tsunamis_prob_max_2_pixel = birds_breeding_grid2$tsunamis_prob_max_2

amphibians_grid2$weighted_tsunamis_prob_sum_2_pixel = amphibians_grid2$tsunamis_prob_sum_2
reptiles_grid2$weighted_tsunamis_prob_sum_2_pixel = reptiles_grid2$tsunamis_prob_sum_2
mammals_grid2$weighted_tsunamis_prob_sum_2_pixel = mammals_grid2$tsunamis_prob_sum_2
birds_breeding_grid2$weighted_tsunamis_prob_sum_2_pixel = birds_breeding_grid2$tsunamis_prob_sum_2

amphibians_grid2$weighted_tsunamis_prob_max_3_pixel = amphibians_grid2$tsunamis_prob_max_3
reptiles_grid2$weighted_tsunamis_prob_max_3_pixel = reptiles_grid2$tsunamis_prob_max_3
mammals_grid2$weighted_tsunamis_prob_max_3_pixel = mammals_grid2$tsunamis_prob_max_3
birds_breeding_grid2$weighted_tsunamis_prob_max_3_pixel = birds_breeding_grid2$tsunamis_prob_max_3

amphibians_grid2$weighted_tsunamis_prob_sum_3_pixel = amphibians_grid2$tsunamis_prob_sum_3
reptiles_grid2$weighted_tsunamis_prob_sum_3_pixel = reptiles_grid2$tsunamis_prob_sum_3
mammals_grid2$weighted_tsunamis_prob_sum_3_pixel = mammals_grid2$tsunamis_prob_sum_3
birds_breeding_grid2$weighted_tsunamis_prob_sum_3_pixel = birds_breeding_grid2$tsunamis_prob_sum_3

amphibians_grid2$weighted_tsunamis_prob_sum_final_pixel = amphibians_grid2$tsunamis_prob_sum_final
reptiles_grid2$weighted_tsunamis_prob_sum_final_pixel = reptiles_grid2$tsunamis_prob_sum_final
mammals_grid2$weighted_tsunamis_prob_sum_final_pixel = mammals_grid2$tsunamis_prob_sum_final
birds_breeding_grid2$weighted_tsunamis_prob_sum_final_pixel = birds_breeding_grid2$tsunamis_prob_sum_final




amphibians_grid2_tsu_0 = amphibians_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & tsunamis_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_tsunamis)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_tsunamis_prob_max),avg_impact_sum =  median(weighted_tsunamis_prob_sum),
            avg_impact_max_2 =  median(weighted_tsunamis_prob_max_2),avg_impact_sum_2 =  median(weighted_tsunamis_prob_sum_2),
            avg_impact_max_3 =  median(weighted_tsunamis_prob_max_3),avg_impact_sum_3 =  median(weighted_tsunamis_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_tsunamis_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_tsunamis_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_tsunamis_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_tsunamis_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_tsunamis_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_tsunamis_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_tsunamis_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_tsunamis_prob_sum_final),
            
            tsunamis_freq = sum(tsunamis_freq))

reptiles_grid2_tsu_0 = reptiles_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & tsunamis_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_tsunamis)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_tsunamis_prob_max),avg_impact_sum =  median(weighted_tsunamis_prob_sum),
            avg_impact_max_2 =  median(weighted_tsunamis_prob_max_2),avg_impact_sum_2 =  median(weighted_tsunamis_prob_sum_2),
            avg_impact_max_3 =  median(weighted_tsunamis_prob_max_3),avg_impact_sum_3 =  median(weighted_tsunamis_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_tsunamis_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_tsunamis_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_tsunamis_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_tsunamis_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_tsunamis_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_tsunamis_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_tsunamis_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_tsunamis_prob_sum_final),
            
            tsunamis_freq = sum(tsunamis_freq))

mammals_grid2_tsu_0 = mammals_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & tsunamis_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_tsunamis)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_tsunamis_prob_max),avg_impact_sum =  median(weighted_tsunamis_prob_sum),
            avg_impact_max_2 =  median(weighted_tsunamis_prob_max_2),avg_impact_sum_2 =  median(weighted_tsunamis_prob_sum_2),
            avg_impact_max_3 =  median(weighted_tsunamis_prob_max_3),avg_impact_sum_3 =  median(weighted_tsunamis_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_tsunamis_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_tsunamis_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_tsunamis_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_tsunamis_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_tsunamis_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_tsunamis_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_tsunamis_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_tsunamis_prob_sum_final),
            tsunamis_freq = sum(tsunamis_freq))

birds_breeding_grid2_tsu_0 = birds_breeding_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & tsunamis_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_tsunamis)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_tsunamis_prob_max),avg_impact_sum =  median(weighted_tsunamis_prob_sum),
            avg_impact_max_2 =  median(weighted_tsunamis_prob_max_2),avg_impact_sum_2 =  median(weighted_tsunamis_prob_sum_2),
            avg_impact_max_3 =  median(weighted_tsunamis_prob_max_3),avg_impact_sum_3 =  median(weighted_tsunamis_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_tsunamis_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_tsunamis_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_tsunamis_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_tsunamis_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_tsunamis_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_tsunamis_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_tsunamis_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_tsunamis_prob_sum_final),
            tsunamis_freq = sum(tsunamis_freq))




#eartquakes----


amphibians_grid2$weighted_earthquakes_prob_max = (amphibians_grid2$earthquakes_prob_max)
reptiles_grid2$weighted_earthquakes_prob_max = (reptiles_grid2$earthquakes_prob_max)
mammals_grid2$weighted_earthquakes_prob_max = (mammals_grid2$earthquakes_prob_max)
birds_breeding_grid2$weighted_earthquakes_prob_max = (birds_breeding_grid2$earthquakes_prob_max)

amphibians_grid2$weighted_earthquakes_prob_sum = (amphibians_grid2$earthquakes_prob_sum)
reptiles_grid2$weighted_earthquakes_prob_sum = (reptiles_grid2$earthquakes_prob_sum)
mammals_grid2$weighted_earthquakes_prob_sum = (mammals_grid2$earthquakes_prob_sum)
birds_breeding_grid2$weighted_earthquakes_prob_sum = (birds_breeding_grid2$earthquakes_prob_sum)

amphibians_grid2$weighted_earthquakes_prob_max_2 = (amphibians_grid2$earthquakes_prob_max_2)
reptiles_grid2$weighted_earthquakes_prob_max_2 = (reptiles_grid2$earthquakes_prob_max_2)
mammals_grid2$weighted_earthquakes_prob_max_2 = (mammals_grid2$earthquakes_prob_max_2)
birds_breeding_grid2$weighted_earthquakes_prob_max_2 = (birds_breeding_grid2$earthquakes_prob_max_2)

amphibians_grid2$weighted_earthquakes_prob_sum_2 = (amphibians_grid2$earthquakes_prob_sum_2)
reptiles_grid2$weighted_earthquakes_prob_sum_2 = (reptiles_grid2$earthquakes_prob_sum_2)
mammals_grid2$weighted_earthquakes_prob_sum_2 = (mammals_grid2$earthquakes_prob_sum_2)
birds_breeding_grid2$weighted_earthquakes_prob_sum_2 = (birds_breeding_grid2$earthquakes_prob_sum_2)

amphibians_grid2$weighted_earthquakes_prob_max_3 = (amphibians_grid2$earthquakes_prob_max_3)
reptiles_grid2$weighted_earthquakes_prob_max_3 = (reptiles_grid2$earthquakes_prob_max_3)
mammals_grid2$weighted_earthquakes_prob_max_3 = (mammals_grid2$earthquakes_prob_max_3)
birds_breeding_grid2$weighted_earthquakes_prob_max_3 = (birds_breeding_grid2$earthquakes_prob_max_3)

amphibians_grid2$weighted_earthquakes_prob_sum_3 = (amphibians_grid2$earthquakes_prob_sum_3)
reptiles_grid2$weighted_earthquakes_prob_sum_3 = (reptiles_grid2$earthquakes_prob_sum_3)
mammals_grid2$weighted_earthquakes_prob_sum_3 = (mammals_grid2$earthquakes_prob_sum_3)
birds_breeding_grid2$weighted_earthquakes_prob_sum_3 = (birds_breeding_grid2$earthquakes_prob_sum_3)

amphibians_grid2$weighted_earthquakes_prob_sum_final = (amphibians_grid2$earthquakes_prob_sum_final)
reptiles_grid2$weighted_earthquakes_prob_sum_final = (reptiles_grid2$earthquakes_prob_sum_final)
mammals_grid2$weighted_earthquakes_prob_sum_final = (mammals_grid2$earthquakes_prob_sum_final)
birds_breeding_grid2$weighted_earthquakes_prob_sum_final = (birds_breeding_grid2$earthquakes_prob_sum_final)


# run with all cells

amphibians_grid2$weighted_earthquakes_prob_max_pixel = amphibians_grid2$earthquakes_prob_max
reptiles_grid2$weighted_earthquakes_prob_max_pixel = reptiles_grid2$earthquakes_prob_max
mammals_grid2$weighted_earthquakes_prob_max_pixel = mammals_grid2$earthquakes_prob_max
birds_breeding_grid2$weighted_earthquakes_prob_max_pixel = birds_breeding_grid2$earthquakes_prob_max

amphibians_grid2$weighted_earthquakes_prob_sum_pixel = amphibians_grid2$earthquakes_prob_sum
reptiles_grid2$weighted_earthquakes_prob_sum_pixel = reptiles_grid2$earthquakes_prob_sum
mammals_grid2$weighted_earthquakes_prob_sum_pixel = mammals_grid2$earthquakes_prob_sum
birds_breeding_grid2$weighted_earthquakes_prob_sum_pixel = birds_breeding_grid2$earthquakes_prob_sum

amphibians_grid2$weighted_earthquakes_prob_max_2_pixel = amphibians_grid2$earthquakes_prob_max_2
reptiles_grid2$weighted_earthquakes_prob_max_2_pixel = reptiles_grid2$earthquakes_prob_max_2
mammals_grid2$weighted_earthquakes_prob_max_2_pixel = mammals_grid2$earthquakes_prob_max_2
birds_breeding_grid2$weighted_earthquakes_prob_max_2_pixel = birds_breeding_grid2$earthquakes_prob_max_2

amphibians_grid2$weighted_earthquakes_prob_sum_2_pixel = amphibians_grid2$earthquakes_prob_sum_2
reptiles_grid2$weighted_earthquakes_prob_sum_2_pixel = reptiles_grid2$earthquakes_prob_sum_2
mammals_grid2$weighted_earthquakes_prob_sum_2_pixel = mammals_grid2$earthquakes_prob_sum_2
birds_breeding_grid2$weighted_earthquakes_prob_sum_2_pixel = birds_breeding_grid2$earthquakes_prob_sum_2

amphibians_grid2$weighted_earthquakes_prob_max_3_pixel = amphibians_grid2$earthquakes_prob_max_3
reptiles_grid2$weighted_earthquakes_prob_max_3_pixel = reptiles_grid2$earthquakes_prob_max_3
mammals_grid2$weighted_earthquakes_prob_max_3_pixel = mammals_grid2$earthquakes_prob_max_3
birds_breeding_grid2$weighted_earthquakes_prob_max_3_pixel = birds_breeding_grid2$earthquakes_prob_max_3

amphibians_grid2$weighted_earthquakes_prob_sum_3_pixel = amphibians_grid2$earthquakes_prob_sum_3
reptiles_grid2$weighted_earthquakes_prob_sum_3_pixel = reptiles_grid2$earthquakes_prob_sum_3
mammals_grid2$weighted_earthquakes_prob_sum_3_pixel = mammals_grid2$earthquakes_prob_sum_3
birds_breeding_grid2$weighted_earthquakes_prob_sum_3_pixel = birds_breeding_grid2$earthquakes_prob_sum_3

amphibians_grid2$weighted_earthquakes_prob_sum_final_pixel = amphibians_grid2$earthquakes_prob_sum_final
reptiles_grid2$weighted_earthquakes_prob_sum_final_pixel = reptiles_grid2$earthquakes_prob_sum_final
mammals_grid2$weighted_earthquakes_prob_sum_final_pixel = mammals_grid2$earthquakes_prob_sum_final
birds_breeding_grid2$weighted_earthquakes_prob_sum_final_pixel = birds_breeding_grid2$earthquakes_prob_sum_final



amphibians_grid2_ear_0 = amphibians_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & earthquakes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_earthquakes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_earthquakes_prob_max),avg_impact_sum =  median(weighted_earthquakes_prob_sum),
            avg_impact_max_2 =  median(weighted_earthquakes_prob_max_2),avg_impact_sum_2 =  median(weighted_earthquakes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_earthquakes_prob_max_3),avg_impact_sum_3 =  median(weighted_earthquakes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_earthquakes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_earthquakes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_earthquakes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_earthquakes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_earthquakes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_earthquakes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_earthquakes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_earthquakes_prob_sum_final),
            earthquakes_freq = sum(earthquakes_freq))

reptiles_grid2_ear_0 = reptiles_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & earthquakes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_earthquakes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_earthquakes_prob_max),avg_impact_sum =  median(weighted_earthquakes_prob_sum),
            avg_impact_max_2 =  median(weighted_earthquakes_prob_max_2),avg_impact_sum_2 =  median(weighted_earthquakes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_earthquakes_prob_max_3),avg_impact_sum_3 =  median(weighted_earthquakes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_earthquakes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_earthquakes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_earthquakes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_earthquakes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_earthquakes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_earthquakes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_earthquakes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_earthquakes_prob_sum_final),
            earthquakes_freq = sum(earthquakes_freq))

mammals_grid2_ear_0 = mammals_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & earthquakes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_earthquakes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_earthquakes_prob_max),avg_impact_sum =  median(weighted_earthquakes_prob_sum),
            avg_impact_max_2 =  median(weighted_earthquakes_prob_max_2),avg_impact_sum_2 =  median(weighted_earthquakes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_earthquakes_prob_max_3),avg_impact_sum_3 =  median(weighted_earthquakes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_earthquakes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_earthquakes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_earthquakes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_earthquakes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_earthquakes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_earthquakes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_earthquakes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_earthquakes_prob_sum_final),
            earthquakes_freq = sum(earthquakes_freq))

birds_breeding_grid2_ear_0 = birds_breeding_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & earthquakes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_earthquakes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_earthquakes_prob_max),avg_impact_sum =  median(weighted_earthquakes_prob_sum),
            avg_impact_max_2 =  median(weighted_earthquakes_prob_max_2),avg_impact_sum_2 =  median(weighted_earthquakes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_earthquakes_prob_max_3),avg_impact_sum_3 =  median(weighted_earthquakes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_earthquakes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_earthquakes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_earthquakes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_earthquakes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_earthquakes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_earthquakes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_earthquakes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_earthquakes_prob_sum_final),
            earthquakes_freq = sum(earthquakes_freq))



#hurricanes----

amphibians_grid2$weighted_hurricanes_prob_max = (amphibians_grid2$hurricanes_prob_max)
reptiles_grid2$weighted_hurricanes_prob_max = (reptiles_grid2$hurricanes_prob_max)
mammals_grid2$weighted_hurricanes_prob_max = (mammals_grid2$hurricanes_prob_max)
birds_breeding_grid2$weighted_hurricanes_prob_max = (birds_breeding_grid2$hurricanes_prob_max)

amphibians_grid2$weighted_hurricanes_prob_sum = (amphibians_grid2$hurricanes_prob_sum)
reptiles_grid2$weighted_hurricanes_prob_sum = (reptiles_grid2$hurricanes_prob_sum)
mammals_grid2$weighted_hurricanes_prob_sum = (mammals_grid2$hurricanes_prob_sum)
birds_breeding_grid2$weighted_hurricanes_prob_sum = (birds_breeding_grid2$hurricanes_prob_sum)

amphibians_grid2$weighted_hurricanes_prob_max_2 = (amphibians_grid2$hurricanes_prob_max_2)
reptiles_grid2$weighted_hurricanes_prob_max_2 = (reptiles_grid2$hurricanes_prob_max_2)
mammals_grid2$weighted_hurricanes_prob_max_2 = (mammals_grid2$hurricanes_prob_max_2)
birds_breeding_grid2$weighted_hurricanes_prob_max_2 = (birds_breeding_grid2$hurricanes_prob_max_2)

amphibians_grid2$weighted_hurricanes_prob_sum_2 = (amphibians_grid2$hurricanes_prob_sum_2)
reptiles_grid2$weighted_hurricanes_prob_sum_2 = (reptiles_grid2$hurricanes_prob_sum_2)
mammals_grid2$weighted_hurricanes_prob_sum_2 = (mammals_grid2$hurricanes_prob_sum_2)
birds_breeding_grid2$weighted_hurricanes_prob_sum_2 = (birds_breeding_grid2$hurricanes_prob_sum_2)

amphibians_grid2$weighted_hurricanes_prob_max_3 = (amphibians_grid2$hurricanes_prob_max_3)
reptiles_grid2$weighted_hurricanes_prob_max_3 = (reptiles_grid2$hurricanes_prob_max_3)
mammals_grid2$weighted_hurricanes_prob_max_3 = (mammals_grid2$hurricanes_prob_max_3)
birds_breeding_grid2$weighted_hurricanes_prob_max_3 = (birds_breeding_grid2$hurricanes_prob_max_3)

amphibians_grid2$weighted_hurricanes_prob_sum_3 = (amphibians_grid2$hurricanes_prob_sum_3)
reptiles_grid2$weighted_hurricanes_prob_sum_3 = (reptiles_grid2$hurricanes_prob_sum_3)
mammals_grid2$weighted_hurricanes_prob_sum_3 = (mammals_grid2$hurricanes_prob_sum_3)
birds_breeding_grid2$weighted_hurricanes_prob_sum_3 = (birds_breeding_grid2$hurricanes_prob_sum_3)

amphibians_grid2$weighted_hurricanes_prob_sum_final = (amphibians_grid2$hurricanes_prob_sum_final)
reptiles_grid2$weighted_hurricanes_prob_sum_final = (reptiles_grid2$hurricanes_prob_sum_final)
mammals_grid2$weighted_hurricanes_prob_sum_final = (mammals_grid2$hurricanes_prob_sum_final)
birds_breeding_grid2$weighted_hurricanes_prob_sum_final = (birds_breeding_grid2$hurricanes_prob_sum_final)


# run with all cells

amphibians_grid2$weighted_hurricanes_prob_max_pixel = amphibians_grid2$hurricanes_prob_max
reptiles_grid2$weighted_hurricanes_prob_max_pixel = reptiles_grid2$hurricanes_prob_max
mammals_grid2$weighted_hurricanes_prob_max_pixel = mammals_grid2$hurricanes_prob_max
birds_breeding_grid2$weighted_hurricanes_prob_max_pixel = birds_breeding_grid2$hurricanes_prob_max

amphibians_grid2$weighted_hurricanes_prob_sum_pixel = amphibians_grid2$hurricanes_prob_sum
reptiles_grid2$weighted_hurricanes_prob_sum_pixel = reptiles_grid2$hurricanes_prob_sum
mammals_grid2$weighted_hurricanes_prob_sum_pixel = mammals_grid2$hurricanes_prob_sum
birds_breeding_grid2$weighted_hurricanes_prob_sum_pixel = birds_breeding_grid2$hurricanes_prob_sum

amphibians_grid2$weighted_hurricanes_prob_max_2_pixel = amphibians_grid2$hurricanes_prob_max_2
reptiles_grid2$weighted_hurricanes_prob_max_2_pixel = reptiles_grid2$hurricanes_prob_max_2
mammals_grid2$weighted_hurricanes_prob_max_2_pixel = mammals_grid2$hurricanes_prob_max_2
birds_breeding_grid2$weighted_hurricanes_prob_max_2_pixel = birds_breeding_grid2$hurricanes_prob_max_2

amphibians_grid2$weighted_hurricanes_prob_sum_2_pixel = amphibians_grid2$hurricanes_prob_sum_2
reptiles_grid2$weighted_hurricanes_prob_sum_2_pixel = reptiles_grid2$hurricanes_prob_sum_2
mammals_grid2$weighted_hurricanes_prob_sum_2_pixel = mammals_grid2$hurricanes_prob_sum_2
birds_breeding_grid2$weighted_hurricanes_prob_sum_2_pixel = birds_breeding_grid2$hurricanes_prob_sum_2

amphibians_grid2$weighted_hurricanes_prob_max_3_pixel = amphibians_grid2$hurricanes_prob_max_3
reptiles_grid2$weighted_hurricanes_prob_max_3_pixel = reptiles_grid2$hurricanes_prob_max_3
mammals_grid2$weighted_hurricanes_prob_max_3_pixel = mammals_grid2$hurricanes_prob_max_3
birds_breeding_grid2$weighted_hurricanes_prob_max_3_pixel = birds_breeding_grid2$hurricanes_prob_max_3

amphibians_grid2$weighted_hurricanes_prob_sum_3_pixel = amphibians_grid2$hurricanes_prob_sum_3
reptiles_grid2$weighted_hurricanes_prob_sum_3_pixel = reptiles_grid2$hurricanes_prob_sum_3
mammals_grid2$weighted_hurricanes_prob_sum_3_pixel = mammals_grid2$hurricanes_prob_sum_3
birds_breeding_grid2$weighted_hurricanes_prob_sum_3_pixel = birds_breeding_grid2$hurricanes_prob_sum_3


amphibians_grid2$weighted_hurricanes_prob_sum_final_pixel = amphibians_grid2$hurricanes_prob_sum_final
reptiles_grid2$weighted_hurricanes_prob_sum_final_pixel = reptiles_grid2$hurricanes_prob_sum_final
mammals_grid2$weighted_hurricanes_prob_sum_final_pixel = mammals_grid2$hurricanes_prob_sum_final
birds_breeding_grid2$weighted_hurricanes_prob_sum_final_pixel = birds_breeding_grid2$hurricanes_prob_sum_final



amphibians_grid2_hur_0 = amphibians_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & hurricanes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_hurricanes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_hurricanes_prob_max),avg_impact_sum =  median(weighted_hurricanes_prob_sum),
            avg_impact_max_2 =  median(weighted_hurricanes_prob_max_2),avg_impact_sum_2 =  median(weighted_hurricanes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_hurricanes_prob_max_3),avg_impact_sum_3 =  median(weighted_hurricanes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_hurricanes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_hurricanes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_hurricanes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_hurricanes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_hurricanes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_hurricanes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_hurricanes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_hurricanes_prob_sum_final_pixel),
            hurricanes_freq = sum(hurricanes_freq))

reptiles_grid2_hur_0 = reptiles_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & hurricanes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_hurricanes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_hurricanes_prob_max),avg_impact_sum =  median(weighted_hurricanes_prob_sum),
            avg_impact_max_2 =  median(weighted_hurricanes_prob_max_2),avg_impact_sum_2 =  median(weighted_hurricanes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_hurricanes_prob_max_3),avg_impact_sum_3 =  median(weighted_hurricanes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_hurricanes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_hurricanes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_hurricanes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_hurricanes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_hurricanes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_hurricanes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_hurricanes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_hurricanes_prob_sum_final_pixel),
            hurricanes_freq = sum(hurricanes_freq))

mammals_grid2_hur_0 = mammals_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & hurricanes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_hurricanes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_hurricanes_prob_max),avg_impact_sum =  median(weighted_hurricanes_prob_sum),
            avg_impact_max_2 =  median(weighted_hurricanes_prob_max_2),avg_impact_sum_2 =  median(weighted_hurricanes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_hurricanes_prob_max_3),avg_impact_sum_3 =  median(weighted_hurricanes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_hurricanes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_hurricanes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_hurricanes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_hurricanes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_hurricanes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_hurricanes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_hurricanes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_hurricanes_prob_sum_final_pixel),
            hurricanes_freq = sum(hurricanes_freq))

birds_breeding_grid2_hur_0 = birds_breeding_grid2 %>% group_by(binomial,range_size) %>% 
  filter(percent_range>0 & hurricanes_prob_max > 0) %>% 
  summarise(total_perc_afected =  sum(percent_range),
            total_perc_afected_pixel = 100*unique(n_cells_hurricanes)/unique(range_n_cells),
            range_n_cells = unique(range_n_cells),
            avg_impact_max =  median(weighted_hurricanes_prob_max),avg_impact_sum =  median(weighted_hurricanes_prob_sum),
            avg_impact_max_2 =  median(weighted_hurricanes_prob_max_2),avg_impact_sum_2 =  median(weighted_hurricanes_prob_sum_2),
            avg_impact_max_3 =  median(weighted_hurricanes_prob_max_3),avg_impact_sum_3 =  median(weighted_hurricanes_prob_sum_3),
            avg_impact_max_pixel =  median(weighted_hurricanes_prob_max_pixel),avg_impact_sum_pixel =  median(weighted_hurricanes_prob_sum_pixel),
            avg_impact_max_2_pixel =  median(weighted_hurricanes_prob_max_2_pixel),avg_impact_sum_2_pixel =  median(weighted_hurricanes_prob_sum_2_pixel),
            avg_impact_max_3_pixel =  median(weighted_hurricanes_prob_max_3_pixel),avg_impact_sum_3_pixel =  median(weighted_hurricanes_prob_sum_3_pixel),
            avg_impact_sum_final =  median(weighted_hurricanes_prob_sum_final),avg_impact_sum_final_pixel =  median(weighted_hurricanes_prob_sum_final_pixel),
            hurricanes_freq = sum(hurricanes_freq))


##amphibians
#amphibians vulcanoes ----


theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont,hjust = 0.5),
                               axis.text.y = element_text(family = myfont),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_text(colour = 'white',family = myfont),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))
pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
#pal_earthquakes = colorRampPalette(rev(hcl.colors(5,'YlOrRd')))


####plots----
colnames(amphibians_grid2_vul_0)

hist(amphibians_grid2_vul_0$total_perc_afected)
hist(amphibians_grid2_vul_0$avg_impact_max)

p1_a_v_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_a_v_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_vulcanoes= arrangeGrob(p1_a_v_max,p1_a_v_sum,
                            p1_a_v_max_2,p1_a_v_sum_2,
                            p1_a_v_max_3,p1_a_v_sum_3,
                            p1_a_v_max_pixel,p1_a_v_sum_pixel,
                            p1_a_v_max_2_pixel,p1_a_v_sum_2_pixel,
                            p1_a_v_max_3_pixel,p1_a_v_sum_3_pixel,
                            ncol = 6)

ggsave(filename = 'grid_vulcanoes_amphibians_17_APR.png',plot = grid_vulcanoes,width = 25,height = 5)

p1_a_v_final = ggplot()+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


#amphibians tsunamis ----

p2_a_t_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p2_a_t_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_a_t_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p2_a_t_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_tsunamis= arrangeGrob(p2_a_t_max,p2_a_t_sum,
                           p2_a_t_max_2,p2_a_t_sum_2,
                           p2_a_t_max_3,p2_a_t_sum_3,
                           p2_a_t_max_pixel,p2_a_t_sum_pixel,
                           p2_a_t_max_2_pixel,p2_a_t_sum_2_pixel,
                           p2_a_t_max_3_pixel,p2_a_t_sum_3_pixel,
                           ncol = 6)

ggsave(filename = 'grid_tsunamis_amphibians_17_APR.png',plot = grid_tsunamis,width = 25,height = 5)


#amphibians earthquakes ----

p3_a_e_max = ggplot()+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p3_a_e_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_a_e_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_earthquakes= arrangeGrob(p3_a_e_max,p3_a_e_sum,
                              p3_a_e_max_2,p3_a_e_sum_2,
                              p3_a_e_max_3,p3_a_e_sum_3,
                              p3_a_e_max_pixel,p3_a_e_sum_pixel,
                              p3_a_e_max_2_pixel,p3_a_e_sum_2_pixel,
                              p3_a_e_max_3_pixel,p3_a_e_sum_3_pixel,
                              ncol = 6)

ggsave(filename = 'grid_earthquakes_amphibians_17_APR.png',plot = grid_earthquakes,width = 25,height = 5)


p3_a_e_final = ggplot()+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


#amphibians hurricanes ----

p4_a_h_max = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_max_2 = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_2 = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p4_a_h_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_a_h_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_hurricanes= arrangeGrob(p4_a_h_max,p4_a_h_sum,
                             p4_a_h_max_2,p4_a_h_sum_2,
                             p4_a_h_max_3,p4_a_h_sum_3,
                             p4_a_h_max_pixel,p4_a_h_sum_pixel,
                             p4_a_h_max_2_pixel,p4_a_h_sum_2_pixel,
                             p4_a_h_max_3_pixel,p4_a_h_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_hurricanes_amphibians_17_APR.png',plot = grid_hurricanes,width = 25,height = 5)



p4_a_h_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (Hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')




library(gridExtra)

##reptiles----

#reptiles vulcanoes ----


theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont,hjust = 0.5),
                               axis.text.y = element_text(family = myfont),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_text(colour = 'white',family = myfont),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))
pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
#pal_earthquakes = colorRampPalette(rev(hcl.colors(5,'YlOrRd')))



colnames(reptiles_grid2_vul_0)


p1_r_v_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_max_2 = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_2 = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_final = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_r_v_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_vulcanoes= arrangeGrob(p1_r_v_max,p1_r_v_sum,
                            p1_r_v_max_2,p1_r_v_sum_2,
                            p1_r_v_max_3,p1_r_v_sum_3,
                            p1_r_v_max_pixel,p1_r_v_sum_pixel,
                            p1_r_v_max_2_pixel,p1_r_v_sum_2_pixel,
                            p1_r_v_max_3_pixel,p1_r_v_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_vulcanoes_reptiles_17_APR.png',plot = grid_vulcanoes,width = 25,height = 5)


p1_r_v_final = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left',
        axis.title.x = element_text(colour = 'black'))

#reptiles tsunamis ----

p2_r_t_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p2_r_t_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_r_t_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_tsunamis= arrangeGrob(p2_r_t_max,p2_r_t_sum,
                           p2_r_t_max_2,p2_r_t_sum_2,
                           p2_r_t_max_3,p2_r_t_sum_3,
                           p2_r_t_max_pixel,p2_r_t_sum_pixel,
                           p2_r_t_max_2_pixel,p2_r_t_sum_2_pixel,
                           p2_r_t_max_3_pixel,p2_r_t_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_tsunamis_reptiles_17_APR.png',plot = grid_tsunamis,width = 25,height = 5)

p2_r_t_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left',
        axis.title.x = element_text(colour = 'black'))

#reptiles earthquakes ----

p3_r_e_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p3_r_e_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_r_e_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_earthquakes= arrangeGrob(p3_r_e_max,p3_r_e_sum,
                              p3_r_e_max_2,p3_r_e_sum_2,
                              p3_r_e_max_3,p3_r_e_sum_3,
                              p3_r_e_max_pixel,p3_r_e_sum_pixel,
                              p3_r_e_max_2_pixel,p3_r_e_sum_2_pixel,
                              p3_r_e_max_3_pixel,p3_r_e_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_earthquakes_reptiles_17_APR.png',plot = grid_earthquakes,width = 25,height = 5)

p3_r_e_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left',
        axis.title.x = element_text(colour = 'black'))


#reptiles hurricanes ----

p4_r_h_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p4_r_h_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_r_h_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_hurricanes= arrangeGrob(p4_r_h_max,p4_r_h_sum,
                             p4_r_h_max_2,p4_r_h_sum_2,
                             p4_r_h_max_3,p4_r_h_sum_3,
                             p4_r_h_max_pixel,p4_r_h_sum_pixel,
                             p4_r_h_max_2_pixel,p4_r_h_sum_2_pixel,
                             p4_r_h_max_3_pixel,p4_r_h_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_hurricanes_reptiles_17_APR.png',plot = grid_hurricanes,width = 25,height = 5)

p4_r_h_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'reptiles (Hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left',
        axis.title.x = element_text(colour = 'black'))


library(gridExtra)

##mammals----

#mammals vulcanoes ----


theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont,hjust = 0.5),
                               axis.text.y = element_text(family = myfont),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_text(colour = 'white',family = myfont),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))
pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
#pal_earthquakes = colorRampPalette(rev(hcl.colors(5,'YlOrRd')))



colnames(mammals_grid2_vul_0)


p1_m_v_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_m_v_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_vulcanoes= arrangeGrob(p1_m_v_max,p1_m_v_sum,
                            p1_m_v_max_2,p1_m_v_sum_2,
                            p1_m_v_max_3,p1_m_v_sum_3,
                            p1_m_v_max_pixel,p1_m_v_sum_pixel,
                            p1_m_v_max_2_pixel,p1_m_v_sum_2_pixel,
                            p1_m_v_max_3_pixel,p1_m_v_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_vulcanoes_mammals_17_APR.png',plot = grid_vulcanoes,width = 25,height = 5)


p1_m_v_final = ggplot()+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

#mammals tsunamis ----

p2_m_t_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p2_m_t_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_final= ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_m_t_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_tsunamis= arrangeGrob(p2_m_t_max,p2_m_t_sum,
                           p2_m_t_max_2,p2_m_t_sum_2,
                           p2_m_t_max_3,p2_m_t_sum_3,
                           p2_m_t_max_pixel,p2_m_t_sum_pixel,
                           p2_m_t_max_2_pixel,p2_m_t_sum_2_pixel,
                           p2_m_t_max_3_pixel,p2_m_t_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_tsunamis_mammals_17_APR.png',plot = grid_tsunamis,width = 20,height = 5)


p2_m_t_final= ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


#mammals earthquakes ----

p3_m_e_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p3_m_e_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_final = ggplot()+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_m_e_sum_final_pixel = ggplot()+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_earthquakes= arrangeGrob(p3_m_e_max,p3_m_e_sum,
                              p3_m_e_max_2,p3_m_e_sum_2,
                              p3_m_e_max_3,p3_m_e_sum_3,
                              p3_m_e_max_pixel,p3_m_e_sum_pixel,
                              p3_m_e_max_2_pixel,p3_m_e_sum_2_pixel,
                              p3_m_e_max_3_pixel,p3_m_e_sum_3_pixel,
                              ncol = 6)

ggsave(filename = 'grid_earthquakes_mammals_17_APR.png',plot = grid_earthquakes,width = 25,height = 5)


p3_m_e_final = ggplot()+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


#mammals hurricanes ----

p4_m_h_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p4_m_h_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_m_h_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p4_m_h_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')



library(gridExtra)
grid_hurricanes= arrangeGrob(p4_m_h_max,p4_m_h_sum,
                             p4_m_h_max_2,p4_m_h_sum_2,
                             p4_m_h_max_3,p4_m_h_sum_3,
                             p4_m_h_max_pixel,p4_m_h_sum_pixel,
                             p4_m_h_max_2_pixel,p4_m_h_sum_2_pixel,
                             p4_m_h_max_3_pixel,p4_m_h_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_hurricanes_mammals_17_APR.png',plot = grid_hurricanes,width = 25,height = 5)


p4_m_h_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'mammals (Hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

library(gridExtra)





##birds----

#birds_breeding vulcanoes ----


theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont,hjust = 0.5),
                               axis.text.y = element_text(family = myfont),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_text(colour = 'white',family = myfont),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal_vul = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

pal_hurricanes = colorRampPalette(rev(hcl.colors(5,'PurpOr')))
pal_tsunamis = colorRampPalette(rev(hcl.colors(5,'ag_GrnYl')))
pal_earthquakes = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))
#pal_earthquakes = colorRampPalette(rev(hcl.colors(5,'YlOrRd')))



colnames(birds_breeding_grid2_vul_0)


p1_b_v_max = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p1_b_v_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_vulcanoes= arrangeGrob(p1_b_v_max,p1_b_v_sum,
                            p1_b_v_max_2,p1_b_v_sum_2,
                            p1_b_v_max_3,p1_b_v_sum_3,
                            p1_b_v_max_pixel,p1_b_v_sum_pixel,
                            p1_b_v_max_2_pixel,p1_b_v_sum_2_pixel,
                            p1_b_v_max_3_pixel,p1_b_v_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_vulcanoes_birds_breeding_17_APR.png',plot = grid_vulcanoes,width = 25,height = 5)


p1_b_v_final = ggplot()+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = vulcanos_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Volcanoes)',colors = alpha(pal_vul(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

#birds_breeding tsunamis ----

p2_b_t_max = ggplot()+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p2_b_t_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p2_b_t_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_tsunamis= arrangeGrob(p2_b_t_max,p2_b_t_sum,
                           p2_b_t_max_2,p2_b_t_sum_2,
                           p2_b_t_max_3,p2_b_t_sum_3,
                           p2_b_t_max_pixel,p2_b_t_sum_pixel,
                           p2_b_t_max_2_pixel,p2_b_t_sum_2_pixel,
                           p2_b_t_max_3_pixel,p2_b_t_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_tsunamis_birds_breeding_17_APR.png',plot = grid_tsunamis,width = 25,height = 5)


p2_b_t_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Tsunamis)',colors = alpha(pal_tsunamis(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


#birds_breeding earthquakes ----

p3_b_e_max = ggplot()+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_max_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_2 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_max_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_3 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p3_b_e_max_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_max_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_2_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_max_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_3_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p3_b_e_sum_final_pixel = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_earthquakes= arrangeGrob(p3_b_e_max,p3_b_e_sum,
                              p3_b_e_max_2,p3_b_e_sum_2,
                              p3_b_e_max_3,p3_b_e_sum_3,
                              p3_b_e_max_pixel,p3_b_e_sum_pixel,
                              p3_b_e_max_2_pixel,p3_b_e_sum_2_pixel,
                              p3_b_e_max_3_pixel,p3_b_e_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_earthquakes_birds_breeding_17_APR.png',plot = grid_earthquakes,width = 25,height = 5)


p3_b_e_final = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'birds_breeding (Earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Earthquakes)',colors = alpha(pal_earthquakes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

#birds_breeding hurricanes ----


p4_b_h_max = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_max_2 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_2 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_2,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_max_3 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_max_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(max3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_3 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_3,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(sum3')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


p4_b_h_max_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_max_2_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_2_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_2_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_2)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_max_3_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_max_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_max_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_3_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_3_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_3)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_final = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

p4_b_h_sum_final_pixel = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected_pixel, y = avg_impact_sum_final_pixel,size = log10(range_n_cells),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact(p_sum_final)')+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')


library(gridExtra)
grid_hurricanes= arrangeGrob(p4_b_h_max,p4_b_h_sum,
                             p4_b_h_max_2,p4_b_h_sum_2,
                             p4_b_h_max_3,p4_b_h_sum_3,
                             p4_b_h_max_pixel,p4_b_h_sum_pixel,
                             p4_b_h_max_2_pixel,p4_b_h_sum_2_pixel,
                             p4_b_h_max_3_pixel,p4_b_h_sum_3_pixel,ncol = 6)

ggsave(filename = 'grid_hurricanes_birds_breeding_17_APR.png',plot = grid_hurricanes,width = 25,height = 5)

p4_b_h_final = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact_sum_final,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Frequency (Hurricanes)',colors = alpha(pal_hurricanes(10),0.5))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  scale_y_continuous(limits = c(0,1.01),breaks = c(0.0,0.25,0.5,0.75,1.0))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.01), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.y = element_text(colour = 'black',hjust = 1,size = 15))+
  theme(legend.position = 'left')

library(gridExtra)


grid = arrangeGrob(p1_a_v_final,p3_a_e_final,p2_a_t_final,p4_a_h_final,
                   p1_b_v_final,p3_b_e_final,p2_b_t_final,p4_b_h_final,
                   p1_m_v_final,p3_m_e_final,p2_m_t_final,p4_m_h_final,
                   p1_r_v_final,p3_r_e_final,p2_r_t_final,p4_r_h_final,ncol = 4)


ggsave(filename = 'final_grid_17_APR.png',plot = grid,width = 15,height = 10)

grid_pixel = arrangeGrob(p1_a_v_sum_final_pixel,p3_a_e_sum_final_pixel,p2_a_t_sum_final_pixel,p4_a_h_sum_final_pixel,
                   p1_b_v_sum_final_pixel,p3_b_e_sum_final_pixel,p2_b_t_sum_final_pixel,p4_b_h_sum_final_pixel,
                   p1_m_v_sum_final_pixel,p3_m_e_sum_final_pixel,p2_m_t_sum_final_pixel,p4_m_h_sum_final_pixel,
                   p1_r_v_sum_final_pixel,p3_r_e_sum_final_pixel,p2_r_t_sum_final_pixel,p4_r_h_sum_final_pixel,
                   ncol = 4)


ggsave(filename = 'final_grid_17_APR_pixel.png',plot = grid_pixel,width = 15,height = 10)




#maps_greyed----

list_amphibians_cells = unique(amphibians_grid$layer)
list_reptiles_cells = unique(reptiles_grid$layer)
list_mammals_cells = unique(mammals_grid$layer)
list_birds_cells = unique(birds_breeding_grid$layer)

greyed_cells = unique(c(list_amphibians_cells,list_reptiles_cells,list_mammals_cells,list_birds_cells))
save(greyed_cells,file = 'greyed_cells_run5.Rdata')



# exporting full excel files
write.xlsx(amphibians_grid2_ear_0,file = 'amphibians_grid2_ear_0.xlsx')
write.xlsx(amphibians_grid2_vul_0,file = 'amphibians_grid2_vul_0.xlsx')
write.xlsx(amphibians_grid2_hur_0,file = 'amphibians_grid2_hur_0.xlsx')
write.xlsx(amphibians_grid2_tsu_0,file = 'amphibians_grid2_tsu_0.xlsx')

write.xlsx(mammals_grid2_ear_0,file = 'mammals_grid2_ear_0.xlsx')
write.xlsx(mammals_grid2_vul_0,file = 'mammals_grid2_vul_0.xlsx')
write.xlsx(mammals_grid2_hur_0,file = 'mammals_grid2_hur_0.xlsx')
write.xlsx(mammals_grid2_tsu_0,file = 'mammals_grid2_tsu_0.xlsx')

write.xlsx(reptiles_grid2_ear_0,file = 'reptiles_grid2_ear_0.xlsx')
write.xlsx(reptiles_grid2_vul_0,file = 'reptiles_grid2_vul_0.xlsx')
write.xlsx(reptiles_grid2_hur_0,file = 'reptiles_grid2_hur_0.xlsx')
write.xlsx(reptiles_grid2_tsu_0,file = 'reptiles_grid2_tsu_0.xlsx')

write.xlsx(birds_breeding_grid2_ear_0,file = 'birds_breeding_grid2_ear_0.xlsx')
write.xlsx(birds_breeding_grid2_vul_0,file = 'birds_breeding_grid2_vul_0.xlsx')
write.xlsx(birds_breeding_grid2_hur_0,file = 'birds_breeding_grid2_hur_0.xlsx')
write.xlsx(birds_breeding_grid2_tsu_0,file = 'birds_breeding_grid2_tsu_0.xlsx')



# excel files----
colnames(amphibians_grid2_ear_0)[2] = paste0(colnames(amphibians_grid2_ear_0)[2],'_ear')
colnames(amphibians_grid2_ear_0)[3] = paste0(colnames(amphibians_grid2_ear_0)[3],'_ear')
colnames(amphibians_grid2_ear_0)[4] = paste0(colnames(amphibians_grid2_ear_0)[4],'_ear')
colnames(amphibians_grid2_ear_0)[5] = paste0(colnames(amphibians_grid2_ear_0)[5],'_ear')
colnames(amphibians_grid2_ear_0)[18] = paste0(colnames(amphibians_grid2_ear_0)[18],'_ear')

colnames(amphibians_grid2_tsu_0)[2] = paste0(colnames(amphibians_grid2_tsu_0)[2],'_tsu')
colnames(amphibians_grid2_tsu_0)[3] = paste0(colnames(amphibians_grid2_tsu_0)[3],'_tsu')
colnames(amphibians_grid2_tsu_0)[4] = paste0(colnames(amphibians_grid2_tsu_0)[4],'_tsu')
colnames(amphibians_grid2_tsu_0)[5] = paste0(colnames(amphibians_grid2_tsu_0)[5],'_tsu')
colnames(amphibians_grid2_tsu_0)[18] = paste0(colnames(amphibians_grid2_tsu_0)[18],'_tsu')

colnames(amphibians_grid2_vul_0)[2] = paste0(colnames(amphibians_grid2_vul_0)[2],'_vul')
colnames(amphibians_grid2_vul_0)[3] = paste0(colnames(amphibians_grid2_vul_0)[3],'_vul')
colnames(amphibians_grid2_vul_0)[4] = paste0(colnames(amphibians_grid2_vul_0)[4],'_vul')
colnames(amphibians_grid2_vul_0)[5] = paste0(colnames(amphibians_grid2_vul_0)[5],'_vul')
colnames(amphibians_grid2_vul_0)[18] = paste0(colnames(amphibians_grid2_vul_0)[18],'_vul')

colnames(amphibians_grid2_hur_0)[2] = paste0(colnames(amphibians_grid2_hur_0)[2],'_hur')
colnames(amphibians_grid2_hur_0)[3] = paste0(colnames(amphibians_grid2_hur_0)[3],'_hur')
colnames(amphibians_grid2_hur_0)[4] = paste0(colnames(amphibians_grid2_hur_0)[4],'_hur')
colnames(amphibians_grid2_hur_0)[5] = paste0(colnames(amphibians_grid2_hur_0)[5],'_hur')
colnames(amphibians_grid2_hur_0)[18] = paste0(colnames(amphibians_grid2_hur_0)[18],'_hur')


amphibians_0 = merge(amphibians_grid2_ear_0[,c(1:5,18,20)],amphibians_grid2_tsu_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
amphibians_1 = merge(amphibians_0,amphibians_grid2_vul_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
amphibians_final = merge(amphibians_1,amphibians_grid2_hur_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)

library(openxlsx)
write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disasters_run7_APRL_17.xlsx')

## reptiles

colnames(reptiles_grid2_ear_0)[2] = paste0(colnames(reptiles_grid2_ear_0)[2],'_ear')
colnames(reptiles_grid2_ear_0)[3] = paste0(colnames(reptiles_grid2_ear_0)[3],'_ear')
colnames(reptiles_grid2_ear_0)[4] = paste0(colnames(reptiles_grid2_ear_0)[4],'_ear')
colnames(reptiles_grid2_ear_0)[5] = paste0(colnames(reptiles_grid2_ear_0)[5],'_ear')
colnames(reptiles_grid2_ear_0)[18] = paste0(colnames(reptiles_grid2_ear_0)[18],'_ear')

colnames(reptiles_grid2_tsu_0)[2] = paste0(colnames(reptiles_grid2_tsu_0)[2],'_tsu')
colnames(reptiles_grid2_tsu_0)[3] = paste0(colnames(reptiles_grid2_tsu_0)[3],'_tsu')
colnames(reptiles_grid2_tsu_0)[4] = paste0(colnames(reptiles_grid2_tsu_0)[4],'_tsu')
colnames(reptiles_grid2_tsu_0)[5] = paste0(colnames(reptiles_grid2_tsu_0)[5],'_tsu')
colnames(reptiles_grid2_tsu_0)[18] = paste0(colnames(reptiles_grid2_tsu_0)[18],'_tsu')

colnames(reptiles_grid2_vul_0)[2] = paste0(colnames(reptiles_grid2_vul_0)[2],'_vul')
colnames(reptiles_grid2_vul_0)[3] = paste0(colnames(reptiles_grid2_vul_0)[3],'_vul')
colnames(reptiles_grid2_vul_0)[4] = paste0(colnames(reptiles_grid2_vul_0)[4],'_vul')
colnames(reptiles_grid2_vul_0)[5] = paste0(colnames(reptiles_grid2_vul_0)[5],'_vul')
colnames(reptiles_grid2_vul_0)[18] = paste0(colnames(reptiles_grid2_vul_0)[18],'_vul')

colnames(reptiles_grid2_hur_0)[2] = paste0(colnames(reptiles_grid2_hur_0)[2],'_hur')
colnames(reptiles_grid2_hur_0)[3] = paste0(colnames(reptiles_grid2_hur_0)[3],'_hur')
colnames(reptiles_grid2_hur_0)[4] = paste0(colnames(reptiles_grid2_hur_0)[4],'_hur')
colnames(reptiles_grid2_hur_0)[5] = paste0(colnames(reptiles_grid2_hur_0)[5],'_hur')
colnames(reptiles_grid2_hur_0)[18] = paste0(colnames(reptiles_grid2_hur_0)[18],'_hur')


reptiles_0 = merge(reptiles_grid2_ear_0[,c(1:5,18,20)],reptiles_grid2_tsu_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
reptiles_1 = merge(reptiles_0,reptiles_grid2_vul_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
reptiles_final = merge(reptiles_1,reptiles_grid2_hur_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)

write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disasters_run7_APRL_17.xlsx')
nrow(reptiles_final)

## mammals

colnames(mammals_grid2_ear_0)[2] = paste0(colnames(mammals_grid2_ear_0)[2],'_ear')
colnames(mammals_grid2_ear_0)[3] = paste0(colnames(mammals_grid2_ear_0)[3],'_ear')
colnames(mammals_grid2_ear_0)[4] = paste0(colnames(mammals_grid2_ear_0)[4],'_ear')
colnames(mammals_grid2_ear_0)[5] = paste0(colnames(mammals_grid2_ear_0)[5],'_ear')
colnames(mammals_grid2_ear_0)[18] = paste0(colnames(mammals_grid2_ear_0)[18],'_ear')

colnames(mammals_grid2_tsu_0)[2] = paste0(colnames(mammals_grid2_tsu_0)[2],'_tsu')
colnames(mammals_grid2_tsu_0)[3] = paste0(colnames(mammals_grid2_tsu_0)[3],'_tsu')
colnames(mammals_grid2_tsu_0)[4] = paste0(colnames(mammals_grid2_tsu_0)[4],'_tsu')
colnames(mammals_grid2_tsu_0)[5] = paste0(colnames(mammals_grid2_tsu_0)[5],'_tsu')
colnames(mammals_grid2_tsu_0)[18] = paste0(colnames(mammals_grid2_tsu_0)[18],'_tsu')

colnames(mammals_grid2_vul_0)[2] = paste0(colnames(mammals_grid2_vul_0)[2],'_vul')
colnames(mammals_grid2_vul_0)[3] = paste0(colnames(mammals_grid2_vul_0)[3],'_vul')
colnames(mammals_grid2_vul_0)[4] = paste0(colnames(mammals_grid2_vul_0)[4],'_vul')
colnames(mammals_grid2_vul_0)[5] = paste0(colnames(mammals_grid2_vul_0)[5],'_vul')
colnames(mammals_grid2_vul_0)[18] = paste0(colnames(mammals_grid2_vul_0)[18],'_vul')

colnames(mammals_grid2_hur_0)[2] = paste0(colnames(mammals_grid2_hur_0)[2],'_hur')
colnames(mammals_grid2_hur_0)[3] = paste0(colnames(mammals_grid2_hur_0)[3],'_hur')
colnames(mammals_grid2_hur_0)[4] = paste0(colnames(mammals_grid2_hur_0)[4],'_hur')
colnames(mammals_grid2_hur_0)[5] = paste0(colnames(mammals_grid2_hur_0)[5],'_hur')
colnames(mammals_grid2_hur_0)[18] = paste0(colnames(mammals_grid2_hur_0)[18],'_hur')

mammals_0 = merge(mammals_grid2_ear_0[,c(1:5,18,20)],mammals_grid2_tsu_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
mammals_1 = merge(mammals_0,mammals_grid2_vul_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
mammals_final = merge(mammals_1,mammals_grid2_hur_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)

write.xlsx(mammals_final,file = 'mammals_grid2_all_disasters_run7_APRL_17.xlsx')
nrow(mammals_final)

## birds_breeding

colnames(birds_breeding_grid2_ear_0)[2] = paste0(colnames(birds_breeding_grid2_ear_0)[2],'_ear')
colnames(birds_breeding_grid2_ear_0)[3] = paste0(colnames(birds_breeding_grid2_ear_0)[3],'_ear')
colnames(birds_breeding_grid2_ear_0)[4] = paste0(colnames(birds_breeding_grid2_ear_0)[4],'_ear')
colnames(birds_breeding_grid2_ear_0)[5] = paste0(colnames(birds_breeding_grid2_ear_0)[5],'_ear')
colnames(birds_breeding_grid2_ear_0)[18] = paste0(colnames(birds_breeding_grid2_ear_0)[18],'_ear')

colnames(birds_breeding_grid2_tsu_0)[2] = paste0(colnames(birds_breeding_grid2_tsu_0)[2],'_tsu')
colnames(birds_breeding_grid2_tsu_0)[3] = paste0(colnames(birds_breeding_grid2_tsu_0)[3],'_tsu')
colnames(birds_breeding_grid2_tsu_0)[4] = paste0(colnames(birds_breeding_grid2_tsu_0)[4],'_tsu')
colnames(birds_breeding_grid2_tsu_0)[5] = paste0(colnames(birds_breeding_grid2_tsu_0)[5],'_tsu')
colnames(birds_breeding_grid2_tsu_0)[18] = paste0(colnames(birds_breeding_grid2_tsu_0)[18],'_tsu')

colnames(birds_breeding_grid2_vul_0)[2] = paste0(colnames(birds_breeding_grid2_vul_0)[2],'_vul')
colnames(birds_breeding_grid2_vul_0)[3] = paste0(colnames(birds_breeding_grid2_vul_0)[3],'_vul')
colnames(birds_breeding_grid2_vul_0)[4] = paste0(colnames(birds_breeding_grid2_vul_0)[4],'_vul')
colnames(birds_breeding_grid2_vul_0)[5] = paste0(colnames(birds_breeding_grid2_vul_0)[5],'_vul')
colnames(birds_breeding_grid2_vul_0)[18] = paste0(colnames(birds_breeding_grid2_vul_0)[18],'_vul')

colnames(birds_breeding_grid2_hur_0)[2] = paste0(colnames(birds_breeding_grid2_hur_0)[2],'_hur')
colnames(birds_breeding_grid2_hur_0)[3] = paste0(colnames(birds_breeding_grid2_hur_0)[3],'_hur')
colnames(birds_breeding_grid2_hur_0)[4] = paste0(colnames(birds_breeding_grid2_hur_0)[4],'_hur')
colnames(birds_breeding_grid2_hur_0)[5] = paste0(colnames(birds_breeding_grid2_hur_0)[5],'_hur')
colnames(birds_breeding_grid2_hur_0)[18] = paste0(colnames(birds_breeding_grid2_hur_0)[18],'_hur')

birds_breeding_0 = merge(birds_breeding_grid2_ear_0[,c(1:5,18,20)],birds_breeding_grid2_tsu_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
birds_breeding_1 = merge(birds_breeding_0,birds_breeding_grid2_vul_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)
birds_breeding_final = merge(birds_breeding_1,birds_breeding_grid2_hur_0[,c(1:5,18,20)],by = 'binomial',all.x=T,all.y = T)

write.xlsx(birds_breeding_final,file = 'birds_breeding_grid2_all_disasters_run7_APRL_17.xlsx')

nrow(amphibians_final)+
  nrow(reptiles_final)+
  nrow(mammals_final)+
  nrow(birds_breeding_final)

colnames(amphibians_final)
# amphibians_final$threshold_any = ifelse(amphibians_final$total_perc_afected_ear>24.5 & amphibians_final$avg_impact_ear>0.245 |
#                                           amphibians_final$total_perc_afected_tsu>24.5 & amphibians_final$avg_impact_tsu>0.245|
#                                           amphibians_final$total_perc_afected_vul>24.5 & amphibians_final$avg_impact_vul>0.245|
#                                           amphibians_final$total_perc_afected_hur>24.5 & amphibians_final$avg_impact_hur>0.245,1,0)
# 
# 
# amphibians_final %>% filter(threshold_any==1) %>% nrow()/amphibians_final %>% nrow()
# write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disasters_run4.xlsx')
# 
# 
# reptiles_final$threshold_any = ifelse(reptiles_final$total_perc_afected_ear>24.5 & reptiles_final$avg_impact_ear>0.245 |
#                                         reptiles_final$total_perc_afected_tsu>24.5 & reptiles_final$avg_impact_tsu>0.245|
#                                         reptiles_final$total_perc_afected_vul>24.5 & reptiles_final$avg_impact_vul>0.245|
#                                         reptiles_final$total_perc_afected_hur>24.5 & reptiles_final$avg_impact_hur>0.245,1,0)
# 
# 
# reptiles_final %>% filter(threshold_any==1) %>% nrow()/reptiles_final %>% nrow()
# write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disasters_run4.xlsx')
# 
# mammals_final$threshold_any = ifelse(mammals_final$total_perc_afected_ear>24.5 & mammals_final$avg_impact_ear>0.245 |
#                                        mammals_final$total_perc_afected_tsu>24.5 & mammals_final$avg_impact_tsu>0.245|
#                                        mammals_final$total_perc_afected_vul>24.5 & mammals_final$avg_impact_vul>0.245|
#                                        mammals_final$total_perc_afected_hur>24.5 & mammals_final$avg_impact_hur>0.245,1,0)
# 
# 
# mammals_final %>% filter(threshold_any==1) %>% nrow()/mammals_final %>% nrow()
# write.xlsx(mammals_final,file = 'mammals_grid2_all_disasters_run4.xlsx')
# 
# 
# birds_breeding_final$threshold_any = ifelse(birds_breeding_final$total_perc_afected_ear>24.5 & birds_breeding_final$avg_impact_ear>0.245 |
#                                               birds_breeding_final$total_perc_afected_tsu>24.5 & birds_breeding_final$avg_impact_tsu>0.245|
#                                               birds_breeding_final$total_perc_afected_vul>24.5 & birds_breeding_final$avg_impact_vul>0.245|
#                                               birds_breeding_final$total_perc_afected_hur>24.5 & birds_breeding_final$avg_impact_hur>0.245,1,0)
# 
# 
# birds_breeding_final %>% filter(threshold_any==1) %>% nrow()/birds_breeding_final %>% nrow()
# write.xlsx(birds_breeding_final,file = 'birds_breeding_grid2_all_disasters_run4.xlsx')
# 

#I just checked the data, the number of species that meet our criteria is 8131. Birds = 918, Mammals = 811, Reptiles = 3138, amphibians = 3264
nrow(amphibians_final)/3264
nrow(reptiles_final)/3138
nrow(mammals_final)/811
nrow(birds_breeding_final)/918


#amphibians
amphibians_taxonomy = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/taxonomy.csv')  
amphibians_assessments = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/amphibians_threats/assessments.csv')  
colnames(amphibians_taxonomy)
colnames(amphibians_assessments)
amphibians_taxonomy_assessments = merge(amphibians_taxonomy[,c(2,5,6,7)],amphibians_assessments[,c(3,4)],by = 'scientificName')  
amphibians_final = merge(amphibians_final,amphibians_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  



#reptiles
reptiles_taxonomy1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/taxonomy.csv')  
reptiles_taxonomy2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/taxonomy.csv')  
reptiles_taxonomy = rbind(reptiles_taxonomy1,reptiles_taxonomy2)
reptiles_assessments1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_0e1e13b5-f355-4302-9cf4-822d33b08258/assessments.csv')
reptiles_assessments2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/Threats/reptiles/redlist_species_data_1e6a78a5-c09e-430f-906e-a710a61fb4b0/assessments.csv')
reptiles_assessments = rbind(reptiles_assessments1,reptiles_assessments2)
colnames(reptiles_taxonomy)
colnames(reptiles_assessments)
reptiles_taxonomy_assessments = merge(reptiles_taxonomy[,c(2,5,6,7)],reptiles_assessments[,c(3,4)],by = 'scientificName')  
reptiles_final = merge(reptiles_final,reptiles_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

reptiles_final[is.na(reptiles_final$className),]

#mammals
mammals_taxonomy = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Mammals/taxonomy.csv')  
mammals_assessments = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/2022_2/Mammals/assessments.csv')  
colnames(mammals_taxonomy)
colnames(mammals_assessments)
mammals_taxonomy_assessments = merge(mammals_taxonomy[,c(2,5,6,7)],mammals_assessments[,c(3,4)],by = 'scientificName')  
mammals_final = merge(mammals_final,mammals_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

mammals_final[is.na(mammals_final$className),]

#birds
birds_taxonomy1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/taxonomy.csv') 
birds_taxonomy2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/taxonomy.csv')  
birds_taxonomy = rbind(birds_taxonomy1,birds_taxonomy2)

birds_assessments1 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds1/assessments.csv')
birds_assessments2 = read.csv('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/birds2/assessments.csv') 
birds_assessments = rbind(birds_assessments1,birds_assessments2)


colnames(birds_taxonomy)
colnames(birds_assessments)
birds_taxonomy_assessments = merge(birds_taxonomy[,c(2,5,6,7)],birds_assessments[,c(3,4)],by = 'scientificName')  
birds_final = merge(birds_breeding_final,birds_taxonomy_assessments,by.x = 'binomial',by.y = 'scientificName',all.x = T)  

birds_final[is.na(birds_final$className),]

write.xlsx(amphibians_final,file = 'amphibians_grid2_all_disastersv2_run7.xlsx')
write.xlsx(reptiles_final,file = 'reptiles_grid2_all_disastersv2_run7.xlsx')
write.xlsx(mammals_final,file = 'mammals_grid2_all_disastersv2_run7.xlsx')
write.xlsx(birds_final,file = 'birds_breeding_grid2_all_disastersv2_run7.xlsx')





##amphibians----

theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'none',
                               legend.key.height = unit(1,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(0.25,'cm'),
                               legend.title = element_text(size = 10,angle = 90,family = myfont),
                               axis.text.y = element_blank(),
                               axis.text.x = element_text(family = myfont),
                               axis.title.y = element_blank(),
                               axis.title.x = element_text(colour = 'white',family = myfont),
                               legend.text = element_text(family = myfont))

pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_a_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (tsunamis)',colors = alpha(pal1(10),0.75))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_a_t = ggplot()+
  #labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Tsunamis)',colors = alpha(pal1(10),0.75))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_a_e = ggplot()+
  #labs(title = 'Amphibians (earthquakes)')+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = earthquakes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Earthquakes)',colors = alpha(pal1(10),0.75))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

amphibians_grid2_hur_0$hurricanes_freq
p4_a_h = ggplot()+
  #labs(title = 'Amphibians (hurricanes)')+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = hurricanes_freq),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Impact probability (Hurricanes)',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'left'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(legend.position = 'left')

# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_amphibians.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

library(gridExtra)

##reptiles----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_r_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_r_t = ggplot()+
  #labs(title = 'Reptiles (tsunamis)')+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_r_e = ggplot()+
  #labs(title = 'Reptiles (earthquakes)')+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_r_h = ggplot()+
  #labs(title = 'Reptiles (hurricanes)')+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters


# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_reptiles.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

##mammals----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_m_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Mammals (tsunamis)')+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_m_t = ggplot()+
  #labs(title = 'Mammals (tsunamis)')+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_m_e = ggplot()+
  #labs(title = 'Mammals (earthquakes)')+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_m_h = ggplot()+
  #labs(title = 'Mammals (hurricanes)')+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters


# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_mammals.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)


##birds----
pal1 = colorRampPalette(c('#fee0d2','#fc9272','#de2d26'))

p1_b_v = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  #labs(title = 'Birds (breeding) (tsunamis)')+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#edf8b1','#7fcdbb','#2c7fb8'))

p2_b_t = ggplot()+
  #labs(title = 'Birds (breeding) (tsunamis)')+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fff7bc','#fec44f','#d95f0e'))

p3_b_e = ggplot()+
  #labs(title = 'Birds (breeding) (earthquakes)')+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters

pal1 = colorRampPalette(c('#fde0dd','#fa9fb5','#c51b8a'))

p4_b_h = ggplot()+
  #labs(title = 'Birds (breeding) (hurricanes)')+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = alpha(pal1(10),0.5))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  guides(fill = guide_colorbar(title.position = 'top'))+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill=NA, alpha=0.2,col = 'red')+
  theme_points_disasters+
  theme(axis.title.x = element_text(colour = 'black',hjust = 1,size = 15))



# grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)
# 
# ggsave(filename = 'points_birds_breeding.png',plot = grid_earthquakes,width = 8,height = 6,dpi = 600)

## grid all ----

grid_all= arrangeGrob(p1_a_v,p1_r_v,p1_m_v,p1_b_v,
                      p2_a_t,p2_r_t,p2_m_t,p2_b_t,
                      p3_a_e,p3_r_e,p3_m_e,p3_b_e,
                      p4_a_h,p4_r_h,p4_m_h,p4_b_h,
                      ncol = 4,heights=c(1,1,1,1), widths=c(1.1,0.933,0.933,0.933))

ggsave(filename = 'points_all.png',plot = grid_all,width = 17,height = 10,dpi = 600)



#maps_greyed----



list_amphibians_cells = unique(amphibians_grid$layer)
list_reptiles_cells = unique(reptiles_grid$layer)
list_mammals_cells = unique(mammals_grid$layer)
list_birds_cells = unique(birds_breeding_grid$layer)

greyed_cells = unique(c(list_amphibians_cells,list_reptiles_cells,list_mammals_cells,list_birds_cells))
save(greyed_cells,file = 'greyed_cells.Rdata')

####old----

library(RColorBrewer)
pal_reds = colorRampPalette(RColorBrewer::brewer.pal(9,'Reds'))
pal_brown = colorRampPalette(RColorBrewer::brewer.pal(9,'YlOrBr'))

theme_points_disasters = theme(panel.background = element_blank(),
                               axis.ticks = element_blank(),
                               legend.position = 'bottom',
                               legend.key.height = unit(0.25,'cm'),
                               axis.line = element_line(),
                               legend.key.width = unit(1,'cm'),
                               legend.title = element_text(size = 8))


p1 = ggplot()+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black',alpha = 0.75)+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))
guides(fill = guide_colorbar(title.position = 'top'))+
  theme_points_disasters



p1 = ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black',alpha = 0.75)+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_reds(max(amphibians_grid2_vul_0$tsunamis_freq)))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  theme_points_disasters+
  theme(legend.position = 'none')
#scale_y_continuous(limits = c(0,1.1))+
#scale_x_continuous(limits = c(0,102))
# guides(fill = guide_colorbar(title.position = 'top'))+
#   theme_points_disasters


p2 = ggplot()+
  geom_point(data = reptiles_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black',alpha = 0.75)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  #scale_y_continuous(limits = c(0,1.1))+
  #scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  theme_points_disasters+
  labs(title = 'Reptiles (tsunamis)')+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  theme(legend.position = 'none')

p3 = ggplot()+
  geom_point(data = mammals_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black',alpha = 0.75)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  #scale_y_continuous(limits = c(0,1.1))+
  #scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  labs(title = 'Mammals (tsunamis)')+
  theme_points_disasters+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  theme(legend.position = 'none')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = log10(range_size),fill = tsunamis_freq),
             shape = 21,col = 'black',alpha = 0.75)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="red", alpha=0.2)+
  #scale_y_continuous(limits = c(0,1.1))+
  #scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('Impact')+
  labs(title = 'Birds (tsunamis)')+
  theme_points_disasters+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  theme(legend.position = 'none')

library(gridExtra)

grid_tsunamis = arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_vucanos_NOV3.png',plot = grid_tsunamis,width = 10,height = 8,dpi = 600)

###tsunamis

amphibians_grid2_tsu_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
reptiles_grid2_tsu_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
mammals_grid2_tsu_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))
birds_breeding_grid2_tsu_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(tsunamis_prob_b))



p1 = ggplot()+
  geom_point(data = amphibians_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Amphibians (tsunamis)')


p2 = ggplot()+
  geom_point(data = reptiles_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Reptiles (tsunamis)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Mammals (tsunamis)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_tsu_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="blue", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  labs(title = 'Birds (tsunamis)')

library(gridExtra)

grid_tsunamis= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_tsunamis.png',plot = grid_tsunamis,width = 6,height = 6,dpi = 600)

###hurricanes


amphibians_grid2_hur_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
reptiles_grid2_hur_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
mammals_grid2_hur_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))
birds_breeding_grid2_hur_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & hurricanes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(hurricanes_prob_b))

library(openxlsx)



p1 = ggplot()+
  geom_point(data = amphibians_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Amphibians (hurricanes)')


p2 = ggplot()+
  geom_point(data = reptiles_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Reptiles (hurricanes)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Mammals (hurricanes)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_hur_0,aes(x = total_perc_afected, y = avg_impact),size = 0.5)+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())+
  geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill="grey80", alpha=0.2)+
  scale_y_continuous(limits = c(0,1.1))+
  scale_x_continuous(limits = c(0,102))+
  xlab('Range affected (%)')+
  #ylab('AVG impact probability')+
  labs(title = 'Birds (hurricanes)')

library(gridExtra)

grid_hurricanes= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_hurricanes.png',plot = grid_hurricanes,width = 6,height = 6,dpi = 600)

###earthquake


library(openxlsx)
write.xlsx(amphibians_grid2_vul_0,file = 'amphibians_grid2_vul_0.xlsx')
write.xlsx(reptiles_grid2_vul_0,file = 'reptiles_grid2_vul_0.xlsx')
write.xlsx(mammals_grid2_vul_0,file = 'mammals_grid2_vul_0.xlsx')
write.xlsx(birds_breeding_grid2_vul_0,file = 'birds_grid2_vul_0.xlsx')

write.xlsx(amphibians_grid2_ear_0,file = 'amphibians_grid2_ear_0.xlsx')
write.xlsx(reptiles_grid2_ear_0,file = 'reptiles_grid2_ear_0.xlsx')
write.xlsx(mammals_grid2_ear_0,file = 'mammals_grid2_ear_0.xlsx')
write.xlsx(birds_breeding_grid2_ear_0,file = 'birds_grid2_ear_0.xlsx')

write.xlsx(amphibians_grid2_tsu_0,file = 'amphibians_grid2_tsu_0.xlsx')
write.xlsx(reptiles_grid2_tsu_0,file = 'reptiles_grid2_tsu_0.xlsx')
write.xlsx(mammals_grid2_tsu_0,file = 'mammals_grid2_tsu_0.xlsx')
write.xlsx(birds_breeding_grid2_tsu_0,file = 'birds_grid2_tsu_0.xlsx')

write.xlsx(amphibians_grid2_hur_0,file = 'amphibians_grid2_hur_0.xlsx')
write.xlsx(reptiles_grid2_hur_0,file = 'reptiles_grid2_hur_0.xlsx')
write.xlsx(mammals_grid2_hur_0,file = 'mammals_grid2_hur_0.xlsx')
write.xlsx(birds_breeding_grid2_hur_0,file = 'birds_grid2_hur_0.xlsx')



ggplot()+
  #geom_rect(aes(xmin=25, xmax=101, ymin=0.25, ymax=1.1), fill='grey20', alpha=0.2,col = NA)+
  labs(title = 'Amphibians (tsunamis)')+
  geom_point(data = amphibians_grid2_vul_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_reds(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  theme(legend.position = 'none')
#scale_y_continuous(limits = c(0,1.1))+
#scale_x_continuous(limits = c(0,102))
# guides(fill = guide_colorbar(title.position = 'top'))+
#   theme_points_disasters


#amphibians_grid2_ear_0$avg_impact_normalized = 100*amphibians_grid2_ear_0$avg_impact/max(amphibians_grid2_ear_0$avg_impact)

amphibians_grid2$weighted_earthquakes_prob_b = (amphibians_grid2$earthquakes_prob_b*amphibians_grid2$range_size_intersect)/amphibians_grid2$range_size
reptiles_grid2$weighted_earthquakes_prob_b = (reptiles_grid2$earthquakes_prob_b*reptiles_grid2$range_size_intersect)/reptiles_grid2$range_size
mammals_grid2$weighted_earthquakes_prob_b = (mammals_grid2$earthquakes_prob_b*mammals_grid2$range_size_intersect)/mammals_grid2$range_size
birds_breeding_grid2$weighted_earthquakes_prob_b = (birds_breeding_grid2$earthquakes_prob_b*birds_breeding_grid2$range_size_intersect)/birds_breeding_grid2$range_size


amphibians_grid2_ear_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
reptiles_grid2_ear_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
mammals_grid2_ear_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))
birds_breeding_grid2_ear_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_earthquakes_prob_b))


amphibians_grid2_ear_0 = amphibians_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(weighted_earthquakes_prob_b),avg_impact =  sum(weighted_earthquakes_prob_b),tsunamis_freq = sum(tsunamis_freq))
reptiles_grid2_ear_0 = reptiles_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(weighted_earthquakes_prob_b),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))
mammals_grid2_ear_0 = mammals_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(weighted_earthquakes_prob_b),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))
birds_breeding_grid2_ear_0 = birds_breeding_grid2 %>% group_by(binomial) %>% filter(percent_range>0 & earthquakes_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  mean(weighted_earthquakes_prob_b),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))


amphibians_grid2_vul_0 = amphibians_grid2 %>% group_by(binomial,range_size) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))
reptiles_grid2_vul_0 = reptiles_grid2 %>% group_by(binomial,range_size) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact = sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))
mammals_grid2_vul_0 = mammals_grid2 %>% group_by(binomial,range_size) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))
birds_breeding_grid2_vul_0 = birds_breeding_grid2 %>% group_by(binomial,range_size) %>% filter(percent_range>0 & tsunamis_prob_b > 0) %>% summarise(total_perc_afected =  sum(percent_range),avg_impact =  sum(weighted_tsunamis_prob_b),tsunamis_freq = sum(tsunamis_freq))

amphibians_grid2_vul_0$avg_impact = amphibians_grid2_vul_0$avg_impact/max(amphibians_grid2_vul_0$avg_impact)
reptiles_grid2_vul_0$avg_impact = reptiles_grid2_vul_0$avg_impact/max(reptiles_grid2_vul_0$avg_impact)
mammals_grid2_vul_0$avg_impact = mammals_grid2_vul_0$avg_impact/max(mammals_grid2_vul_0$avg_impact)
birds_breeding_grid2_vul_0$avg_impact = birds_breeding_grid2_vul_0$avg_impact/max(birds_breeding_grid2_vul_0$avg_impact)

p1 = ggplot()+
  geom_point(data = amphibians_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_brown(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  theme(legend.position = 'none')+
  labs(title = 'Amphibians (earthquakes)')
#geom_rect(aes(xmin=25, xmax=101, ymin=25, ymax=101), fill='grey20', alpha=0.2,col = NA)



p2 = ggplot()+
  geom_point(data = reptiles_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_brown(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  theme(legend.position = 'none')+
  labs(title = 'Reptiles (earthquakes)')

p3 = ggplot()+
  geom_point(data = mammals_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_brown(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  theme(legend.position = 'none')+
  labs(title = 'Mammals (earthquakes)')

p4 = ggplot()+
  geom_point(data = birds_breeding_grid2_ear_0,aes(x = total_perc_afected, y = avg_impact,size = total_perc_afected,fill = avg_impact),
             shape = 21,col = 'black')+
  scale_size_continuous(guide = "none")+
  scale_fill_gradientn('Average Impact',colors = pal_brown(10))+
  xlab('Range affected (%)')+
  ylab('AVG impact probability')+
  theme_points_disasters+
  theme(legend.position = 'none')+
  labs(title = 'Birds (earthquakes)')

library(gridExtra)

grid_earthquakes= arrangeGrob(p1,p2,p3,p4,ncol = 2)

ggsave(filename = 'points_earthquakes_11_2_mean.png',plot = grid_earthquakes,width = 6,height = 6,dpi = 600)


