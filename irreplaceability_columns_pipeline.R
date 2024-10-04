#irreplaceability layers

load('irrep_s1_amphibians.Rdata')
r_grid$ferrier_a = NA
r_grid[r_grid$land==1,]$ferrier_a = irrep_s1$plot_total

load('irrep_s1_reptiles.Rdata')
r_grid$ferrier_r = NA
r_grid[r_grid$land==1,]$ferrier_r = irrep_s1$plot_total

load('irrep_s1_mammals.Rdata')
r_grid$ferrier_m = NA
r_grid[r_grid$land==1,]$ferrier_m = irrep_s1$plot_total

load('irrep_s1_birds.Rdata')
r_grid$ferrier_b = NA
r_grid[r_grid$land==1,]$ferrier_b = irrep_s1$plot_total




save(r_grid,file = 'r_grid_metrics_run2.Rdata')