library(sf)
sf_use_s2(F)
library(raster)
library(fasterize)
colnames(r_grid_final)

library(raster)
library(spdep)
library(spatialreg)

library(raster)

# Create a spatial weights matrix using the rook's case contiguity rule
r_grid2_test = r_grid2[!is.na(r_grid2$hunting_m) & !is.na(r_grid2$hunting_m),]

nb <- poly2nb(r_grid2_test)

listw <- nb2listw(nb, style="W", zero.policy=TRUE)


# Fit a linear regression model
lm_model <- lm(hunting_m ~ hunting_r, data=r_grid2_test)
summary(lm_model)


# Calculate the spatial error model (SEM) accounting for spatial autocorrelation
test1 = moran.test(r_grid2_test$hunting_a, listw, zero.policy=TRUE,
           na.action=na.omit)

set.ZeroPolicyOption(TRUE)

sem_model <- errorsarlm(hunting_m ~ hunting_r, data=r_grid2_test, listw=listw)

# Compare the linear regression model and the spatial error model
summary(lm_model)
summary(sem_model)
