library(sf)
library(dplyr)
sf_use_s2(FALSE)


sf2_1 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_2.shp')
sf2_1 = sf2_1[1:1000,]
sf2_1 = sf2_1 %>% group_by(sci_name) %>% summarise()

save(sf2_1,file = 'sf2_1.Rdata')
rm(sf2_1)

sf2_2 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_2.shp')
sf2_2 = sf2_2[1001:2000,]
sf2_2 = sf2_2 %>% group_by(sci_name) %>% summarise()

save(sf2_2,file = 'sf2_2.Rdata')
rm(sf2_2)

sf2_3 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_2.shp')
sf2_3 = sf2_3[2001:3000,]
sf2_3 = sf2_3 %>% group_by(sci_name) %>% summarise()

save(sf2_3,file = 'sf2_3.Rdata')
rm(sf2_3)

sf2_4 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_2.shp')
sf2_4 = sf2_4[3001:5000,]
sf2_4 = sf2_4 %>% group_by(sci_name) %>% summarise()

save(sf2_4,file = 'sf2_4.Rdata')
rm(sf2_4)

sf3 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_3.shp')
sf3 = sf3 %>% group_by(sci_name) %>% summarise()

save(sf3,file = 'sf3.Rdata')
rm(sf3)

sf4 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_4.shp')
sf4 = sf4 %>% group_by(sci_name) %>% summarise()

save(sf4,file = 'sf4.Rdata')
rm(sf4)

sf5 = st_read('/Users/xfarha/Dropbox/IUCN_shapefiles_2022_1/Birds/batch_5.shp')
sf5 = sf5 %>% group_by(sci_name) %>% summarise()

save(sf5,file = 'sf5.Rdata')
rm(sf5)