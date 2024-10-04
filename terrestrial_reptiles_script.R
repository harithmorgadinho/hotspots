#terrestrial reptiles script

sf_reptiles_1 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/CROCODILES_ALLIGATORS/CROCODILES_ALLIGATORS.shp')
sf_reptiles_2 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/SCALED_REPTILES/SCALED_REPTILES.shp')

to_remove = unique(sf_reptiles_2[sf_reptiles_2$marine == 'true' & sf_reptiles_2$terrestial == 'false' & sf_reptiles_2$freshwater == 'false',]$binomial)
sf_reptiles_2 = sf_reptiles_2[sf_reptiles_2$binomial %in% setdiff(sf_reptiles_2$binomial,to_remove),]

sf_reptiles_3 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/TURTLES/TURTLES.shp')

to_remove = unique(sf_reptiles_3[sf_reptiles_3$marine == 'true' & sf_reptiles_3$freshwater == 'false',]$binomial)

sf_reptiles_3 = sf_reptiles_3[sf_reptiles_3$binomial %in% setdiff(sf_reptiles_3$binomial,to_remove),]

sf_reptiles_4 = st_read('/Users/gdt366/Dropbox/IUCN_shapefiles_2022_1/redlist_species_data_6ef81a4f-8fee-46e3-a74e-f4364b0a87a2/data_0.shp')

colnames(sf_reptiles_4) = tolower(colnames(sf_reptiles_4))
sf_reptiles_4$category = "LC"

to_keep = intersect(colnames(sf_reptiles_4),colnames(sf_reptiles_3))

sf_reptiles_final = rbind(sf_reptiles_1[,to_keep],sf_reptiles_2[,to_keep],sf_reptiles_3[,to_keep],sf_reptiles_4[,to_keep])

rm(sf_reptiles_1,sf_reptiles_2,sf_reptiles_3,sf_reptiles_4)


reptiles_sf_summ = sf_reptiles_final[sf_reptiles_final$presence ==1,] %>% group_by(binomial,category) %>% summarise

reptiles_sf_summ$range_size = st_area(reptiles_sf_summ)/1000000

units(reptiles_sf_summ$range_size) = NULL

save(reptiles_sf_summ,file = 'reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')

load('reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
length(unique(reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]$binomial))

