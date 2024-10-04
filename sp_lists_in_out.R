#sp_list_outside_hotspots
load('/Users/gdt366/Dropbox/disaster_project/amphibians_sf_summ_all_sps_present.Rdata')
amphibians_sf_summ = amphibians_sf_summ[amphibians_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(amphibians_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(amphibians_sf_summ)

sp_list = st_intersects(sf,r_grid[r_grid$wege2_a_d %in% 10,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_wege2_a_d.csv')
write.csv(sp_list_out,file = 'sp_list_out_wege2_a_d.csv')


sp_list = st_intersects(sf,r_grid[r_grid$hotspots_all %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_a_hotspots_all.csv')
write.csv(sp_list_out,file = 'sp_list_out_a_hotspots_all.csv')

sp_list = st_intersects(sf,r_grid[r_grid$hotspot_any_threat_a %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_a_hotspot_any_threat.csv')
write.csv(sp_list_out,file = 'sp_list_out_a_hotspot_any_threat.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_a_hot) & r_grid$ferrier_a_hot>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_a_hotspot_ferrier_a_hot.csv')
write.csv(sp_list_out,file = 'sp_list_out_a_hotspot_ferrier_a_hot.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_a) & r_grid$ferrier_a>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_a_hotspot_ferrier_a.csv')
write.csv(sp_list_out,file = 'sp_list_out_a_hotspot_ferrier_a.csv')


#reptiles

load('/Users/gdt366/Dropbox/postdoc_KU_paper_2/reptiles_sf_summ_all_sps_present_only_terrestrial.Rdata')
reptiles_sf_summ = reptiles_sf_summ[reptiles_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(reptiles_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(reptiles_sf_summ)

sp_list = st_intersects(sf,r_grid[r_grid$wege2_r_d %in% 10,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_wege2_r_d.csv')
write.csv(sp_list_out,file = 'sp_list_out_wege2_r_d.csv')

sp_list = st_intersects(sf,r_grid[r_grid$hotspots_all %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_r_hotspots_all.csv')
write.csv(sp_list_out,file = 'sp_list_out_r_hotspots_all.csv')

sp_list = st_intersects(sf,r_grid[r_grid$hotspot_any_threat_r %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_r_hotspot_any_threat.csv')
write.csv(sp_list_out,file = 'sp_list_out_r_hotspot_any_threat.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_r_hot) & r_grid$ferrier_r_hot>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_r_hotspot_ferrier_r_hot.csv')
write.csv(sp_list_out,file = 'sp_list_out_r_hotspot_ferrier_r_hot.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_r) & r_grid$ferrier_r>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_r_hotspot_ferrier_r.csv')
write.csv(sp_list_out,file = 'sp_list_out_r_hotspot_ferrier_r.csv')

#mammals
load('/Users/gdt366/Dropbox/disaster_project/mammals_sf_summ_all_sps_present.Rdata')
mammals_sf_summ = mammals_sf_summ[mammals_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
sf = st_transform(mammals_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')

sp_list = st_intersects(sf,r_grid[r_grid$wege2_m_d %in% 10,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_wege2_m_d.csv')
write.csv(sp_list_out,file = 'sp_list_out_wege2_m_d.csv')


sp_list = st_intersects(sf,r_grid[r_grid$hotspots_all %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_m_hotspots_all.csv')
write.csv(sp_list_out,file = 'sp_list_out_m_hotspots_all.csv')

sp_list = st_intersects(sf,r_grid[r_grid$hotspot_any_threat_m %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_m_hotspot_any_threat.csv')
write.csv(sp_list_out,file = 'sp_list_out_m_hotspot_any_threat.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_m_hot) & r_grid$ferrier_m_hot>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_m_hotspot_ferrier_m_hot.csv')
write.csv(sp_list_out,file = 'sp_list_out_m_hotspot_ferrier_m_hot.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_m) & r_grid$ferrier_m>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_m_hotspot_ferrier_m.csv')
write.csv(sp_list_out,file = 'sp_list_out_m_hotspot_ferrier_m.csv')

#birds

load('/Users/gdt366/Dropbox/disaster_project/birds_sf_summ_breeding_resident.Rdata')
birds_sf_summ = birds_sf_summ[birds_sf_summ$category %in% c('DD','LC','NT','VU','EN','CR'),]
birds_sf_summ = st_crop(birds_sf_summ,extent(-179.9,179.9,-89.9,89.9))
sf = st_transform(birds_sf_summ,'+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs')
rm(birds_sf_summ)

sp_list = st_intersects(sf,r_grid[r_grid$hotspots_all %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_b_hotspots_all.csv')
write.csv(sp_list_out,file = 'sp_list_out_b_hotspots_all.csv')

sp_list = st_intersects(sf,r_grid[r_grid$hotspot_any_threat_b %in% 1,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_b_hotspot_any_threat.csv')
write.csv(sp_list_out,file = 'sp_list_out_b_hotspot_any_threat.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_b_hot) & r_grid$ferrier_b_hot>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_b_hotspot_ferrier_b_hot.csv')
write.csv(sp_list_out,file = 'sp_list_out_b_hotspot_ferrier_b_hot.csv')

sp_list = st_intersects(sf,r_grid[!is.na(r_grid$ferrier_b) & r_grid$ferrier_b>0,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_b_hotspot_ferrier_b.csv')
write.csv(sp_list_out,file = 'sp_list_out_b_hotspot_ferrier_b.csv')

sp_list = st_intersects(sf,r_grid[r_grid$wege2_b_d %in% 10,])

sp_list_in = sf[unique(unlist(t(sp_list))),]
st_geometry(sp_list_in) = NULL
sp_list_out = sf[!sf$binomial %in% sp_list_in$binomial,]
st_geometry(sp_list_out) = NULL
write.csv(sp_list_in,file = 'sp_list_in_wege2_b_d.csv')
write.csv(sp_list_out,file = 'sp_list_out_wege2_b_d.csv')








