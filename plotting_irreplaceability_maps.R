
library(ggplot2)

df_sf=st_as_sf(irrep_s1[, "plot_total"])
pal = colorRampPalette(c('steelblue','khaki','indianred'))
p_birds = ggplot()+
  geom_sf(data = df_sf,aes(col = log10(plot_total),fill = log10(plot_total)),size = 0)+
  scale_fill_gradientn(colours = pal(100))+
  scale_colour_gradientn(colours = pal(100))

ggsave(filename = 'rodrigues_percent_protection_birds.png',plot = p_birds)

load('irrep_s1_amphibians.Rdata')
df_sf=st_as_sf(irrep_s1[, "plot_total"])
pal = colorRampPalette(c('steelblue','khaki','indianred'))
p_amphibians = ggplot()+
  geom_sf(data = df_sf,aes(col = log10(plot_total),fill = log10(plot_total)),size = 0)+
  scale_fill_gradientn(colours = pal(100))+
  scale_colour_gradientn(colours = pal(100))

ggsave(filename = 'rodrigues_percent_protection_amphibians.png',plot = p_birds)

load('irrep_s1_reptiles.Rdata')
df_sf=st_as_sf(irrep_s1[, "plot_total"])
pal = colorRampPalette(c('steelblue','khaki','indianred'))
p_reptiles = ggplot()+
  geom_sf(data = df_sf,aes(col = log10(plot_total),fill = log10(plot_total)),size = 0)+
  scale_fill_gradientn(colours = pal(100))+
  scale_colour_gradientn(colours = pal(100))

ggsave(filename = 'rodrigues_percent_protection_reptiles.png',plot = p_birds)


load('irrep_s1_mammals.Rdata')
df_sf=st_as_sf(irrep_s1[, "plot_total"])
pal = colorRampPalette(c('steelblue','khaki','indianred'))
p_mammals = ggplot()+
  geom_sf(data = df_sf,aes(col = log10(plot_total),fill = log10(plot_total)),size = 0)+
  scale_fill_gradientn(colours = pal(100))+
  scale_colour_gradientn(colours = pal(100))

ggsave(filename = 'rodrigues_percent_protection_mammals.png',plot = p_birds)


library(gridExtra)

grid1 = arrangeGrob(p_amphibians,p_reptiles,p_mammals,p_birds,ncol=2)

ggsave(filename = 'rodrigues_percent_protection_all.png',plot = grid1,width = 10,height = 6,dpi = 2000)


