#' """ CELA environmental data maps
#'     @author: Ryan James
#'     date: 4/16/2021"""

library(tidyverse)
library(sf)
library(RColorBrewer)
library(colorRamps)
library(viridis)

df = read_csv('data/CELA_envTow.csv') %>% 
  mutate(Lake = if_else(Lake == 'Seven Palm', 'Seven Palm Lake', Lake)) %>% 
  group_by(Lake, Season) %>%
  summarise_if(is.numeric, mean)

lake = st_read('gis/Lakes2.shp') %>% full_join(df, by = c('Lake')) %>%
  rename(System = Estuary)





fl = st_read('gis/Gulf_coast_states.shp')
banks = st_read('gis/Shallow_Banks_in_Florida_Bay_and_Florida_Keys.shp')

# DO----
# season
ggplot() + 
  #geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = fl, color = NA, fill = 'grey60')+
  geom_sf(data = lake, aes( color = System, fill = DO), size = 1) + 
  coord_sf(xlim = c(-80.85,-80.715), ylim = c(25.14, 25.22))+
  scale_fill_gradientn(colors =rev(matlab.like2(10)))+
  scale_color_manual(values = c('Alligator Subestuary' = 'darkolivegreen3',
                                'McCormick Subestuary' = 'deepskyblue3'),
                     labels = c('Alligator Subestuary' = 'Alligator',
                                'McCormick Subestuary' = 'McCormick'))+
  scale_x_continuous(breaks=c(-80.84,  -80.76))+
  scale_y_continuous(breaks=c(25.16, 25.20))+
  theme_bw()+
  facet_wrap(~Season, labeller = as_labeller(c('Dry' = 'Dry Season', 'Wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, fill = 'DO %')+
  theme(axis.title = element_text(size = 18), 
      axis.text = element_text(size = 14, colour = "gray0"), 
      plot.title = element_text(size = 18, hjust=0.5),
      legend.title = element_text(size = 14),
      strip.text.x = element_text(size = 18),
      legend.text = element_text(size = 13))

ggsave("figs/DOseason.tiff", units="in", width=9, height=5, 
       dpi=600,compression = 'lzw')

# average
ggplot() + 
  #geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = fl, color = NA, fill = 'grey60')+
  geom_sf(data = lake, aes( color = System, fill = DO), size = 1) + 
  coord_sf(xlim = c(-80.85,-80.715), ylim = c(25.14, 25.22))+
  scale_fill_gradientn(colors =rev(matlab.like2(10)))+
  scale_color_manual(values = c('Alligator Subestuary' = 'darkolivegreen3',
                                'McCormick Subestuary' = 'deepskyblue3'),
                     labels = c('Alligator Subestuary' = 'Alligator',
                                'McCormick Subestuary' = 'McCormick'))+
  scale_x_continuous(breaks=c(-80.84,  -80.76))+
  scale_y_continuous(breaks=c(25.16, 25.20))+
  theme_bw()+
  #facet_wrap(~Season, labeller = as_labeller(c('Dry' = 'Dry Season', 'Wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, fill = 'DO %')+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 13))

ggsave("figs/DOavg.tiff", units="in", width=7, height=5, 
       dpi=600,compression = 'lzw')



# salinty----
# season
ggplot() + 
  #geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = fl, color = NA, fill = 'grey60')+
  geom_sf(data = lake, aes( color = System, fill = Salinity), size = 1) + 
  coord_sf(xlim = c(-80.85,-80.715), ylim = c(25.14, 25.22))+
  scale_fill_viridis(option = 'plasma')+
  scale_color_manual(values = c('Alligator Subestuary' = 'darkolivegreen3',
                                'McCormick Subestuary' = 'deepskyblue3'),
                     labels = c('Alligator Subestuary' = 'Alligator',
                                'McCormick Subestuary' = 'McCormick'))+
  scale_x_continuous(breaks=c(-80.84,  -80.76))+
  scale_y_continuous(breaks=c(25.16, 25.20))+
  theme_bw()+
  facet_wrap(~Season, labeller = as_labeller(c('Dry' = 'Dry Season', 'Wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, fill = 'Salinity (ppt)')+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 13))

ggsave("figs/Salseason.tiff", units="in", width=9, height=5, 
       dpi=600,compression = 'lzw')

# average
ggplot() + 
  #geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = fl, color = NA, fill = 'grey60')+
  geom_sf(data = lake, aes( color = System, fill = Salinity), size = 1) + 
  coord_sf(xlim = c(-80.85,-80.715), ylim = c(25.14, 25.22))+
  scale_fill_viridis(option = 'plasma')+
  scale_color_manual(values = c('Alligator Subestuary' = 'darkolivegreen3',
                                'McCormick Subestuary' = 'deepskyblue3'),
                     labels = c('Alligator Subestuary' = 'Alligator',
                                'McCormick Subestuary' = 'McCormick'))+
  scale_x_continuous(breaks=c(-80.84,  -80.76))+
  scale_y_continuous(breaks=c(25.16, 25.20))+
  theme_bw()+
  #facet_wrap(~Season, labeller = as_labeller(c('Dry' = 'Dry Season', 'Wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, fill = 'Salinity (ppt)')+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 13))

ggsave("figs/Salavg.tiff", units="in", width=7, height=5, 
       dpi=600,compression = 'lzw')



