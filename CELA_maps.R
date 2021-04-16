#' """ CELA environmental data maps
#'     @author: Ryan James
#'     date: 4/16/2021"""

library(tidyverse)
library(sf)

df = read_csv('data/CELA_envTow.csv') %>% 
  mutate(Lake = if_else(Lake == 'Seven Palm', 'Seven Palm Lake', Lake)) %>% 
  group_by(Lake, Season) %>%
  summarise_if(is.numeric, mean)

lake = st_read('gis/Lakes2.shp') %>% full_join(df, by = c('Lake')) %>%
  rename(System = Estuary)

st_write(lake, 'gis/Cela_lakes.shp')



fl = st_read('gis/Gulf_coast_states.shp')
banks = st_read('gis/Shallow_Banks_in_Florida_Bay_and_Florida_Keys.shp')

#all$Season = factor(all$Season,levels=c("wet","dry"))

ggplot() + 
  #geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = fl, color = NA, fill = 'grey50')+
  geom_sf(data = lake, aes( color = System, fill = DO), size = 1) + 
  coord_sf(xlim = c(-80.86,-80.72), ylim = c(25.14, 25.22))+
  scale_fill_brewer()+
  # scale_x_continuous(breaks=c(-80.86, -80.72))+
  # scale_y_continuous(breaks=c(25.22, 25.14))+
  theme_bw()+
  facet_wrap(~Season, labeller = as_labeller(c('Dry' = 'Dry Season', 'Wet'= 'Wet Season')))+
  theme(axis.title = element_text(size = 18), 
      axis.text = element_text(size = 14, colour = "gray0"), 
      plot.title = element_text(size = 18, hjust=0.5),
      legend.title = element_text(size = 14),
      strip.text.x = element_text(size = 18),
      legend.text = element_text(size = 13))





scale_colour_gradient(low = "yellow3",
                        high = "purple4",
                        na.value = "grey50")+
  coord_sf(xlim = c(-80.95,-80.7), ylim = c(24.98, 25.17))+ #crs = st_crs(32617)
  theme_bw()+
  scale_x_continuous(breaks=c(-80.9, -80.8))+
  scale_y_continuous(breaks=c(25, 25.1))+
  facet_wrap(~Season, labeller = as_labeller(c('dry' = 'Dry Season', 'wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, color = 'Brown \nPathway')+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 13))+
  guides(color = guide_colorbar(title.position = "top"))

gr = ggplot() + 
  geom_sf(data = banks, colour = NA, fill = 'grey85') + 
  geom_sf(data = do, color = 'red', fill = NA, size = 1) + 
  geom_point(data = all, mapping = aes(x = lon, y = lat, colour = gr), size = 3.5) + 
  scale_colour_gradient(low = "yellow3",
                        high = "purple4",
                        na.value = "grey50")+
  coord_sf(xlim = c(-80.95,-80.7), ylim = c(24.98, 25.17))+ #crs = st_crs(32617)
  theme_bw()+
  scale_x_continuous(breaks=c(-80.9, -80.8))+
  scale_y_continuous(breaks=c(25, 25.1))+
  facet_wrap(~Season, labeller = as_labeller(c('dry' = 'Dry Season', 'wet'= 'Wet Season')))+
  labs(title = NULL, x = NULL, y = NULL, color = 'Green \nPathway')+
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 14, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 13))+
  guides(color = guide_colorbar(title.position = "top"))
