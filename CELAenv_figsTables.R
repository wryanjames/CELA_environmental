#' """ CELA environmental data figures and tables
#'     @author: Ryan James and Josh Linenfelser
#'     date: 4/8/2021"""

library(tidyverse)
library(ggpubr)

# Chla by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(chla) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(chla),
            sd = sd(chla))

# # plot
# ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
#   geom_pointrange(aes( ymin=mean-sd,ymax=mean+sd),
#                   width=.2, size=0.75)+
#   geom_line(size = 1.5)+
#   theme_bw()+
#   scale_color_manual(values = c('ACS' = 'darkolivegreen3',
#                                'MCS' = 'deepskyblue3'),
#                     labels = c('ACS' = 'Alligator Creek',
#                                'MCS' = 'McCormick Creek')) +
#   labs(x = NULL, y = expression(paste("Chlorophyll a (",mu,"g/L)")))+
#   scale_x_continuous(breaks = 1:4, 
#                      labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
#   coord_flip()+
#   theme(legend.position = 'bottom',
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title = element_text(size = 18), 
#         axis.text = element_text(size = 16, colour = "gray0"), 
#         plot.title = element_text(size = 18, hjust=0.5),
#         legend.title = element_blank(),
#         strip.text.x = element_text(size = 18),
#         legend.text = element_text(size = 16))
# ggsave("figs/Ov_sys.tiff", units="in", width=6, height=4.5, dpi=600,compression = 'lzw')

ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                  width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = expression(paste("Chlorophyll a (",mu,"g/L)")))+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))
ggsave("figs/chla_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# salinity by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Sal.) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Sal.),
            sd = sd(Sal.)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Salinity) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Salinity),
            sd = sd(Salinity)) %>% drop_na()%>% 
  mutate(source = 'CELA')

df = bind_rows(df,dc)
  

ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = 'Salinity (ppt)')+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  facet_wrap(~source)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/sal_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# temp by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Temp.) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Temp.),
            sd = sd(Temp.)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Temperature) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Temperature),
            sd = sd(Temperature)) %>% drop_na()%>% 
  mutate(source = 'CELA')

df = bind_rows(df,dc)


ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "Temperature (°C)")+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  facet_wrap(~source)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/temp_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# Depth by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(`Depth (cm)`) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(`Depth (cm)`),
            sd = sd(`Depth (cm)`)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Depth) %>% 
  group_by(Position, Season, pos, System) %>%
  mutate(Depth = Depth * 100) %>%
  summarize(mean = mean(Depth),
            sd = sd(Depth)) %>% drop_na()%>% 
  mutate(source = 'CELA') 

df = bind_rows(df,dc)


ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "Depth (cm)")+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  facet_wrap(~source)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/Depth_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')



# Secchi by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(`Depth (cm)`) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(`Depth (cm)`),
            sd = sd(`Depth (cm)`)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Secchi.Disk) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Secchi.Disk),
            sd = sd(Secchi.Disk)) %>% drop_na()%>% 
  mutate(source = 'CELA') 


ggplot(dc, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "Secchi Depth (m)")+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/Secchi.Depth_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')





#SAV by zone 2016-2109 ----

sum(df, acetabularia::Udotea)


  df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Temp.) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Temp.),
            sd = sd(Temp.)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(Temperature) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(Temperature),
            sd = sd(Temperature)) %>% drop_na()%>% 
  mutate(source = 'CELA')

df = bind_rows(df,dc)


ggplot(df, aes(pos, mean, color = System, linetype = Season)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "Temperature (°C)")+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  facet_wrap(~source)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/temp_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')
