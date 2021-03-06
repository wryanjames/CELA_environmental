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





# SAV by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  pivot_longer(cols = Acetabularia:Udotea, names_to = 'sp', values_to = 'cover') %>%
  group_by(Site, Date, Position, Season, pos, System, Year)%>%
  summarize(total = sum(cover, na.rm = T)) %>%
  group_by(Position, Season, pos, System, Year) %>%
  summarize(mean = mean(total),
            sd = sd(total)) %>% drop_na() %>%
  mutate(source = 'Tom')


b = read_csv('data/TomSites.csv')
m = unique(b[ c('Lake', 'Position','pos', 'System')])

dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(SAV) %>% 
  group_by(Position, Season, pos, System, Year) %>%
  summarize(mean = mean(SAV),
            sd = sd(SAV)) %>% drop_na()%>% 
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
  labs(x = NULL, y = "SAV Percent Cover")+
  scale_x_continuous(breaks = 1:4, 
                     labels = c('Bay', 'Downstream', 'Middle', 'Upstream'))+
  coord_flip()+
  facet_wrap(Year~source, nrow = 2)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/sav_zone16-19.tiff", units="in", width=12, height=8, dpi=600,compression = 'lzw')

# DO by zone 2016-2019 ----
dc = read_csv('data/CELA_envTow.csv') %>% 
  select(-System)%>% left_join(m, by = c('Lake'))%>%
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(DO) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(DO),
            sd = sd(DO)) %>% drop_na()%>% 
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
  labs(x = NULL, y = "Disolved oxygen %")+
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

ggsave("figs/do_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# SAV all years ----
df = read_csv('data/tomDataAll.csv') %>% 
  #filter(between(Year, 2016, 2019)) %>% 
  pivot_longer(cols = Acetabularia:Udotea, names_to = 'sp', values_to = 'cover') %>%
  drop_na(cover)%>%
  group_by(Site, Date, Position, Season, pos, System, Year, Lake)%>%
  summarize(total = sum(cover, na.rm = T)) %>%
  group_by(Position, Season, pos, System, Date) %>%
  summarize(total = mean(total), n = n(), 
            lake = paste(unique(Lake), collapse = ", "))%>%
  drop_na(total,Position)
  # summarize(mean = mean(total),
  #           sd = sd(total)) %>% drop_na() %>%
  # mutate(source = 'Tom')
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))


ggplot(df, aes(Date, total, color = System)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "SAV Percent Cover")+
  facet_wrap(~Position, ncol = 1)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/sav_zoneAll.tiff", units="in", width=8, height=12, dpi=600,compression = 'lzw')

# Salinity all years ----
df = read_csv('data/tomDataAll.csv') %>% 
  drop_na(Sal., Position) 
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))


ggplot(df, aes(Date, Sal., color = System)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = 'Salinity (ppt)')+
  facet_wrap(~Position, ncol = 1)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/sal_zoneAll.tiff", units="in", width=8, height=12, dpi=600,compression = 'lzw')

# chl a all years ----
df = read_csv('data/tomDataAll.csv') %>% 
  drop_na(chla, Position) %>%
  group_by(Position, Season, pos, System, Date) %>%
  summarize(chla = mean(chla))
            
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))


ggplot(df, aes(Date, chla, color = System)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y =  expression(paste("Chlorophyll a (",mu,"g/L)")))+
  facet_wrap(~Position, ncol = 1)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/chla_zoneAll.tiff", units="in", width=8, height=12, dpi=600,compression = 'lzw')

# SAV all years ----
df = read_csv('data/tomDataAll.csv') %>% 
  #filter(between(Year, 2016, 2019)) %>% 
  group_by(Position, Season, pos, System, Date) %>%
  drop_na(Chara) %>%
  summarize(Chara = mean(Chara, na.rm = T),
            Halodule = mean(Halodule, na.rm = T),
            Batophora = mean(Batophora, na.rm = T),
            n = n(), 
            lake = paste(unique(Lake), collapse = ", "))%>%
  drop_na(Position) %>%
  pivot_longer(cols = Chara:Batophora, names_to = 'Species', values_to = 'cover')
# summarize(mean = mean(total),
#           sd = sd(total)) %>% drop_na() %>%
# mutate(source = 'Tom')
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))


ggplot(df, aes(Date, cover, color = System)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "SAV Percent Cover")+
  facet_wrap(Position~Species, ncol = 3)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/savSP_zoneAll.tiff", units="in", width=11, height=12, dpi=600,compression = 'lzw')

# SAV all years ----
df = read_csv('data/tomDataAll.csv') %>% arrange(Date) %>% dr
  #filter(between(Year, 2016, 2019)) %>% 
  group_by(Position, Season, pos, System, Date) %>%
  drop_na(Chara) %>%
  summarize(Chara = mean(Chara, na.rm = T),
            Halodule = mean(Halodule, na.rm = T),
            Batophora = mean(Batophora, na.rm = T),
            n = n(), 
            lake = paste(unique(Lake), collapse = ", "))%>%
  drop_na(Position) %>%
  pivot_longer(cols = Chara:Batophora, names_to = 'Species', values_to = 'cover')
# summarize(mean = mean(total),
#           sd = sd(total)) %>% drop_na() %>%
# mutate(source = 'Tom')
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))

df = df %>% filter(!(Species == 'Halodule'))

ggplot(df, aes(Date, cover, color = System, linetype = Species, shape = Species)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "SAV Percent Cover")+
  facet_wrap(~Position, ncol = 1)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/savBatCHar_zoneAll.tiff", units="in", width=8, height=12, dpi=600,compression = 'lzw')

# SAV all years ----
df = read_csv('data/tomDataAll.csv') %>% 
#filter(between(Year, 2016, 2019)) %>% 
group_by(Position, Season, pos, System, Date) %>%
  drop_na(Chara) %>%
  summarize(Chara = mean(Chara, na.rm = T),
            Halodule = mean(Halodule, na.rm = T),
            Batophora = mean(Batophora, na.rm = T),
            n = n(), 
            lake = paste(unique(Lake), collapse = ", "))%>%
  drop_na(Position) %>%
  pivot_longer(cols = Chara:Batophora, names_to = 'Species', values_to = 'cover')
# summarize(mean = mean(total),
#           sd = sd(total)) %>% drop_na() %>%
# mutate(source = 'Tom')
df$Position = factor(df$Position, levels= c('Upstream', 'Middle', 'Downstream', 'Bay'))



ggplot(df, aes(Date, cover, color = System, linetype = Species, shape = Species)) + 
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = "SAV Percent Cover")+
  facet_wrap(~Position, ncol = 1)+
  theme(legend.position = 'bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16, colour = "gray0"), 
        plot.title = element_text(size = 18, hjust=0.5),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 18),
        legend.text = element_text(size = 11))


ggsave("figs/savStack_zoneAll.tiff", units="in", width=8, height=12, dpi=600,compression = 'lzw')


# Figs for proposal Josh----
# Chla by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(chla) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(chla),
            sd = sd(chla))

# plot
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

# Total N by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(TOTN) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(TOTN),
            sd = sd(TOTN))

# plot
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
  labs(x = NULL, y = expression(paste("Total N (",mu,"M)")))+
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
ggsave("figs/TotN_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# Total P by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(TOTP) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(TOTP),
            sd = sd(TOTP))

# plot
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
  labs(x = NULL, y = expression(paste("Total P (",mu,"M)")))+
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
ggsave("figs/TotP_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# N:P by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(NP) %>% 
  group_by(Position, Season, pos, System) %>%
  summarize(mean = mean(NP),
            sd = sd(NP))

# plot
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
  labs(x = NULL, y = expression(paste("N:P")))+
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
ggsave("figs/NP_zone16-19.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# Figs for proposal Josh No season----
# Chla by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(chla) %>% 
  group_by(Position, pos, System) %>%
  summarize(mean = mean(chla),
            sd = sd(chla))

# plot
ggplot(df, aes(pos, mean, color = System)) + 
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
ggsave("figs/chla_zone16-19noSeason.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# Total N by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(TOTN) %>% 
  group_by(Position, pos, System) %>%
  summarize(mean = mean(TOTN),
            sd = sd(TOTN))

# plot
ggplot(df, aes(pos, mean, color = System)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = expression(paste("Total N (",mu,"M)")))+
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
ggsave("figs/TotN_zone16-19noSeason.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# Total P by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(TOTP) %>% 
  group_by(Position, pos, System) %>%
  summarize(mean = mean(TOTP),
            sd = sd(TOTP))

# plot
ggplot(df, aes(pos, mean, color = System)) + 
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  geom_point(size = 4)+
  geom_line(size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = expression(paste("Total P (",mu,"M)")))+
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
ggsave("figs/TotP_zone16-19noSeason.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')

# N:P by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(NP) %>% 
  group_by(Position, pos, System) %>%
  summarize(mean = mean(NP),
            sd = sd(NP))

# plot
ggplot(df, aes(pos, mean, color = System)) + 
  geom_line(size = 1.5)+
  geom_point(size = 4)+
  geom_errorbar(aes( ymin=mean-sd,ymax=mean+sd),
                width=.1, size = 1.5)+
  theme_bw()+
  scale_color_manual(values = c('ACS' = 'darkolivegreen3',
                                'MCS' = 'deepskyblue3'),
                     labels = c('ACS' = 'Alligator Creek',
                                'MCS' = 'McCormick Creek')) +
  labs(x = NULL, y = expression(paste("N:P")))+
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
ggsave("figs/NP_zone16-19noSeason.tiff", units="in", width=6, height=8, dpi=600,compression = 'lzw')
