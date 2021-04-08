#' """ CELA environmental data figures and tables
#'     @author: Ryan James and Josh Linenfelser
#'     date: 4/8/2021"""

library(tidyverse)

# Chla by zone 2016-2109 ----
df = read_csv('data/tomDataAll.csv') %>% 
  filter(between(Year, 2016, 2019)) %>% 
  drop_na(chla) %>% 
  case_when(Site == '4A' ~
              System = 'MCS',
              Position = 'Upstream',
              Lake = 'Seven Palms',
              pos = 4, pos_rev = 1)
