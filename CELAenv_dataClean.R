#' """ CELA environmental data cleaning and wrangling
#'     @author: Ryan James and Josh Linenfelser
#'     date: 4/7/2021"""

library(tidyverse)
library(lubridate)

# load all data in ----
# load CELA env data from seine 
cody = read_csv('data/CELA_envTow.csv')

# load tom site data information
site = read_csv('data/TomSites.csv') %>% 
  rename(Site = `Site #`)

# load tom water quality data 
wq = read_csv('data/WQtom.csv') %>% 
  mutate(Site = as.character(Site))

# chlorophyll data
chl = read_csv('data/Chlorophyll.csv') %>%
  select(Site:chla) %>%
  mutate(Site = as.character(Site))

# SAV data
sav = list.files('data/', pattern = 'SAV',full.names = T)

for (i in 1:length(sav)){
  d = read_csv(sav[i], skip = 1, col_types = cols(.default = "c") )
  if (i == 1){
    SAV = d
  } else{
    SAV = bind_rows(SAV, d)
  }
}

# filter only raw reps
SAV = SAV %>% rename(Site = `Site #`) %>%
  filter(Rep %in% as.character(1:15)) %>% 
  mutate_at(names(SAV[4:21]), as.numeric) %>% 
  pivot_longer(Chara:Digenia,names_to = 'Cover_type', values_to = 'SAV_tom')

notDates = c("discontinued", "Discontinued", "Gator" ,"not done", "Not done",    
             "too shallow" , "Too shallow",  "Tooshallow",   NA )

ds = SAV %>% group_by(Date, Site, Cover_type)%>% 
  summarize(mSAV = mean(SAV_tom)) %>% 
  filter(!(Date %in% notDates))

#write_csv(ds, 'data/SAVtom.csv')
ds = ds %>% pivot_wider(names_from = 'Cover_type', values_from = 'mSAV')

# merging tom's data
tom = full_join(wq, chl, by = c('Site', 'Date') )
tom = full_join(tom, ds, by = c('Site', 'Date'))
tom = full_join(tom, site, by = c('Site'))

# add season and year columns from date
tom = tom %>% 
  mutate(Date = mdy(Date),
         Year = year(Date), 
         Season = case_when(
           month(Date) %in% c(2,3,4,5,6,7) ~ 'Dry',
           month(Date) %in% c(8,9,10,11,12,1) ~ 'Wet'
         )
  ) %>% 
  select(Site, Date, Year, Season, everything())

tom = tom %>% arrange(Date)

write_csv(tom, 'data/tomDataAll.csv')
