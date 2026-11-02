# Open and process denominator/offset of female population of reproductive age;




# Interpolate year-to-day  ------------------------------------------------

library(dplyr)
library(tidyr)
library(zoo)

pop_daily <- readRDS("data/pop_female_08_22.rds") %>% 
  select(comres, eta, pop, ANNO) %>%
  
  # get sums of relevant ages
  filter(eta %in% 16:49) %>% 
  group_by(comres, ANNO) %>%
  summarise(pop_16_49 = sum(pop)) %>%
  ungroup() %>% 
  
  # create a date for each yearly observation
  # make sure this is by  monday and therefore week, "comres2" = "comres", "LMP" = "date"  
  mutate(date = as.Date(paste0(ANNO, "-01-01")), 
         dow = lubridate::wday(date)) %>%
  filter(dow==2) %>% # select only mondays
  
  # create full daily sequence per group
  group_by(comres) %>%  
  complete(date = seq(min(date), max(date), by = "day")) %>% 
  
  # linear interpolation
  mutate(
    pop_daily_16_49 = na.approx(total_pop, x = date, na.rm = FALSE)
  ) %>%
  ungroup() %>% 
  
  # make the comres 6 char
  mutate(comres = str_pad(comres, width = 6, pad = "0")) %>% 
  
  # reduce size df
  select(-c(ANNO, pop_16_49))

save(
  pop_daily,
  file = file.path(dir.data, "/weekly_population_f_16_45.RData")
)





