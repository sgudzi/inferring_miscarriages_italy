# Open and process denominator



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




#readRDS(paste0(dir.data, "/pop_female_08_22.RDS"))  %>% 
#  filter(eta >= 16 &
#           eta<46) %>% 
#  mutate(age_group = 
#           case_when(eta<25 ~ "<25",
#                     eta>=25 &  eta< 35 ~ "25-35",
#                     eta>=35 ~ "35+"
#                     )) %>% 
#  group_by(comres, ANNO, age_group) %>% 
#  mutate(comres = str_pad(comres, width = 6, pad = "0")) %>% # add the two leading 0s to the codice coumnale
#  summarise(pop = sum(pop), .groups = "drop") -> population
#




# Open and process denominator ------------------------------------------

# 
# readRDS(paste0(dir.data, "/pop_female_08_22.rds")) %>%
#   filter(eta >= 16 & eta < 46) %>%
#   mutate(
#     age_group = case_when(
#       eta < 25 ~ "pop<25",
#       eta >= 25 & eta < 35 ~ "pop25-35",
#       eta >= 35 ~ "pop35+"
#     )#,
#     #comres = str_pad(comres, width = 6, pad = "0")
#   ) %>%
#   group_by(ANNO, age_group, comres) %>%
#   summarise(pop = sum(pop), .groups = "drop") %>%
#   pivot_wider(
#     names_from = age_group,
#     values_from = pop,
#     values_fill = 0
#   ) %>%
#   mutate(total_pop = `pop<25` + `pop25-35` + `pop35+`) -> population




