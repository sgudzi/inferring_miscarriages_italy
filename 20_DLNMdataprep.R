# --------------------------------------------------------------------
# --------------------- DATASET PREPARATION --------------------------
# --------------------------------------------------------------------

# 17.12.2025: ensure it runs for whole italy (failed)
# 18.12.2025: added utci
# 19.12.2025: ensure it runs for whole italy



#source("00_env_setup.R")

min_GA <- 22 # The gestational age from which we include counts 
#weekly_aggregation <- "avg_weekly" # temperature aggregation

# which gestational age do you want the lead to begin with?
# ON LINE 173


# ------------------------ Outcome -----------------------------------


# Distinct combinations of births per week and comune, fill up 0s---
load(paste0(dir.data, "/outcome_patient_level.RData")) %>% get() %>%
  filter(comres2 %in% shapefile_comuni$PRO_COM_T) %>%   
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
  group_by(LMP, comres2) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  complete(
    comres2, LMP, fill = list(n=0) # how many same LMPs  per comune
  ) -> distinct_combinations



# ------------------------ Exposure ----------------------------------

# shp ---

shapefile_comuni$id_space = 1:nrow(shapefile_comuni)


# IDF ---
idf_comuni <- read_excel(paste0(dir.data, "/IDF.xlsx"), 
                         sheet ="Sheet1") %>% 
  filter(TIME_PERIOD == 2021) %>%
  select(DATA_TYPE, Osservazione, REF_AREA, Territorio) %>% 
  pivot_wider(id_cols = c(REF_AREA, Territorio),
              names_from = DATA_TYPE, 
              values_from = Osservazione) %>% 
  mutate(comres = str_pad(REF_AREA, width = 6, pad = "0"))




# Lag UTCI 0 to 22 GW ----- -----------------------------------------------



readRDS(paste0(dir.data, "/weekly_avg_comune_UTCI_2008_2020.RDS")) %>% 
  pivot_longer(
    cols = matches("^20"),        # all columns starting with 20-...
    names_to = "date",
    values_to = "UTCI") %>% 
  mutate(
    date = as.Date(date),
    UTCI = UTCI - 273.15) %>% 
  select(codice_comunale, Name, date, UTCI) %>% 
  filter(codice_comunale %in% shapefile_comuni$PRO_COM_T) %>% # retain only the ones that belong to the shapefile 
  rename(weekly_value = UTCI, 
         Week_date = date) %>%
  #  mutate(year = year(Week_date)) %>% 
  arrange(codice_comunale, Week_date) %>% 
  #  group_by(codice_comunale, year) %>%
  mutate(
    cop_lead1 = lead(weekly_value, 1), 
    cop_lead2 = lead(weekly_value, 2), 
    cop_lead3 = lead(weekly_value, 3), 
    cop_lead4 = lead(weekly_value, 4), 
    cop_lead5 = lead(weekly_value, 5), 
    cop_lead6 = lead(weekly_value, 6), 
    cop_lead7 = lead(weekly_value, 7), 
    cop_lead8 = lead(weekly_value, 8),
    cop_lead9 = lead(weekly_value, 9),
    cop_lead10 = lead(weekly_value, 10), 
    cop_lead11 = lead(weekly_value, 11),
    cop_lead12 = lead(weekly_value, 12),
    cop_lead13 = lead(weekly_value, 13),
    cop_lead14 = lead(weekly_value, 14),
    cop_lead15 = lead(weekly_value, 15),
    cop_lead16 = lead(weekly_value, 16),
    cop_lead17 = lead(weekly_value, 17),
    cop_lead18 = lead(weekly_value, 18),
    cop_lead19 = lead(weekly_value, 19),
    cop_lead20 = lead(weekly_value, 20),
    cop_lead21 = lead(weekly_value, 21),
    cop_lead22 = lead(weekly_value, 22)
  ) %>% 
  rename(cop_lead0 = weekly_value) -> lead_UTCI_0_22



# Lag temperature 0 to 22 GW ----


load(paste0(dir.data, "/temp_max_weekly_italia_2008_2022_new")) %>% get() %>% 
  select(codice_comunale, Name, Week_date, !!sym(weekly_aggregation))  %>%  
  filter(codice_comunale %in% shapefile_comuni$PRO_COM_T) %>% 
  rename(weekly_value = !!sym(weekly_aggregation)) %>%
  #  mutate(year = year(Week_date)) %>% 
  arrange(codice_comunale, Week_date) %>% 
  #  group_by(codice_comunale, year) %>%
  mutate(
    cop_lead1 = lead(weekly_value, 1), 
    cop_lead2 = lead(weekly_value, 2), 
    cop_lead3 = lead(weekly_value, 3), 
    cop_lead4 = lead(weekly_value, 4), 
    cop_lead5 = lead(weekly_value, 5), 
    cop_lead6 = lead(weekly_value, 6), 
    cop_lead7 = lead(weekly_value, 7), 
    cop_lead8 = lead(weekly_value, 8),
    cop_lead9 = lead(weekly_value, 9),
    cop_lead10 = lead(weekly_value, 10), 
    cop_lead11 = lead(weekly_value, 11),
    cop_lead12 = lead(weekly_value, 12),
    cop_lead13 = lead(weekly_value, 13),
    cop_lead14 = lead(weekly_value, 14),
    cop_lead15 = lead(weekly_value, 15),
    cop_lead16 = lead(weekly_value, 16),
    cop_lead17 = lead(weekly_value, 17),
    cop_lead18 = lead(weekly_value, 18),
    cop_lead19 = lead(weekly_value, 19),
    cop_lead20 = lead(weekly_value, 20),
    cop_lead21 = lead(weekly_value, 21),
    cop_lead22 = lead(weekly_value, 22)
  ) %>% 
  rename(cop_lead0 = weekly_value) -> lead_temp_0_22


# Lag temperature -4 to 22 GW ----


load(paste0(dir.data, "/temp_max_weekly_italia_2008_2022_new")) %>% get() %>% #EC update here temp
  select(codice_comunale, Name, Week_date, !!sym(weekly_aggregation))  %>%  
  filter(codice_comunale %in% shapefile_comuni$PRO_COM_T) %>% 
  rename(weekly_value = !!sym(weekly_aggregation)) %>%
  arrange(codice_comunale, Week_date) %>% 
  mutate(
    cop_lead1 = lag(weekly_value, 4), # lead1 is now equivalent to -4
    cop_lead2 = lag(weekly_value, 3), 
    cop_lead3 = lag(weekly_value, 2), 
    cop_lead4 = lag(weekly_value, 1), 
    # this line is missing on purpose: the LMP temperature is "weekly value" which is renamed() to cop_lead5 
    cop_lead6 = lead(weekly_value, 1), 
    cop_lead7  = lead(weekly_value, 2), 
    cop_lead8 = lead(weekly_value, 3), 
    cop_lead9 = lead(weekly_value, 4), 
    cop_lead10= lead(weekly_value, 5), 
    cop_lead11 = lead(weekly_value, 6), 
    cop_lead12 = lead(weekly_value, 7), 
    cop_lead13 = lead(weekly_value, 8),
    cop_lead14 = lead(weekly_value, 9),
    cop_lead15 = lead(weekly_value, 10), 
    cop_lead16 = lead(weekly_value, 11),
    cop_lead17 = lead(weekly_value, 12),
    cop_lead18 = lead(weekly_value, 13),
    cop_lead19 = lead(weekly_value, 14), 
    cop_lead20 = lead(weekly_value, 15),
    cop_lead21 = lead(weekly_value, 16),
    cop_lead22 = lead(weekly_value, 17), 
    cop_lead23 = lead(weekly_value, 18),
    cop_lead24 = lead(weekly_value, 19),
    cop_lead25 = lead(weekly_value, 20),
    cop_lead25 = lead(weekly_value, 21),
    cop_lead25 = lead(weekly_value, 22)
  ) %>% 
  rename(cop_lead5 = weekly_value # cop_lead5 is in this case is week 0 
  ) -> lead_temp_minus4_22

# lead_temp_minus4_12 is now lead_temp_minus4_22 


# Lag PM2.5  --------

readRDS(paste0(dir.data, "/pm2p5_mean_daily_lazio_2013_2019.RDS")) %>%  
  filter(codice_comunale %in% shapefile_comuni$PRO_COM_T) %>%
  #  mutate(year = year(Week_date)) %>% 
  arrange(codice_comunale, date) %>% 
  group_by(codice_comunale, ID, Name, yr, week) %>%
  summarise(
    weekly_value = mean(pm_scaled[,1], na.rm = TRUE), # NB change here the type of pm2p5
    Week_date = floor_date(min(date), unit = "week", week_start = 1),
    .groups = "drop"
  ) %>%
  mutate(
    pm2p5_lead1 = lead(weekly_value, 1), 
    pm2p5_lead2 = lead(weekly_value, 2), 
    pm2p5_lead3 = lead(weekly_value, 3), 
    pm2p5_lead4 = lead(weekly_value, 4), 
    pm2p5_lead5 = lead(weekly_value, 5), 
    pm2p5_lead6 = lead(weekly_value, 6), 
    pm2p5_lead7 = lead(weekly_value, 7), 
    pm2p5_lead8 = lead(weekly_value, 8),
    pm2p5_lead9 = lead(weekly_value, 9),
    pm2p5_lead10 = lead(weekly_value, 10), 
    pm2p5_lead11 = lead(weekly_value, 11),
    pm2p5_lead12 = lead(weekly_value, 12),
    pm2p5_lead13 = lead(weekly_value, 13),
    pm2p5_lead14 = lead(weekly_value, 14),
    pm2p5_lead15 = lead(weekly_value, 15),
    pm2p5_lead16 = lead(weekly_value, 16),
    pm2p5_lead17 = lead(weekly_value, 17),
    pm2p5_lead18 = lead(weekly_value, 18),
    pm2p5_lead19 = lead(weekly_value, 19),
    pm2p5_lead20 = lead(weekly_value, 20),
    pm2p5_lead21 = lead(weekly_value, 21),
    pm2p5_lead22 = lead(weekly_value, 22)
  ) %>% 
  rename(pm2p5_lead0 = weekly_value) -> lead_pm2p5_0_22



# Lag NO2  --------



# ------------------ Join exposure and outcome ---------------------------


#lead_temp <- lead_temp_minus4_22


# CEDAP data is only reliable after 2010: therefore the earliest LMP is in March 2009. We only need exposure data from beginning 2009.

distinct_combinations %>%  
  left_join(lead_UTCI_0_22, by = c("comres2" = "codice_comunale",  "LMP" = "Week_date")) %>% # EC: add temperature data
#  left_join(lead_pm2p5_0_22, by = c("comres2" = "codice_comunale",  "LMP" = "Week_date")) %>% # EC: add pollution data
  left_join(
    load(paste0(dir.data, "/weekly_population_f_16_45.RData" )) %>% get(), 
    by = c("comres2" = "comres", "LMP" = "date" )) %>% 
  left_join(., idf_comuni %>% select(comres, HIGH_EMISS_MOTOR_RATE, POP_DEPEN_INDEX_ADJ, PROTECTED_NAT_AREAS, EMPL_RATE_20_64, POP_25_64_UNDER_DIP_LOW_SEC_EDU, INDEX_ACCES_ESSENT_SERVICES), 
            by = c("comres2" = "comres")) %>% #EC: I uncommented 240 and 241; 
  mutate(total_pop = round(total_pop)) %>% 
  mutate(id_time_of_birth = 
           as.numeric(LMP),
         id_week = isoweek(LMP),
         year_number = year(LMP) - min(year(LMP)) + 1) %>% 
  left_join(shapefile_comuni %>% #NB change when performing on the whole country
              select(PRO_COM_T, id_space, Ecoregioni) %>% 
              st_drop_geometry(), 
            by = c("comres2"="PRO_COM_T")) %>% 
  arrange(id_space, LMP) %>% 
  filter(year(LMP) %in% 2009:2019 # EC: filter if necessary by complete years 
         ) -> outcome_exposure_lead


saveRDS(outcome_exposure_lead, 
        file =paste0(dir.data,"/outcome_exposure_lead.RDS"))


# give this poor machine a break# give this poor mpm2p5_lead21achine a break

rm(lead_temp_minus4_22)
rm(lead_temp_0_22)
rm(lead_UTCI_0_22)
rm(lead_pm2p5_0_22)
rm(distinct_combinations)

gc()


# ----------------- End of code for joining exposure and outcome  -----------------




