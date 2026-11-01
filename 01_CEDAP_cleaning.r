# Run set up pgm ----------------------------------------------------------


# source(paste0(getwd(),"/pgms_births/00_env_setup.R"))


# CEDAP Data cleaning -----------------------------------


att_0 <-  haven::read_sas("X:/1_CEDAP/cedap.sas7bdat") %>% 
#  filter(regres == 120) %>% 
  as.data.frame() %>% 
  mutate(
    across(
      c(Conteggio_neonati, eta, ETA_GESTAZ, circ_cranio, Peso, Lunghezza), as.numeric)) %>% 
  mutate(
    DATA_PARTO=as.Date(
      as.numeric(DATA_PARTO), origin = "1970-01-01")) %>% 
  select(progr, eta, ETA_GESTAZ,Conteggio_neonati, DATA_PARTO, mod_parto, circ_cranio, Peso, Lunghezza, sesso, comresd, provres2, vitalita, decorso, tit_stu, cittad, comres2, regres, regresd)

prev_step <-att_0
current_step <- att_0

data.frame(description = "Initial number of births", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_0

# Attrition ---------------------------------------------------------------

# 1. Remove missing values for Date of birth, gestational age, and comune di residenza

att_0 %>% 
  filter(!is.na(DATA_PARTO)) %>% 
  filter(DATA_PARTO>=as.Date("01-01-2010")) -> att_1 #date of birth missing if NA or before 1st Jan 2010

nrow(att_1)

prev_step <-att_0
current_step <- att_1

data.frame(description = "Births 01-01-2010 onwards (known DOB)", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_1


#--------------------------------------------------------------------------------

att_1 %>% 
  filter(!is.na(ETA_GESTAZ)) %>% 
  filter(ETA_GESTAZ>=22 &
           ETA_GESTAZ<=42) -> att_2 #Gestational age missing if NA, less than 22, more than 42 weeks

nrow(att_2)

prev_step <-att_1
current_step <- att_2


data.frame(description = "Gestational Age 22 - 42 weeks", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_2


# 2. Remove missing ages or less than 16, more than 48  -------------------

att_2 %>% 
  filter(!is.na(eta)) %>% 
  filter(eta>=16 &
           eta<=45)-> att_3 # Mothers age missing if its NA, or if it's more than 45 and less than 16

nrow(att_3)

prev_step <-att_2
current_step <- att_3


data.frame(description = "Maternal age Age 16 - 45 years", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_3


# 4. Remove births with missing comune di residenza -------------------         

att_4 <- att_3 %>%
  filter(!is.na(comres2), 
         trimws(comres2) != "",
         comres2 != "ND",
         comres2 != "ER")

prev_step <-att_3
current_step <- att_4


data.frame(description = "Municipality available ", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_4


# 5. Remove non single births

progr_noGemelli <- att_4 %>% 
  group_by(progr) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  .$progr

att_5 <- att_4 %>%
  filter(progr %in% progr_noGemelli)

prev_step <-att_4
current_step <- att_5


data.frame(description = "Single birth", 
           count = nrow(current_step), 
           drop_step_perc = ((nrow(prev_step) - nrow(current_step))/nrow(prev_step))*100,
           drop_total_perc =((nrow(att_0) - nrow(current_step))/nrow(att_0))*100) -> attrition_5


dati_completi <- att_5


# make attrition table

list_attr_objects <- ls(pattern = "^attrition") %>% lapply(get) 
do.call(rbind, list_attr_objects) -> db_att_steps

saveRDS(db_att_steps, 
        file = paste0(dir.output, "/attrition.RDS"))

# remove temporary tables from working memory otherwise laptop dies

rm(list = ls(pattern = "^att"))
rm(current_step, prev_step, progr_noGemelli)
gc()

#-------------------------------------------------------------------------------
# Add variable for LMP week and Settimana parto, both on MONDAYS  --------------


dati_completi %>%
  mutate(row_id = row_number()) %>%
  mutate(Comune = (toupper(comresd))) %>%
  mutate(
    LMP = floor_date(
      as.Date(DATA_PARTO - weeks(ETA_GESTAZ)),  unit = "week") + days(1)) %>% 
  mutate(
    SETTIMANA_PARTO = floor_date(
      as.Date(DATA_PARTO),  unit = "week") +days(1)) %>% 
  mutate(age_group = 
           case_when(eta<25 ~ "birth<25",
                     eta>=25 &  eta< 35 ~ "birth25-35",
                     eta>=35 ~ "birth35+"
           )) %>% 
  #    filter(ETA_GESTAZ>36 & ETA_GESTAZ<44) %>% # keep only full term births 
  filter(SETTIMANA_PARTO>=as.Date("01.01.2010", format="%d.%m.%Y") & 
           SETTIMANA_PARTO<as.Date("31.12.2022", format="%d.%m.%Y")) %>% 
  mutate(anno = year(SETTIMANA_PARTO)) -> outcome_patient_level

#weekdays(outcome_patient_level$LMP)
#weekdays(outcome_patient_level$SETTIMANA_PARTO)

save(outcome_patient_level, 
     file = paste0(dir.data, "/outcome_patient_level.Rdata"))
