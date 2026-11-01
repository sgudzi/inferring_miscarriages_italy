
###############################################################################
# Pre-process exposure and exploration of expsoures 
###############################################################################


cds.key <- wf_set_key()

#  This function is for extracting and processing daily statistics --------

#dir_tif_yr <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr, "/max.tif")

for (yr in 2024) {
  
#  yr<-2024
  product <- "temp_land"
  raw_data_dir <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr, "/raw")
  final_data_dir <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr)
  
  dir.create(raw_data_dir, recursive = TRUE)
  
  for (month_index in 1:12) { #start monthly hourly data download
    
#    month_index<- 12
    request <- list(
      dataset_short_name = "derived-era5-land-daily-statistics", #!!daily statistics
      variable = "2m_temperature",
      year = as.character(yr),
      month = sprintf("%02d", month_index),
      day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"), #it does not matter is it is 31 days in the request but the month only has 29 days, the request will automatically contain the right number of days  
      daily_statistic = "daily_maximum", #the daily statistics
      time_zone = "utc+00:00",
      frequency = "1_hourly",
      area = c(47, 6, 36, 19),
      target = paste(product, yr,  sprintf("%02d", month_index),  sep = "_")
    )
    
    out <- try( # word of caution!! sometimes there is a server error and the dataset is not oulled directly in the raw_data_dir
      wf_request(
        request   = request,
        transfer  = TRUE,
        path      = raw_data_dir,
        time_out  = 3600,
        retry     = 100,   # poll every 3 minutes instead of every 30 s
        verbose   = TRUE
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error")) {
      message("Month ", month_index, " failed. Waiting 10 minutes before retry...")
      Sys.sleep(60)
    }
    
    Sys.sleep(30)   # optional pause between months
    gc()
  } # end max data download 
  
  #-------------------------------------------------------------------------------
  
  # combine all rasters into one yearly raster
  raw_data_files <- list.files(
    raw_data_dir,
    pattern = paste0(yr),
    full.names = TRUE
  )
  
  spatvector <- terra::vect(shapefile_comuni_full) #%>% project("OGC:CRS84")
  

  # Read the  file for the months of the year selected into a *list* of SpatRaster objects
  rasters <- lapply(raw_data_files, rast)
  
  # Combine into one big SpatRaster
  all_days <- do.call(what = c, 
                       args = rasters)
  
  # Crop to retain the relevant geography (i.e. Italy)
  
  # Ensure the projection for both the raster and the shapefile is the same
  if (st_crs(shapefile_comuni) != terra::crs(all_days)) {
    shapefile_comuni <- st_transform(shapefile_comuni, terra::crs(all_days))
  }
  
  # Convert shapefile to a terra spatvector
  spatvector <- terra::vect(shapefile_comuni) #%>% project("OGC:CRS84")
  
  # Cut the raster so its only for italy
  all_hours_crop <- crop(all_days, spatvector)      # Optional: crops to the extent of the shape # OK NOW IT WORKS
  
  daily_max <- all_hours_crop

  # Save
  
  writeRaster(
    daily_max,
    filename = file.path(final_data_dir, "max.tif"),
    overwrite = TRUE
  )
  
  municipality_values <- terra::extract(
    x = daily_max,
    y = spatvector,
    fun = mean,        # mean of the values within each spatial feature
    weights = TRUE,
    na.rm = TRUE
  )
  
  #make a vector of dates for the year at hand 
  start_date <- as.Date(paste0(yr, "-01-01"))
  end_date   <- as.Date(paste0(yr, "-12-31"))
  seq.Date(from = start_date, to = end_date, by = "day") %>% as.character()-> date_vector
  
  df_extracted  <- as.data.frame(municipality_values) # Convert to df
  colnames(df_extracted)[-1] <- date_vector
  df_extracted$Name <- spatvector$COMUNE # Add name of province
  df_extracted$codice_comunale <- spatvector$PRO_COM_T
  df_extracted$PRO_COM <- spatvector$PRO_COM
  
  
  #pivot the df, into long format
  
  db <- df_extracted %>% 
    select(-c(ID)) %>% 
    pivot_longer(
      cols = -c(Name, codice_comunale),
      names_to = "date",
      values_to = "tmax"
    ) %>%
    mutate(date = as.Date(date)) %>%
    rename(
#      PRO_COM_T = codice_comunale,
      name = Name
    ) %>%
    select(codice_comunale, name, date, tmax) %>% 
    drop_na(date) %>% 
    filter(codice_comunale %in% shapefile_comuni$PRO_COM_T) %>%  #filter to exclude the islands 
    left_join(. ,
              shapefile_comuni %>% st_drop_geometry() %>% select(PRO_COM_T, PRO_COM), by = c( "codice_comunale" = "PRO_COM_T")) 
  
  
  # check that the temperature progression makes sense
#  db %>% filter(is.na(tmax)) %>% View() sanity check
#    filter(PRO_COM=="001001") %>% 
#    mutate(tmax= tmax-273.15) %>% 
#  #  slice(-n()) %>% 
#    ggplot(aes(y=tmax, x=date))+
#    geom_line()
  
  
  #fill NA.s
  neigh_dt <- build_first_order_neighbours(shapefile_comuni, id_col = "PRO_COM")
  
  
  db_MaxTemp_fill <- fill_na_from_first_order_neighbours(
    db = db,
    neigh_dt = neigh_dt,
    id_col = "PRO_COM",
    date_col = "date",
    value_col = "tmax",
    n_iter = 4,
    verbose = TRUE
  )

  output_path <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr,  "/temp_LAND_shp2025_daily_max_", yrs, ".RDS")
  saveRDS(object = db, 
          file = output_path)  
    
  output_path <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr,  "/temp_LAND_shp2025_daily_max_clean_", yrs, ".RDS")
  saveRDS(object = db_MaxTemp_fill, 
          file = output_path)  
  
  #unlink(list.files(raw_data_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
  gc()
  
  
}






#test area

paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/2022/temp_LAND_shp2025_daily_max_clean_2022.RDS") %>% readRDS() %>% head() 
  filter(is.na(`2023-11-09`)) %>% 
  .$PRO_COM
db %>% 
  filter(is.na(tmax)) %>% 
  .$PRO_COM %>% unique()













###############################################################################
# Pre-process exposure and exploration of expsoures HOURLY
###############################################################################

#for processing requests that give you hourly rasters- when processed daily summaries are not available 



cds.key <- wf_set_key()

#dir_tif_yr <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr, "/max.tif")

for (yr in 2024) {
  
  product <- "temp_land"
  raw_data_dir <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr, "/raw")
  final_data_dir <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/", yr)
  
  dir.create(raw_data_dir, recursive = TRUE)
  
  for (month_index in 1:12) { #start monthly hourly data download
    
    request <- list(
      dataset_short_name = "reanalysis-era5-land",
      variable = "2m_temperature",
      year = as.character(yr),
      month = sprintf("%02d", month_index),
      day = sprintf("%02d", 1:31),
      time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"), #here you request all hours
      data_format = "grib",
      download_format = "zip",
      area = c(47, 6, 36, 19),
      target = paste(product, yr,  sprintf("%02d", month_index),  sep = "_")
    )
    
    out <- try(
      wf_request(
        request   = request,
        transfer  = TRUE,
        path      = raw_data_dir,
        time_out  = 3600,
        retry     = 100,   # poll every 3 minutes instead of every 30 s
        verbose   = TRUE
      ),
      silent = TRUE
    )
    
    if (inherits(out, "try-error")) {
      message("Month ", month_index, " failed. Waiting 10 minutes before retry...")
      Sys.sleep(600)
    }
    
    Sys.sleep(30)   # optional pause between months
    gc()
  } # end hourly data download 
  
  
  
  # 2.  combine the whole year into one raster file
      # it needs to be unzipped first
      temp_dir <- tempdir()
      
      zip_files <- list.files(
        raw_data_dir,
        pattern = paste0(yr),
        full.names = TRUE
      )
      
      raw_data_files <- lapply(zip_files, unzip, exdir = temp_dir)
      
      # Read the  file for the months of the year selected into a *list* of SpatRaster objects
      rasters <- lapply(raw_data_files, rast)
      
      # Combine into one big SpatRaster
      all_hours <- do.call(what = c, 
                           args = rasters)
      
      # Crop to retain the relevant geography (i.e. Italy)
      
      # Ensure the projection for both the raster and the shapefile is the same
      if (st_crs(shapefile_comuni) != terra::crs(all_hours)) {
        shapefile_comuni <- st_transform(shapefile_comuni, terra::crs(all_hours))
      }
      
      # Convert shapefile to a terra spatvector
      spatvector <- terra::vect(shapefile_comuni) #%>% project("OGC:CRS84")
      
      # Cut the raster so its only for italy
      all_hours_crop <- crop(all_hours, spatvector)      # Optional: crops to the extent of the shape # OK NOW IT WORKS
    
      
      # Combine into daily mean, max and min  
      
      # extract n of obs per day, by looking at the number of layers in January 
      hrs <- dim(rast(unlist(raw_data_files)[1]))[[3]]/31 # Number of daily readings 
      dim(all_hours_crop)[[3]] %% hrs == 0 # check its a whole number 
      
      # Group by day 
      n_days <- dim(all_hours_crop)[[3]] / hrs
      
      # Create an index grouping each hourly layers into one day
      day_groups <- rep(1:n_days, each = hrs) #the logic is that every hrs number of layer correspond one day
      
      # Use tapp to aggregate: one min,  one max and one mean for each group
       daily_max <- tapp(all_hours_crop, day_groups, fun = max)
       daily_mean <- tapp(all_hours_crop, day_groups, fun = mean)
       daily_min <- tapp(all_hours_crop, day_groups, fun = min)
  
  # Save
      
      writeRaster(
        daily_max,
        filename = file.path(final_data_dir, "max.tif"),
        overwrite = TRUE
      )
      
      writeRaster(
        daily_mean,
        filename = file.path(final_data_dir, "mean.tif"),
        overwrite = TRUE
      )
      
      writeRaster(
        daily_min,
        filename = file.path(final_data_dir, "min.tif"),
        overwrite = TRUE
      )
      
      

      # delete raw data and gc()
      
      #unlink(list.files(raw_data_dir, full.names = TRUE), recursive = TRUE, force = TRUE)
      gc()
      
}


# Extract raster to spatial weighted values per municipality ----------------------------------------------



db_23 <- paste0("X:/EMILIANO/Analisi Spaziali/temperature_LAND/2023/temp_LAND_shp2025_daily_max_2023.RDS") %>% readRDS() %>% 
  select(-c(ID)) %>% 
  pivot_longer(
    cols = -c(Name, codice_comunale),
    names_to = "date",
    values_to = "tmax"
  ) %>%
  mutate(date = as.Date(date)) %>%
  rename(
    PRO_COM = codice_comunale,
    name = Name
  ) %>%
  select(PRO_COM, name, date, tmax) %>% 
  drop_na(date)

neigh_dt <- build_first_order_neighbours(shapefile_comuni, id_col = "PRO_COM_T")

db_MaxTemp_fill <- fill_na_from_first_order_neighbours(
  db = db,
  neigh_dt = neigh_dt,
  id_col = "PRO_COM",
  date_col = "date",
  value_col = "tmax",
  n_iter = 4,
  verbose = TRUE
)



#load(paste0(dir.data, "/temp_max_weekly_italia_2008_2022_new")) %>% get() %>% 
#  filter(year(Week_date) == 2016 ) -> temp_weekly










###############################################################################
# Ecoregioni  
###############################################################################


shapefile_comuni[!is.na(shapefile_comuni$Ecoregioni),] %>% 
  ggplot()+
  geom_sf(aes(fill = Ecoregioni), color = NA) +
  scale_fill_viridis_d(option = "plasma", na.value = "grey80") +
  theme_minimal()


shapefile_comuni[!is.na(shapefile_comuni$Ecoregioni),] %>% 
  ggplot()+
  geom_sf(aes(fill = Ecoregioni), linewidth = 0.001) +
  scale_fill_viridis_d(option = "plasma", na.value = "grey80") +
  theme_minimal()




# Quanti nati per ecoregione? 

load(paste0(dir.data, "/outcome_patient_level.RData")) %>% get() %>%
  #  filter(comres2 %in% shapefile_comuni$PRO_COM_T) %>% # filter for Lazio; NB remove when carring out the whole country analysis  
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
#  filter(year(LMP)==2016) %>% 
  left_join(shapefile_comuni %>%  select(Ecoregioni, PRO_COM_T), 
            by = c("comres2"= "PRO_COM_T")) %>% 
  group_by(Ecoregioni) %>% 
  summarise(n=n()) %>% ungroup() %>%  drop_na() 
  

###############################################################################
# UTCI  
###############################################################################
gc()
  



UTCI <- readRDS(paste0(dir.data, "/weekly_avg_comune_UTCI_2008_2020.RDS")) %>% 
  pivot_longer(
    cols = matches("^20"),        # all columns starting with 20-...
    names_to = "date",
    values_to = "UTCI") %>% 
  mutate(
    UTCI = UTCI - 273.15, 
    date = as.Date(date))


qs_utci <- quantile(UTCI$UTCI, probs = seq(0, 1, 0.1), na.rm = TRUE)


# nati per utci during gw 0, 5, 10, 15, 20 

options(scipen = 999)
load(paste0(dir.data, "/outcome_patient_level.RData")) %>% get() %>%
  #  filter(comres2 %in% shapefile_comuni$PRO_COM_T) %>% # filter for Lazio; NB remove when carring out the whole country analysis  
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
  mutate(gw5 = LMP + lubridate::weeks(5), 
         gw10 = LMP + lubridate::weeks(10),
         gw15 = LMP + lubridate::weeks(15)) %>% 
  group_by(LMP, comres2, gw15) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  left_join(UTCI, 
            by = c("comres2"= "codice_comunale", "gw15"="date")) %>% 
  mutate(UTCI_cut = cut(
    UTCI,
    breaks = qs_utci,
    include.lowest = TRUE,
    labels = FALSE
  )) %>% 
  select(n, UTCI_cut) %>% 
  group_by(UTCI_cut) %>% 
  summarise(n_per_q = sum(n)) %>% 
  plot(type = "l", 
       main = "count of conceptions by quantiles of average UTCI in Italy (GW 15)")



## 1. Load and prepare outcome data
outcome <- load(paste0(dir.data, "/outcome_patient_level.RData")) %>% 
  get() %>%
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
  mutate(
    gw1  = LMP + weeks(1),
    gw5  = LMP + weeks(5),
    gw10 = LMP + weeks(10),
    gw15 = LMP + weeks(15)
  )

## 2. Pivot GW variables to long format
outcome_long <- outcome %>%
  pivot_longer(
    cols = starts_with("gw"),
    names_to = "gw",
    values_to = "gw_date"
  )

## 3. Aggregate conceptions
outcome_agg <- outcome_long %>%
  group_by(gw, comres2, gw_date) %>%
  summarise(n = n(), .groups = "drop")

## 4. Join UTCI
outcome_utci <- outcome_agg %>%
  left_join(
    UTCI,
    by = c("comres2" = "codice_comunale",
           "gw_date" = "date")
  )

## 5. Cut UTCI into quantiles
outcome_utci <- outcome_utci %>%
  mutate(
    UTCI_cut = cut(
      UTCI,
      breaks = qs_utci,
      include.lowest = TRUE,
      labels = FALSE
    )
  )

## 6. Table: counts per quantile × GW
tab_quantiles_UTCI <- outcome_utci %>%
  group_by(gw, UTCI_cut) %>%
  summarise(
    n_per_q = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

tab_quantiles_UTCI


ggplot(tab_quantiles_UTCI,
       aes(x = UTCI_cut, y = n_per_q, color = gw)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "UTCI quantile",
    y = "Count of conceptions",
    color = "Gestational window",
    title = "Counts of conceptions by UTCI quantiles and gestational window"
  ) +
  theme_minimal()


# Distribution of utci per ecoregione 

UTCI %>% 
  left_join(shapefile_comuni %>% select(PRO_COM_T, Ecoregioni), 
            by = c("codice_comunale" ="PRO_COM_T"))  %>% 
  group_by(Ecoregioni) %>% 
  summarise(
    n        = sum(!is.na(UTCI)),
    mean     = mean(UTCI, na.rm = TRUE),
    median   = median(UTCI, na.rm = TRUE),
    p25      = quantile(UTCI, 0.25, na.rm = TRUE),
    p75      = quantile(UTCI, 0.75, na.rm = TRUE),
    .groups  = "drop"
  )
  
gc()

###############################################################################
# NO2  
###############################################################################


no2 <- readRDS(paste0(dir.data, "/no2/no2_daily_mean_2016.RDS")) %>% 
  pivot_longer(
    cols = matches("^20"),        # all columns starting with 20-...
    names_to = "date",
    values_to = "no2")
#  ) %>%
#  mutate(date = as.Date(date), 
#         yr = year(date), 
#         week = week(date), 
#         no2_log  = log(no2+1), 
#         no2_scaled = scale(no2))



# space ---- 

library(ggplot2)


shapefile_comuni %>% 
  left_join(
    no2 %>% filter(date == "2016-01-01"),
    by = c("PRO_COM_T" = "codice_comunale")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = no2), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = "Average Values NO2 01.08.2016 (Italy)",
    fill = "NO2"
  )


no2 %>% 
  filter(Name == "Riofreddo") %>% 
  ggplot(aes(x = date, y = no2, group = 1)) +
  geom_line() +

no2 %>% 
  filter(Name == "Roma") %>% 
  ggplot(aes(x = date, y = no2, group = 1)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Valori medi 01.01.2016",
    fill = "Valore"
  )




no2_weekly <- no2 %>% 
  mutate(date = as.Date(date))

no2_weekly <- no2 %>% 
  mutate(
    date = as.Date(date),
    week_monday = floor_date(date, unit = "week", week_start = 1)
  ) %>% 
  group_by(
    ID, Name, codice_comunale, week_monday
  ) %>% 
  summarise(
    no2 = mean(no2, na.rm = TRUE),
    .groups = "drop"
  )


# NO2 by ecoregione -------------------------------------------------------


no2_weekly %>% 
  left_join(shapefile_comuni %>% select(PRO_COM_T, Ecoregioni), 
            by = c("codice_comunale" ="PRO_COM_T")) %>% 
  group_by(Ecoregioni) %>% 
  count(is.na(Ecoregioni))


no2_weekly %>% 
  left_join(shapefile_comuni %>% select(PRO_COM_T, Ecoregioni), 
            by = c("codice_comunale" ="PRO_COM_T")) %>% 
  group_by(Ecoregioni) %>% 
  ggplot(aes(Ecoregioni, no2)) +
  geom_boxplot()


gc()


no2_weekly %>% 
  left_join(
    shapefile_comuni %>% select(PRO_COM_T, Ecoregioni), 
    by = c("codice_comunale" = "PRO_COM_T")
  ) %>% 
  group_by(Ecoregioni) %>% 
  summarise(
    n        = sum(!is.na(no2)),
    mean     = mean(no2, na.rm = TRUE),
    median   = median(no2, na.rm = TRUE),
    p25      = quantile(no2, 0.25, na.rm = TRUE),
    p75      = quantile(no2, 0.75, na.rm = TRUE),
    .groups  = "drop"
  )





# correlation across time 

no2_weekly %>% 
  left_join(
    temp_weekly %>% 
      select(Week_date, codice_comunale, avg_weekly),
    by = c("week_monday" = "Week_date",
           "codice_comunale" = "codice_comunale")
  ) %>% 
  drop_na() %>% 
  left_join(
    shapefile_comuni %>% 
      select(COD_REG, PRO_COM_T), 
    by = c("codice_comunale" = "PRO_COM_T")
  ) %>% 
  group_by(Name) %>% 
  summarise(
    correlat = cor(no2, avg_weekly, use = "complete.obs"),
    .groups = "drop"
  ) %>% 
  mutate(x = row_number()) %>% 
  ggplot(aes(x = x, y = correlat)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Index", y = "Correlation") 


# correlation across week of the year (how does the correlation change between weeks of the year)

no2_weekly %>% 
  left_join(
    temp_weekly %>% 
      select(Week_date, codice_comunale, avg_weekly),
    by = c("week_monday" = "Week_date",
           "codice_comunale" = "codice_comunale")
  ) %>% 
  drop_na() %>% 
  left_join(
    shapefile_comuni %>% 
      select(COD_REG, PRO_COM_T), 
    by = c("codice_comunale" = "PRO_COM_T")
  ) %>% 
  group_by(week_monday) %>% 
  summarise(
    correlat = cor(no2, avg_weekly, use = "complete.obs"),
    .groups = "drop"
  ) %>% 
  mutate(x = row_number()) %>% 
  ggplot(aes(x = x, y = correlat)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Index", y = "Correlation") 




# correlation across space

cor_data <- no2_weekly %>% 
  left_join(
    temp_weekly %>% 
      select(Week_date, codice_comunale, avg_weekly),
    by = c(
      "week_monday" = "Week_date",
      "codice_comunale" = "codice_comunale"
    )
  ) %>% 
  drop_na() %>% 
  group_by(codice_comunale) %>% 
  summarise(
    correlat = cor(no2, avg_weekly, use = "complete.obs"),
    .groups = "drop"
  )

sf_cor <- shapefile_comuni %>% 
  left_join(
    cor_data,
    by = c("PRO_COM_T" = "codice_comunale")
  )

ggplot(sf_cor) +
  geom_sf(aes(fill = correlat)) +
  theme_minimal() +
  labs(title = "corr between temperature and NO2 across space") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(fill = "Correlation")



# How many conceptions per decile of NO2 at LMP

qs_no2 <- quantile(no2$no2, probs = seq(0, 1, 0.1), na.rm = TRUE)


load(paste0(dir.data, "/outcome_patient_level.RData")) %>% get() %>%
#  filter(comres2 %in% shapefile_comuni$PRO_COM_T) %>% # filter for Lazio; NB remove when carring out the whole country analysis  
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
  filter(year(LMP)==2016) %>% 
  group_by(LMP, comres2) %>% 
  summarise(n=n()) %>% ungroup() %>% 
  left_join(no2_weekly, 
            by = c("comres2"= "codice_comunale", "LMP"="week_monday")) %>% 
  mutate(no2_cut = cut(
    no2,
    breaks = qs,
    include.lowest = TRUE,
    labels = FALSE
  )) %>% 
  select(n, no2_cut) %>% 
  group_by(no2_cut) %>% 
  summarise(n_per_q = sum(n)) %>% 
  plot(type = "l", 
       main = "count of conceptions by quantiles of NO2 in Italy (2016)")


## 1. Load and prepare outcome data
outcome <- load(paste0(dir.data, "/outcome_patient_level.RData")) %>%
  get() %>%
  filter(ETA_GESTAZ %in% min_GA:42) %>%
  filter(year(LMP) == 2016) %>%
  mutate(
    gw1  = LMP + weeks(1),
    gw5  = LMP + weeks(5),
    gw10 = LMP + weeks(10),
    gw15 = LMP + weeks(15)
  )

## 2. Pivot gestational windows to long format
outcome_long <- outcome %>%
  pivot_longer(
    cols = starts_with("gw"),
    names_to  = "gw",
    values_to = "gw_date"
  )

## 3. Aggregate conceptions
outcome_agg <- outcome_long %>%
  group_by(gw, comres2, gw_date) %>%
  summarise(n = n(), .groups = "drop")

## 4. Join weekly NO2
outcome_no2 <- outcome_agg %>%
  left_join(
    no2_weekly,
    by = c(
      "comres2" = "codice_comunale",
      "gw_date" = "week_monday"
    )
  )

## 5. Cut NO2 into quantiles
outcome_no2 <- outcome_no2 %>%
  mutate(
    no2_cut = cut(
      no2,
      breaks = qs_no2,
      include.lowest = TRUE,
      labels = FALSE
    )
  )

## 6. Table: counts per quantile × GW
tab_quantiles <- outcome_no2 %>%
  group_by(gw, no2_cut) %>%
  summarise(
    n_per_q = sum(n, na.rm = TRUE),
    .groups = "drop"
  )

tab_quantiles

  

ggplot(tab_quantiles,
       aes(x = no2_cut, y = n_per_q, color = gw)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    x = "NO₂ quantile",
    y = "Count of conceptions",
    color = "Gestational window",
    title = "Counts of conceptions by NO₂ quantiles (year 2016)"
  ) +
  theme_minimal()



  

qs_temp <- quantile(temp_weekly$max_weekly, probs = seq(0, 1, 0.1), na.rm = TRUE)


outcome_exposure_lead %>%
  mutate(cop_lead0_q10 = cut(
    cop_lead0,
    breaks = qs_temp,
    include.lowest = TRUE,
    labels = FALSE
  )) %>% 
  select(n, cop_lead0_q10) %>% 
  group_by(cop_lead0_q10) %>% 
  summarise(n_per_q = sum(n)) %>% 
  plot(type = "l", 
       main = "count of conceptions by quantiles of temperature")




# -------------------------------------------------------

#Quantiles relative to the comune di residenza 

qs_by_comune <- no2_weekly %>%
  group_by(codice_comunale) %>%
  summarise(
    qs = list(
      quantile(no2, probs = seq(0, 1, 0.1), na.rm = TRUE)
    ),
    .groups = "drop"
  )

load(paste0(dir.data, "/outcome_patient_level.RData")) %>% 
  get() %>%
  filter(ETA_GESTAZ %in% min_GA:42) %>% 
  filter(year(LMP) == 2016) %>% 
  group_by(LMP, comres2) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  left_join(
    no2_weekly,
    by = c("comres2" = "codice_comunale", "LMP" = "week_monday")
  ) %>% 
  left_join(
    qs_by_comune,
    by = c("comres2" = "codice_comunale")
  ) %>% 
  mutate(
    no2_cut = mapply(
      function(x, q) {
        cut(
          x,
          breaks = q,
          include.lowest = TRUE,
          labels = FALSE
        )
      },
      no2, qs
    )
  ) %>% 
  select(n, no2_cut) %>% 
  group_by(no2_cut) %>% 
  summarise(n_per_q = sum(n), .groups = "drop") %>% 
  plot(
    type = "l",
    main = "Count of conceptions by within-comune NO2 quantiles (Italy, 2016)",
    xlab = "NO2 decile (within comune)",
    ylab = "Number of conceptions"
  )


########################################################################
# Lista NO2 
########################################################################

load(paste0(dir.data, "/lista_NO2mean_year.RData")) %>% 
  bind_rows(lista_NO2mean_year) %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(ID, week) %>%
  summarise(no2 = mean(no2, na.rm = TRUE), .groups = "drop")



###############################################################################
# PM2.5  
###############################################################################

# Process the raw data and extract raster for the year
# Unzip ---
# path to zip archive
dir.zip <- file.path(dir.data, "pm_2p5.zip")
# where you want the filtered files extracted
outdir <- file.path(dir.data, "pm_2p5_mean")
# create the target folder if it doesn't exist
if (!dir.exists(outdir)) dir.create(outdir)

# list contents of the zip
files_in_zip <- unzip(dir.zip, list = TRUE)
# filter for files containing "_min_"
files_mean <- files_in_zip$Name[grepl("_mean_", files_in_zip$Name)]
# extract only those files into your target folder
unzip(dir.zip, files = files_mean, exdir = outdir)


# unite the files into one 


files_mean <- files_in_zip$Name[grepl("_mean_", files_in_zip$Name)]


# list contents of the zip

files <- list.files(
  outdir, 
  pattern = "_mean_",
  full.names  = TRUE)


dfs <- map(files, readRDS)

# Full join all data frames by codice_comunale
pm2p5_all <- reduce(dfs, full_join, by = "codice_comunale") %>% 
  select(-matches("\\.x$"), -matches("\\.y$")) %>% 
  filter(codice_comunale %in% shapefile_comuni$PRO_COM_T)

pm2p5_long <- pm2p5_all %>%
  pivot_longer(
    cols = matches("^20"),        # all columns starting with 20-...
    names_to = "date",
    values_to = "pm2p5"
  ) %>%
  mutate(date = as.Date(date), 
         yr = year(date), 
         week = week(date), 
         pm_log  = log(pm2p5+1), 
         pm_scaled = scale(pm2p5))


saveRDS(pm2p5_long,
        paste0(dir.data, "/pm2p5_mean_daily_lazio_2013_2019.RDS"))


###############################################################################
# PM2.5 exploration -----------------------------------------------------------------
###############################################################################


readRDS(paste0(dir.data, "/pm2p5_mean_daily_lazio_2013_2019.RDS")) -> pm2p5 

# space ---- 

library(ggplot2)

lat <- 41.8967
lon <- 12.4822

point_sf <- st_as_sf(data.frame(lon = lon, lat = lat),
                     coords = c("lon", "lat"),
                     crs = st_crs(shapefile_comuni))

shapefile_comuni %>% 
  left_join(
    data.frame(pm =   rowMeans(test[, 1:30]),
               codice_comunale = test$codice_comunale),
    by = c("PRO_COM_T" = "codice_comunale")
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = pm), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(
    title = "Valori del 3 dicembre 2013",
    fill = "Valore"
  )

rowMeans(test[, 1:30]) 

# time --- 

pm2p5_long %>% 
  filter(Name %in% c("Roma", "Leonessa", "Frosinone"), 
         str_detect(date, "^2013")) %>% 
  ggplot(aes(x = date, y = value, color = Name, group = Name)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.2) +  # add loess trend
  theme_minimal() +
  labs(
    x = "Date",
    y = "Value",
    title = "Values over time by Comune"
  )


pm2p5_long$value %>% log() %>%  hist()

pm2p5_long$value %>% log() %>%  hist()
pm2p5_long$value %>% scale()






