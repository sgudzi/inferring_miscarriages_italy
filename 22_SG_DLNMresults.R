# define model and centring option 

rm(list = ls())
gc()

#modello_i <- "models_nb_no2"
#modello_i <- "models_nb_tmax_NO2_cov_ntile"

#modello_i <- "models_nb_tmax_NO2_cov_ntile_v2"



#modello_i<- "models_nb_soloNO2_cov_ntile"
#centering_boolean <- T # only used for naming purposes

# --------------------------------------------------------------------------

# create folder for results 
#results_path <- file.path(paste0("results/", modello_i, "/centering_", centering_boolean))
results_path <- file.path(paste0("results/", modello_i))
dir.create(results_path, recursive = T, showWarnings = F)


# --------------------------------------------------------------------------

#shapefile_comuni <- load(paste0(getwd(), "/shapefile_comuni.RData")) %>% get()

#label_name_eco = c(
#  "1A" = "Catena Alpina",
#  "1B" = "Pianura Padana",
#  "1C" = "Catena Appenninica",
#  "2B" = "Tirreno e isole",
#  "2C" = "Puglia e Adriatico"
#)

label_name_eco_eng = c( # now cross checked with the whole thing 
  "1A" = "Alpine Chain",
  "1B" = "Padana Plain",
  "1C" = "Apennine Chain",
  "2B" = "Tyrrhenian Coast and the islands",
  "2C" = "Apulia and the Adriatic Coast"
)


#shapefile_comuni %>% 
#  group_by(Ecoregioni) %>% 
#  summarise(geometry = st_union(geometry)) %>% 
#  ggplot()+
#  geom_sf(aes(fill = Ecoregioni))+
#  theme_bw()+
#  scale_fill_discrete(labels = label_name_eco_eng)-> mappa_ecoreg
#

shapefile_comuni$Ecoregioni %>% unique()-> ecoregioni_names #EC is this correct?
#ecoregioni_names<- c("1B", "1A", "1C", "2B", "2C")
#
#overall_ris <- list()
#for (eco in ecoregioni_names) {
#  
#  overall_ris[[eco]] <- readRDS(
#    file = file.path("output",
#                     "Final_5", #Replace here with whatever different result
#                     paste0("res_", eco, ".rds"))
#  )
#}

#View(overall_ris)


# Risultati DLNM  ---------------------------------------------------------

fn_make_cp <- function(modello, 
                       centeringPerc = 0.5, 
                       atPerc = F){
  

  t_seq <- seq(
    floor(modello$dlnm_tmax$tmax_min) + 1,
    floor(modello$dlnm_tmax$tmax_max),
    by = .1
  )
  
  
  # extract number of df in lag dimension
  df_lag <- as.numeric(sub(".*\\.l", "", modello$dlnm_tmax$cb_names)) %>% max()
  
  basis <- crossbasis(
    x = t_seq,
    lag = c(-22,-3),
    argvar = list(
      fun = "ns",
      knots = modello$dlnm_tmax$var_knots,
      intercept = FALSE
    ),
    arglag = list(fun = "ns", 
                  df = df_lag, 
                  intercept = TRUE)
  )
  
  
  
  p_centering <- quantile(t_seq, centeringPerc, na.rm = TRUE)

  if(atPerc==T){
    p_at <- quantile(t_seq, atPerc, na.rm = TRUE)
    
  cp <- crosspred(
    basis,
    coef  = modello$dlnm_tmax$coef,
    vcov  = modello$dlnm_tmax$vcov,
    cen   = p_centering,
    at    = p_at,
    from  = quantile(t_seq, 0, na.rm = TRUE),
    to    = quantile(t_seq, 1, na.rm = TRUE),
#    from  = floor(modello$dlnm_tmax$tmax_min)+1,
#    to    = floor(modello$dlnm_tmax$tmax_max),
    by    = .1,
    cumul = T
  )
  }
  
  if(atPerc==F){
    
    cp <- crosspred(
      basis,
      coef  = modello$dlnm_tmax$coef,
      vcov  = modello$dlnm_tmax$vcov,
      cen   = p_centering,
      from  = quantile(t_seq, 0, na.rm = TRUE),
      to    = quantile(t_seq, 1, na.rm = TRUE),
      #from  = floor(modello$dlnm_tmax$tmax_min)+1,
      #to    = floor(modello$dlnm_tmax$tmax_max),
      by    = .1,
      cumul = T
    )
  }
  
  cp
}





# make cp extraction for no2

fn_make_cp_no2 <- function(modello, cen_at_min = TRUE){
  
  overall_ris[1]$`1B` -> modello
  modello$
  no2_seq <- seq(
    floor(modello$dlnm_no2$no2_min) + 1,
    floor(modello$dlnm_no2$no2_max),
    by = .1
  )
  
  # extract number of df in lag dimension
  df_lag <- as.numeric(sub(".*?(\\d+)_no2.*", "\\1", modello$dlnm_no2$cb_names)) %>% max()
  df_var <- as.numeric(sub(".*v(\\d+)\\..*", "\\1", modello$dlnm_no2$cb_names)) %>% max()
  
  basis <- crossbasis(
    x = no2_seq,
    lag = -22,
    argvar = list(fun = "ns", df = df_var, intercept = FALSE),
    arglag = list(fun = "ns", df = df_lag, intercept = TRUE)
  )
  
  cp <- crosspred(
    basis,
    coef = modello$dlnm_no2$coef,
    vcov = modello$dlnm_no2$vcov,
    cen  = modello$dlnm_no2$no2_cen,
#    cen = 20,
    from = floor(modello$dlnm_no2$no2_min)+1,
    to   = floor(modello$dlnm_no2$no2_max),
    by   = 1, 
    cumul = T
  )

  
  if(cen_at_min){
    mmt <- cp$predvar[which.min(exp(cp$allfit))]
    cp <- crosspred(
      basis,
      coef = modello$dlnm_no2$coef,
      vcov = modello$dlnm_no2$vcov,
      cen  = modello$dlnm_no2$no2_min,
      from  = quantile(t_seq, 0, na.rm = TRUE),
      to    = quantile(t_seq, 1, na.rm = TRUE),
      
      #from = floor(modello$dlnm_no2$no2_min)+1,
      #to   = floor(modello$dlnm_no2$no2_max),
      by   = 1, 
      cumul = T
    )
  }
  
  cp
}

# cp_functions <- list(
#   no2 = fn_make_cp_no2,
#   temp = fn_make_cp
# )


# End crosspred extraction fn ---------------------------------------------


# Begin extraction of plots -----------------------------------------------

# Extracts a plot that is marginailised against lag. The effect at each lag of a temperature range
# the temp range is above or below the ecoregion specific mct

extract_RR_by_temp_group <- function(cp, eco,
                                     cum = F, # is it the additive cumulative effect? cumfit?
                                     mct_plus = F, # true if heat 
                                     no2_80perc = F,
                                     start_gw = 3, 
                                     end_gw = 22,
                                     temp_90perc = F, 
                                     temp_10perc = F,
                                     temp_at90perc = F,
                                     temp_at10perc = F,
                                     mct_minus = F # true of cold
                                     ){
  

          # single lag effect or cumulative additive lag effect?--------------
  
        # flip RR by lag and temp matrix - necessary as the matrix goes from -22 to 0 otherwise 
        if(cum==F){
          cp$matfit[,  20:0]-> RR_swapped
          cp$matlow[,  20:0]-> low_swapped
          cp$mathigh[, 20:0]-> high_swapped
        }
  
        if(cum==T){
            cp$cumfit[,  20:0]-> RR_swapped
            cp$cumlow[,  20:0]-> low_swapped
            cp$cumhigh[, 20:0]-> high_swapped
        }
  
  
        #heat or cold? -----------------------------------------------------
  

        # slice by threshold
        if(mct_plus){ # of heat
          max_conc_temp <- cp$predvar[which.max(exp(cp$allfit))]
          RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) > max_conc_temp, ] #subset only hot rows
          RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) > max_conc_temp, ]
          RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) > max_conc_temp, ]
        }
        
        if(mct_minus){ # of cold
          max_conc_temp <- cp$predvar[which.max(exp(cp$allfit))]
          RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) < max_conc_temp, ] #subset only hot rows
          RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) < max_conc_temp, ]
          RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) < max_conc_temp, ]
        }
  
         if(temp_90perc == T){ # of heat
          threshold_90 <- quantile(cp$predvar, .90)[1] 
          RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) > threshold_90, ] #subset only hot rows
          RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) > threshold_90, ]
          RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) > threshold_90, ]
         }
  
          if(temp_at90perc == T){ # of heat
            threshold_90 <- quantile(cp$predvar, .90)[1] %>% as.numeric()
            closest_row <- which.min(abs(as.numeric(rownames(RR_swapped)) - threshold_90))
            RR_sliced   <- RR_swapped[closest_row, , drop = FALSE]
#            RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) == threshold_90, ] #subset only hot rows
            RR_l_sliced <- low_swapped[closest_row, , drop = FALSE]
            RR_u_sliced <- high_swapped[closest_row, , drop = FALSE]
          }
  
          if(temp_10perc == T){ # of cold
            threshold_10 <- quantile(cp$predvar, .10)[1] 
            RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) < threshold_10, ] #subset only hot rows
            RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) < threshold_10, ]
            RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) < threshold_10, ]
          }
  
         if(temp_at10perc == T){ # of heat
           threshold <- quantile(cp$predvar, .10)[1] 
           closest_row <- which.min(abs(as.numeric(rownames(RR_swapped)) - threshold))
           RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) == threshold_10, ] #subset only hot rows
           RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) == threshold_10, ]
           RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) == threshold_10, ]
         }

    # NO2?
  
      # slice by threshold
      if(no2_80perc == T){ # of heat
        threshold_80 <- quantile(cp$predvar, .80)[1] 
        RR_sliced <- RR_swapped[as.numeric(rownames(RR_swapped)) > threshold_80, ] #subset only hot rows
        RR_l_sliced <- low_swapped[as.numeric(rownames(low_swapped)) > threshold_80, ]
        RR_u_sliced <- high_swapped[as.numeric(rownames(high_swapped)) > threshold_80, ]
      }
  
#  start_gw<- 3
#  end_gw<-20
  if(temp_at90perc == T){ # of heat
    data.frame(
      RR   = exp(RR_sliced[(start_gw):(end_gw)]),
      RR_l = exp(RR_l_sliced[(start_gw):(end_gw)]),
      RR_u = exp(RR_u_sliced[(start_gw):(end_gw)]),
      lag = seq((start_gw), (end_gw), by =1),
      eco  = eco
    ) ->df
  }
  
  if(temp_at10perc == T){ # of heat
    data.frame(
      RR   = exp(RR_sliced[(start_gw):(end_gw)]),
      RR_l = exp(RR_l_sliced[(start_gw):(end_gw)]),
      RR_u = exp(RR_u_sliced[(start_gw):(end_gw)]),
      lag = seq((start_gw), (end_gw), by =1),
      eco  = eco
    ) -> df
  }
  
  if(temp_at90perc == F &
     temp_at10perc == F){ # of heat   
    data.frame(
      RR   = exp(RR_sliced[,start_gw:end_gw] %>% colSums()),
      RR_l = exp(RR_l_sliced[,start_gw:end_gw] %>% colSums()),
      RR_u = exp(RR_u_sliced[,start_gw:end_gw] %>% colSums()),
      lag = seq((start_gw+1), (end_gw+1), by =1),
      eco  = eco
    )->df
  }
  df
}


# Function to extract selected gw groups
# The resulting plot is marginalised against lag

extract_RR_by_gw_group <- function(cp, eco, start_gw, end_gw ){
  
  cp$matfit[, 19:0]-> RR_swapped
  cp$matlow[, 19:0]-> low_swapped
  cp$mathigh[, 19:0]-> high_swapped

   data.frame(
   # lag  = seq(cp$lag[1], cp$lag[2], 1),
   RR   = exp((RR_swapped)[,start_gw:end_gw] %>% rowSums()),
   RR_l = exp((low_swapped)[,start_gw:end_gw] %>% rowSums()),
   RR_u = exp((high_swapped)[,start_gw:end_gw] %>% rowSums()),
   temp = cp$predvar,
   eco  = eco
 )
}


# Function to extract overall effect of exposure (throughout the lag period)
# marginalised against exposure. Centered at optimal conception temperature

extract_overall_exposure_effect <- function(cp, eco){

  data.frame(
    temp = cp$predvar,
    RR   = exp(cp$allfit),
    RR_l = exp(cp$alllow),
    RR_u = exp(cp$allhigh),
    eco  = eco
  )
}


# Get single curves by GA lead
extract_RR_by_gw <- function(cp, eco ){
  
  cp
  cp$matfit[, 19:0]-> RR_swapped
  cp$matlow[, 19:0]-> low_swapped
  cp$mathigh[, 19:0]-> high_swapped
  
  df_list <- vector("list", 20)
  
  for (i in 1:19) {
    df_list[[i]] <- data.frame(
      RR   = exp(RR_swapped)[, i],
      RR_l = exp(low_swapped)[, i],
      RR_u = exp(high_swapped)[, i],
      temp = cp$predvar,
      lead = i,
      eco  = eco
    )
  }
  return(df_list)
}


# End of plot functions definition ------------------------------
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# --------------------------------------------------------------

# Begin Plotting ------------------------------------------------
# Plotting is achieved by calling the bespoke crosspred function 
# wrappers + bespoke plotting functions -------------------------

# ---------------------------------------------------------------
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# Plot Overall effect marg against temperature during GW 0-22

purrr::map(
  overall_ris,
  ~ extract_overall_exposure_effect(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione
  )) %>% bind_rows() %>% 
  ggplot(aes(x = temp, 
             y = RR, 
             group = eco, 
             colour = factor(eco),
             ymin = RR_l,
             ymax = RR_u,
             fill = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Temperature", 
    y = "Relative Risk", 
    colour = "eco",
    title = "Overall exposure effect during GW 0-22", 
    subtitle = "Centererd around maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng, name = "Ecoregions")+ 
  scale_fill_hue(labels = label_name_eco_eng, name = "Ecoregions") -> p01
# todo: extract mct per ecoregione

ggsave(filename = paste0(results_path, "/","RR_marg_against_temp_across_0_22.png"), 
       plot = p01,
       width = 7.49,
       height = 3.53)
  
# End of Plot Overall effect of temperature during GW 0-22  ---------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Plot cumulative associations marg against lag for  temperature groups --

# Effetto temperatura of *heat*

purrr::map(
  overall_ris,
  ~ extract_RR_by_temp_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione,
    cum = F,
    start_gw = 5, 
    end_gw = 12,
    mct_plus = T # effect of heat
  )) %>% bind_rows() %>% 
  ggplot(aes(x = lag, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Lag", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Overall exposure effect of hot temperatures"), 
    subtitle = "Above maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p02

ggsave(filename = paste0(results_path, "/","RR_marg_against_lag_cum_effect_above_mct.png"), 
       plot = p02,
       width = 7.49,
       height = 3.53)



# -------------------------------------------------------------------------

# Effetto temperatura of *cold*

purrr::map(
  overall_ris,
  ~ extract_RR_by_temp_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione, 
    mct_minus = T,  # effect of heat
    cum = F, 
    start_gw = 1, 
    end_gw = 23
  )) %>% bind_rows() %>% 
  ggplot(aes(x = lag, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Lag", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Overall exposure effect of cold temperatures"), 
    subtitle = "Below maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p03 # I suggest we add one df for the lag dimension

ggsave(filename = paste0(results_path, "/","RR_marg_against_lag_cum_effect_below_mct.png"), 
       plot = p03,
       width = 7.49,
       height = 3.53)


# -------------------------------------------------------------------------


# Extract cumulative associations by temperature for selected lags   --------

start_lag <-1
end_lag <- 5

# effetto temperatura by lag group
RR_by_gw_groups <- purrr::map(
  overall_ris,
  ~ extract_RR_by_gw_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione, 
    start_gw = start_lag, 
    end_gw = end_lag
  )) %>% bind_rows() %>% 
  ggplot(aes(x = temp, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Temperature", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Overall exposure effect during GW ", start_lag, "-", end_lag), 
    subtitle = "Centererd around maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p04

ggsave(filename = paste0(results_path, "/","RR_marg_against_temp_across_", start_lag, "_", end_lag,".png"), 
       plot = p04,
       width = 7.49,
       height = 3.53)




# End of slice functions definition ---------------------------------------

# -------------------------------------------------------------------------

# Extract cumulative associations by temperature across all lags   --------

# Interpretation: the curve represents the augmentative cumulative effect of the exposure 
# across all time points specified in the model, relative to a centering value specified in the crosspred function. 
# The crosspred is centered 
# by the maximum conception rate, which is reasoned to be the optimal temperature
# to sustain a pregnancy. 

# AUGMENTATIVE EXPSOURE TO HEAT
# momenti di sviluppo 

purrr::map(
  overall_ris,
  ~ extract_RR_by_temp_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione, 
    mct_plus = T,  # effect of heat
    cum = T
  )) %>% bind_rows() %>% 
  ggplot(aes(x = lag, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Lag", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Augmentative exposure to heat"), 
    subtitle = "Above maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p06

ggsave(filename = paste0(results_path, "/","RR_augmentative_lag_above_mct.png"), 
       plot = p06,
       width = 7.49,
       height = 3.53)


# AUGMENTATIVE EXPSOURE TO COLD


purrr::map(
  overall_ris,
  ~ extract_RR_by_temp_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione, 
    mct_minus  = T,  # effect of heat
    cum = T
  )) %>% bind_rows() %>% 
  ggplot(aes(x = lag, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Lag", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Augmentative exposure to cold"), 
    subtitle = "Below maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p07

ggsave(filename = paste0(results_path, "/","RR_augmentative_lag_below_mct.png"), 
       plot = p07,
       width = 7.49,
       height = 3.53)


# Now I look at the cumulative effect within certain meaningful gw ranges


# momenti di sviluppo 

purrr::map(
  overall_ris,
  ~ extract_RR_by_temp_group(
    cp  = fn_make_cp(.x, centered = TRUE),
    eco = .x$ecoregione, 
    mct_plus = T,  # effect of heat
    cum = T
  )) %>% bind_rows() %>% 
  ggplot(aes(x = lag, 
             y = RR, 
             group = eco, 
             colour = factor(eco))) + 
  geom_line(alpha = 0.8) + 
  geom_ribbon(aes(ymin = RR_l,
                  ymax = RR_u,
                  fill = factor(eco)),
              alpha = 0.2,
              colour = NA) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_minimal() + 
  labs(
    x = "Lag", 
    y = "Relative Risk", 
    colour = "eco",
    title = paste0("Augmentative exposure to heat"), 
    subtitle = "Above maximum conception temperature"
  ) + 
  scale_color_hue(labels = label_name_eco_eng) -> p06




# END ------




# Extract slice plots (exposure-outcome by lag) ---------------------------


lags_interest <- 1:23

df_all_temp <- purrr::imap_dfr(
  cp_functions,
  function(cp_fun, cp_name) {
    
    purrr::map_dfr(overall_ris, function(m){
      
      ## ---- safely create cp ----
      cp <- purrr::possibly(cp_fun, otherwise = NULL)(
        m,
        centered = centering_boolean
      )
      
      ## skip failed models
      if (is.null(cp) || is.null(cp$predvar))
        return(NULL)
      
      ## ---- extract all temperatures ----
      purrr::map_dfr(
        cp$predvar,
        function(temp_val){
          
          purrr::possibly(extract_temp_slice, otherwise = NULL)(
            cp,
            eco = m$ecoregione,
            temp_value = temp_val
          )
          
        }
      )
      
    }) |>
      dplyr::mutate(exposure = cp_name)
  }
)

# dev.new()

# split df by no2 and temp, then run the visualisations

df_split <- split(df_all_temp, df_all_temp$exposure)

# define functions to extract plots by exposure by which df was split   

plots_by_exposure <- function(df){

  df$exposure[1]-> exp_i # extract name of the exposure for plot naming purposes 
  
    # create subset dir where the analysis goes
      ggplot(df, 
             aes(x = temp, 
                 y = RR, 
                 group = eco, 
                 colour = factor(eco))) + 
        geom_line(alpha = 0.8) + 
        geom_ribbon(aes(ymin = RR_l,
                        ymax = RR_u,
                        fill = factor(eco)),
                    alpha = 0.2,
                    colour = NA) +
        geom_hline(yintercept = 1, linetype = "dashed") +
        theme_minimal() + 
        labs(
          x = "Temperature", 
          y = "Relative Risk", 
          colour = "eco",
          title = "Exposure - Response curve by lags"
        ) + 
        facet_wrap(~lag, nrow = 1)+ 
        scale_color_hue(labels = label_name_eco_eng) -> p1
    
  ggsave(filename = paste0(results_path, "/","RR_", exp_i, "_by_lag.png"), 
         plot = p1)
    
  
  
    ggplot(df,
           aes(temp, RR, colour = factor(lag),
               group = lag)) +
      geom_line(alpha = .8) +
      facet_wrap(~eco, labeller = labeller(eco = label_name_eco_eng)) +
      geom_hline(yintercept = 1, linetype="dashed") +
      theme_minimal() +
      labs(colour = "Lag", 
           title = "Exposure - response curve by lag and macro-eco-region") -> p2
    
    ggsave(filename = paste0(results_path, "/","RR_", exp_i, "_by_lag_and_eco_region.png"), 
           plot = p2)
    
    # should the seasonal component be relative to the municipality? id_space_2*f(id_week, rw2)? 
    
} # end of exposure-response by lag extraction function


# call function by exposure

lapply(df_split, plots_by_exposure)


# End Extraction of slice plots (exposure-outcome by lag) ----------------

# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# Extract slice plots (lag-outcome by temperature) -----------------------

# temperature associated with highest RR per lag

df_all_temp %>%
  filter(exposure=="temp") %>% 
  group_by(eco, lag) %>%
  slice_max(RR, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  ggplot(aes(x = lag, y = temp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ eco) +
  labs(
    x = "Lag",
    y = "Temperature at maximum RR",
    title = "Temperature associated with highest RR by lag"
  ) +
  theme_minimal()-> p4

ggsave(filename = paste0(results_path, "/","Temperature_associated_with_max_RR_by_lag.png"), 
       plot = p4)



# Seasonality -------------------------------------------------------------

# Run only for sin cos seasonality

bind_cols(lapply(overall_ris,
                 function(m) m[["fixed"]]["sin_i", "0.5quant"]*as.numeric(scale(sin((2*pi*1:53)/53))) + m[["fixed"]]["cos_i", "0.5quant"]*as.numeric(scale(cos((2*pi*1:53)/53))))%>% 
            bind_rows() %>% 
            mutate(week = 1:53) %>% 
            pivot_longer(cols =all_of( ecoregioni_names), values_to = "RR"),
          lapply(overall_ris,
                 function(m) m[["fixed"]]["sin_i", "0.025quant"]*as.numeric(scale(sin((2*pi*1:53)/53))) + m[["fixed"]]["cos_i", "0.5quant"]*as.numeric(scale(cos((2*pi*1:53)/53))))%>% 
            bind_rows() %>% 
            mutate(week = 1:53) %>% 
            pivot_longer(cols =all_of( ecoregioni_names), values_to = "RR_u") %>% 
            select(-c(week, name)),
          lapply(overall_ris,
                 function(m) m[["fixed"]]["sin_i", "0.975quant"]*as.numeric(scale(sin((2*pi*1:53)/53))) + m[["fixed"]]["cos_i", "0.5quant"]*as.numeric(scale(cos((2*pi*1:53)/53))))%>% 
            bind_rows() %>% 
            mutate(week = 1:53) %>% 
            pivot_longer(cols =all_of( ecoregioni_names), values_to = "RR_l")%>% 
            select(-c(week, name))) %>% 
  ggplot(aes(x = week,
             y = RR,
             ymin = RR_l,
             ymax = RR_u,
             color = name, 
             fill = name))+
  geom_line()+
  geom_ribbon(alpha = .2, color = NA)+
  theme_minimal()+
  labs(x = "Week") +
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  scale_color_discrete(labels = label_name_eco)+
  scale_fill_discrete(labels = label_name_eco) -> p5

ggsave(filename = paste0(results_path, "/","seasonality_sin_cos.png"), 
       plot = p5)


# Seasonality rw2 ---------------------------------------------------------

# Run only for rw2 seasonality 

lapply(overall_ris,
       function(m) m$id_week %>% 
         mutate(eco = m$eco)) %>% 
  bind_rows() %>% 
  ggplot(aes(x = ID,
             ymin = `0.025quant`,
             y = `0.5quant`,
             ymax = `0.975quant`,
             color = eco,
             fill = eco))+
  geom_line()+
  geom_ribbon(alpha = .2, color = NA)+
  theme_bw()+
  labs(x = "Week",
       y = "RR 95% CrI")+
  scale_color_discrete(labels = label_name_eco)+
  scale_fill_discrete(labels = label_name_eco) -> p6

ggsave(filename = paste0(results_path, "/","seasonality_rw2.png"), 
       plot = p6)



# Termine lineare ---------------------------------------------------------


fn_lin_terms = function(modello){
  exp(modello[["fixed"]])%>% 
    rownames_to_column(var = "var") %>% 
    as_tibble() %>% 
    mutate(eco = modello[["ecoregione"]]) 
}

# Mappe bym ---------------------------------------------------------------

# Posterior probability that the RR is greater than one
shp_list <-  load(paste0(getwd(), "/shp_list.RData")) %>% get()


bind_rows(shp_list) %>% 
  mutate(RR = lapply(overall_ris,
                     function(m) m$prob_gt1[1:(nrow(m$id_space)/2)]) %>% 
           unlist()) %>% 
  ggplot()+
  geom_sf(aes(fill = RR), color = NA)+
  scale_fill_viridis_c()+
  theme_bw()+
  geom_sf(data = shapefile_comuni %>% 
            group_by(COD_REG)%>% 
            summarise(geometry = st_union(geometry)),
          fill = NA, color = "red") -> post_prob_Bym



ggsave(filename = paste0(results_path, "/","map_pp.png"), 
       plot = post_prob_Bym)


bind_rows(shp_list) %>% 
  mutate(RR = lapply(overall_ris,
                     function(m) exp(m[["id_space"]][["mean"]][1:(nrow(m$id_space)/2)])) %>% 
           unlist()) %>% 
  ggplot()+
  geom_sf(aes(fill = RR), color = NA)+
  theme_bw()+
  scale_fill_gradient2(low = "#023e8a", midpoint = 1, mid = "lightgrey", high = "#c1121f")-> p8



ggsave(filename = paste0(results_path, "/","map_RR.png"), 
       plot = p8)



## Zero nb  ----------------------------------------------------------------
#
#
#mod_zip = readRDS(
#  file = file.path("output",
#                   "models_zip",
#                   paste0("res_", eco, ".rds")))
#
#rbind(fn_grafico_dlnm_tmax(mod_zip) %>% 
#        mutate(mod = "ZNegBin"),
#      fn_grafico_dlnm_tmax(overall_ris[["2C"]]) %>% 
#        mutate(mod = "NegBin")) %>% 
#  ggplot(aes(x = temp,
#             y = RR,
#             ymin = RR_l,
#             ymax = RR_u))+
#  geom_line()+
#  geom_ribbon(alpha = .2, color = NA)+
#  theme_minimal()+
#  labs(x = "Daily maximum temperature (°C)") +
#  geom_hline(aes(yintercept = 1), linetype = "dashed")+
#  facet_wrap(vars(mod))
#
#
#mod$summary.hyperpar
#
#
#
## --------------------------------------------------------------------------------------------
## END FOR NOW --------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------
#
#
#
#
## extract the first derivative
#
#library(zoo)
#
#curve_slopes <- df_all_temp %>%
#  arrange(eco, lag, temp) %>%
#  group_by(eco, lag) %>%
#  mutate(
#    RR_smooth = rollmean(RR, 3, fill = NA, align = "center"),
#    slope = (RR_smooth - lag(RR_smooth)) /
#      (temp - lag(temp))
#  ) %>%  filter(slope < 0) %>%
#  slice_head(n = 1) # looks like after 10 weeks the most significant tirning point is heat
#
#
#
#
#
#
#
#
## Define function for Arte moderna --------------------------------------------
#
#extract_surface <- function(cp, eco){
#  
#  df <- expand.grid(
#    temp = cp$predvar,
#    lag  = seq(cp$lag[1], cp$lag[2], 1)
#  )
#  
#  df$RR  <- as.vector(exp(cp$matfit))
#  df$RR_l <- as.vector(exp(cp$matlow))
#  df$RR_u <- as.vector(exp(cp$mathigh))
#  
#  df$eco <- eco
#  
#  df
#}
#
#


plot.crosspred(cp,
               "slices",
               var   = c(p90),        # exposure value of interest
               col   = c("steelblue"),
               ylab  = "Relative Risk",
               xlab  = "Lag (weeks)",
               main  = "Lag-response at 90th percentile of Tmax"
)

p99 <- quantile(t_seq, 0.10, na.rm = TRUE)



plot(cp,
     "slices",
     var   = p99,        # exposure value of interest
     col   = "steelblue",
     ylab  = "Relative Risk",
     xlab  = "Lag (weeks)",
     main  = "Lag-response at 90th percentile of Tmax"
)



plot(cp,
     xlab  = "Tmax (°C)",
     zlab  = "Relative Risk",
     theta = 200,        # horizontal rotation angle
     phi   = 30,         # vertical rotation angle
     lphi  = 30,
     main  = "3D exposure-lag-response surface"
)

plot(cp,
     "contour",
     xlab    = "Tmax (°C)",
     ylab    = "Lag (weeks)",
     key.title = title("RR"),
     main    = "Exposure-lag-response contour"
)


lag_df <- data.frame(
  lag    = cp$lag,
  RR     = cp$matfit[p90, ],   # or use the numeric value
  RR_low = cp$matlow[p90, ],
  RR_high= cp$mathigh[p90, ]
)

ggplot(lag_df, aes(x = lag, y = RR)) +
  geom_line(colour = "tomato", linewidth = 0.8) +
  geom_ribbon(aes(ymin = RR_low, ymax = RR_high),
              alpha = 0.2, fill = "tomato") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  theme_minimal() +
  labs(
    x     = "Lag (weeks)",
    y     = "Relative Risk",
    title = "Lag-response at 90th percentile of Tmax"
  )





