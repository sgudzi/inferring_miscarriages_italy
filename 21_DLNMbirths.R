# ------------------------------------------------------------------------------

# 08.09.2025 aggiornato per includere ecoregioni, includiamo anche le nascite dalle 24 settimane di gestazione in poi 
# "2025-09-11 08:30:03 BST : aggiornato per selezionare la range dei lag da usare 

# 15.12.2025 Aggiornato per sensitivity analysis
# 17.12.2025 Aggiornato per girare su ecoregioni
# 19.12.2025 Aggiornato per girare su ecoregioni
# 10.02.2026 Aggiornato per aggiungere una nuova formula (line 82:95)


# Load data

outcome_exposure_lead <- readRDS(paste0(dir.data,"/outcome_exposure_lead.RDS"))


#!! define the following: outcome_exposure_lead

# ensure it is scaled 



# Filter Dataset by ecoregione ---------------------------------------------------------------------


ecoregioni_list<- unique(outcome_exposure_lead$Ecoregioni)


for (i in ecoregioni_list) {
  
  ecoregione <- ecoregioni_list[1]
  
  outcome_exposure_ecoregione <- outcome_exposure_lead %>% 
    filter(Ecoregioni == ecoregione) %>% 
    drop_na() %>% 
    slice_sample(n = 200)
  
    gc()    
#      filter(ecoregioni_sezioni == as.character(ecoregione))
    

    
    # Create directory to save models  ---------------------------------------------------------------------
    
    dir.mod.out <- paste0(dir.output, "/models_",  ecoregione)
    dir.create(dir.mod.out)
    
    # Parameters ---------------------------------------------------------------------
    

    # temp knots definition  ---------------------------------------------------------
    
    knots_temp_50_90 <-  c(quantile(outcome_exposure_ecoregione$cop_lead1, c(0.50, 0.90)))
    knots_temp_25_75 <-  c(quantile(outcome_exposure_ecoregione$cop_lead1, c(0.25, 0.75)))

    
    # lag knots definition ----------------------------------------------------------- 
    
    knots_lag_0_22      <-  c(5, 15)
    knots_lag_log_0_22  <-  logknots(knots_lag_0_22, 3)
    knots_lag_minus4_22 <-  c(5, 15)
    knots_lag_minus4_22 <-  c(0, 8, 16)
    
    
    # penalised complexity priors --------------------------------------------- 
    
    
    hyper.iid <- list(theta = list(prior = "pc.prec", param = c(0.1, 0.01)))
    hyper.bym <- list(theta1 = list("PCprior", c(0.1, 0.01)),
                      theta2 = list("PCprior", c(0.5, 0.5)))
    
    
    # seasonality via sine and cosine ----------------------------------------- 
    
    sin_i <- scale(sin((2*pi*outcome_exposure_ecoregione$id_week)/53))[,1]
    cos_i <- scale(cos((2*pi*outcome_exposure_ecoregione$id_week)/53))[,1]
    
    # seasonality via sine and cosine ----------------------------------------- 

  formula_idf <- =  as.formula(paste( #EC formula IDF 10.02.2026
        "n ~ 1 + 
    year_number +
    sin_i + cos_i +
    POP_DEPEN_INDEX_ADJ + EMPL_RATE_20_64 +  INDEX_ACCES_ESSENT_SERVICES +", #EC spatial deprivation covariates 10.02.2026
        paste(c(cb_temp$cols_cb), collapse = " + ")))


  
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    # --------------------- SELECTION -------------------------------------------------------------------------------------------------------------
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    
    # important to define lag structure here, because that is where the knots will be defined. 
    
    lower_lag <- 0
    upper_lag <- 22
    
    # temp lead temp matrix ---------------------------------------------------
    
    # a. select exposure matrix
    lead_temp_matrix_temp <- outcome_exposure_ecoregione %>%
      select(starts_with("cop_lead")) %>%
      as.matrix()
    
    # b. ensure its within min max lags
    lead_temp_matrix_temp[,(lower_lag + 1):(upper_lag + 1)] -> lead_temp_matrix_temp
    
    
    
    
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    
    
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    # function starts here ------------------------------------------------------------------------------------------------------------------------
    # ---------------------------------------------------------------------------------------------------------------------------------------------
    
    # ----------------------------------------------------
    # 1. Function for Cross-basis temperature–lag function: extract_cb()
    # ----------------------------------------------------
    
    
    extract_cb <- function(lead_temp_matrix, argvar, arglag, label) {
      
      # build crossbasis
      cb <- crossbasis(
        x      = lead_temp_matrix,
        argvar = argvar,
        arglag = arglag
      )
      
      # extract cb column names
      cols_cb <- grep("^v[0-9]+\\.l[0-9]+$", colnames(cb), value = TRUE)
      
      # add prefix to both colnames and extracted names
      colnames(cb) <- paste0(label, "_", colnames(cb))
      cols_cb     <- paste0(label, "_", cols_cb)
      
      # return
      list(cb = cb, cols_cb = cols_cb)
    }
    
    
    # Call function for making crossbasis -----------------
    # crossbasis
    
    knots_lag_0_22
    
    
    cb_temp_1 <- extract_cb(
        lead_temp_matrix = lead_temp_matrix_temp,
        argvar = list(fun = "ns", knots = knots_temp_50_90),
        arglag = list(fun = "ns", knots = knots_lag_0_22),
        label  = "temp"
    )
    
    cb_temp_2 <- extract_cb(
      lead_temp_matrix = lead_temp_matrix_temp,
      argvar = list(fun = "ns", knots = knots_temp_25_75),
      arglag = list(fun = "ns", knots = knots_lag_0_22),
      label  = "temp"
    )
    
    cb_temp_3 <- extract_cb(
      lead_temp_matrix = lead_temp_matrix_temp,
      argvar = list(fun = "ns", knots = knots_temp_50_90),
      arglag = list(fun = "ns", knots = knots_lag_log_0_22),
      label  = "temp"
    )
    
    cb_temp_4 <- extract_cb(
      lead_temp_matrix = lead_temp_matrix_temp,
      argvar = list(fun = "ns", knots = knots_temp_25_75),
      arglag = list(fun = "ns", knots = knots_lag_log_0_22),
      label  = "temp"
    )
    
    # ----------------------------------------------------
    # 2. Update formula, Run model and extract model perf. 
    # ----------------------------------------------------
    
    fit_model <- function(formula, 
                          name_mod, #name by which the model will be saved
                          cb, 
                          min_GA, 
                          lowerlag = lower_lag, 
                          upperlag = upper_lag, 
                          leadtempmatrix = lead_temp_matrix_temp, 
                          var_knots,
                          lag_knots, 
                          arg_var,
                          arg_lag
                          ) {
      
      # 1. Update formula
      f <- formula
      
      # 2. Time the model fit
      start_time <- Sys.time()
      
      # 3. Fit the model -------------------------------------
      
      mod <- inla( 
        formula = f, 
        family = "0poisson", #EC I updated to zero inflated poisson
        data = cbind(outcome_exposure_ecoregione, cb), 
        offset = log(total_pop), 
        control.predictor = list(link = 1), 
        control.compute   = list(waic = TRUE, 
                                 cpo = TRUE, config = FALSE), 
        num.threads       = 10 
      )
      run_time <- Sys.time() - start_time
      
      # extract the matrix
      
     # matrix_temperature <- extract_matrix(
     #   mod_fixed_effects = mod$summary.fixed, # okay
     #   label = name_mod, 
     #   cb = cb, 
     #   lead_matrix = leadtempmatrix, 
     #   lower_lag = model$lowerlag, 
     #   upper_lag = model$upperlag, 
     #   argvar = arg_var,
     #   arglag = arglag)
     # 
      
      model_save <- list()

      model_save$runtime <-run_time
      model_save$cb <- cb 
      model_save$f <- f
      model_save$lower_lag <- lowerlag
      model_save$upper_lag <- upperlag
      model_save$lead_temp_matrix <- leadtempmatrix 
      model_save$varknots <- var_knots
      model_save$lagknots <- lag_knots
      model_save$lcpo <- sum(log(mod$cpo$cpo))
      model_save$waic <- mod$waic$waic
      model_save$cpu <- mod$cpu.used[4]
      model_save$hyperpar <- summary(mod)$hyperpar
      model_save$argvar <- arg_var 
      model_save$arglag <- arg_lag 
      model_save$fixed <- summary(mod)$fixed
      model_save$summary_fixed <- mod$summary.fixed
   #   model_save$M <- M
   #   model_save$M_ub <- M_ub
   #   model_save$M_lb <- M_lb
   #   model_save$temp_seq <- temp_seq
   #   model_save$lag_seq <- lag_seq
   #   model_save$rr_matrix <- rr_matrix
   #   model_save$rr_matrix_lb <- rr_matrix_lb
   #   model_save$rr_matrix_ub <- rr_matrix_ub

      
      saveRDS(model_save, 
              paste0(dir.mod.out, "/", name_mod))
      
      saveRDS(mod, 
              paste0(dir.mod.out, "/inla_", name_mod))
      
      
      # 5. Return results (both table + model object if needed)
      return(model_save)

    }
    
    # ----------------------------------------------------------------------------
    # call models ----------------------------------------------------------------
    # ----------------------------------------------------------------------------
    
    
    fit_model( #dummy test to see if it works 
      name_mod = "mod_0", #!!make it unique as this will determine your file name
      min_GA = min_GA,
      formula =  as.formula(paste(
        "n ~ 1 + year_number + sin_i + cos_i + ",
        paste(c(cb_temp$cols_cb), collapse = " + "))),
      cb = cbind(cb_temp_1$cb), 
      var_knots =  attr(cb_temp_1$cb,"argvar")$knots, 
      lag_knots =  attr(cb_temp_1$cb,"arglag")$knots, 
      arg_var = list(fun = "ns", knots = knots_temp_50_90),
      arg_lag = list(fun = "ns", knots = knots_lag_0_22)
    )
    

    fit_model(
      name_mod = "mod_1", #!!make it unique as this will determine your file name
      min_GA = min_GA,
      formula =  as.formula(paste(
        "n ~ 1 + year_number + sin_i + cos_i + f(id_space, model = 'bym2', graph = g_lazio, hyper = hyper.bym, scale.model = TRUE) +",
        paste(c(cb_temp$cols_cb), collapse = " + "))),
      cb = cbind(cb_temp_1$cb), 
      var_knots =  attr(cb_temp_1$cb,"argvar")$knots, 
      lag_knots =  attr(cb_temp_1$cb,"arglag")$knots
    )
    
    
    fit_model(
      name_mod = "mod_2", #!!make it unique as this will determine your file name
      min_GA = min_GA,
      formula =  as.formula(paste(
        "n ~ 1 + year_number + f(week, model = 'rw2',  hyper = hyper.iid, scale.model = TRUE)  + f(id_space, model = 'bym2', graph = g_lazio, hyper = hyper.bym, scale.model = TRUE) +",
        paste(c(cb_temp$cols_cb), collapse = " + "))),
      cb = cbind(cb_temp_1$cb)
    ) 
    
    
    fit_model(
      name_mod = "mod_3", #!!make it unique as this will determine your file name
      min_GA = min_GA,
      formula =  as.formula(paste(
        "n ~ 1 + f(year_number, model = 'iid', hyper = hyper.iid, scale.model = TRUE) + f(week, model = 'rw2', hyper = hyper.iid, scale.model = TRUE)  + f(id_space, model = 'bym2', graph = g_lazio, hyper = hyper.bym, scale.model = TRUE) +",
        paste(c(cb_temp$cols_cb), collapse = " + "))),
      cb = cbind(cb_temp_1$cb)
    ) 
    
    
    fit_model(
      name_mod = "mod_4", #!!make it unique as this will determine your file name
      min_GA = min_GA,
      formula =  as.formula(paste(
        "n ~ 1 + f(year_number, model = 'iid', hyper = hyper.iid, scale.model = TRUE) + f(week, model = 'rw2',  hyper = hyper.iid, scale.model = TRUE) + f(id_space, model = 'bym2', graph = g_lazio, hyper = hyper.bym, scale.model = TRUE,) +",
        paste(c(cb_temp$cols_cb), collapse = " + "))),
      cb = cbind(cb_temp_1$cb)
    ) 
    
}

#  ------------- Sanity check ------------------------------------------------------------

# matrix_temperature <- extract_matrix(
#   mod_fixed_effects = model_test$summary_fixed, 
#   label = "model_test", 
#   cb = model_test$cb, 
#   lead_matrix = model_test$lead_temp_matrix, 
#   lower_lag = model_test$lower_lag, 
#   upper_lag = model_test$upper_lag, 
#   argvar = model_test$argvar,
#   arglag = model_test$arglag)
#      



