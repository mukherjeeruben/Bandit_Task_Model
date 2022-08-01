fit_model <- function(raw_data_file){
  raw_data <- data.table::fread(file = paste0("clean_data/", raw_data_file, ".csv"),
                                header = TRUE, sep = ",",  data.table = TRUE, fill = TRUE, 
                                stringsAsFactors = TRUE, logical01 = FALSE)
  raw_data <- raw_data %>% drop_na()
  data_list <- frame2list(raw_data)
  fit <- stan(
    file = "stan_files/bandit2arm_delta.stan",
    data = data_list,
    iter = 4000,
    warmup = 1000,
    chains = 4
  )
  
  saveRDS(fit, file = paste0("fitted_data/", raw_data_file, "_fit.rda"))
  fit_extract <- rstan::extract(fit)
  output = list(raw_data = raw_data, list_data = data_list, fit = fit_extract)
  saveRDS(output, file = paste0("fitted_data/", raw_data_file, ".rda"))
  return(output)
}

frame2list <- function(new_voice_df){
  payscale = 100
  DT_trials <- new_voice_df[, .N, by = "subjID"]
  subjs     <- DT_trials$subjID
  n_subj    <- length(subjs)
  t_subjs   <- DT_trials$N
  t_max     <- max(t_subjs)
  
  # Initialize (model-specific) data arrays
  choice  <- array(-1, c(n_subj, t_max))   ###recycle -1 array n_subj rows and t_max columns
  #outcome_blue <- array( -1, c(n_subj, t_max))
  outcome <- array(-1, c(n_subj, t_max))
  #amount_blue <- array(0, c(n_subj, t_max))
  #amount_orange <- array(0, c(n_subj, t_max))
  
  # Write from raw_data to the data arrays
  for (i in 1:n_subj) {
    subj <- subjs[i]
    t <- t_subjs[i]
    DT_subj <- new_voice_df[new_voice_df$subjID == subj]
    
    choice[i, 1:t]  <- DT_subj$choice
    #outcome_blue[i, 1:t] <- DT_subj$outcome_blue
    outcome[i, 1:t] <- DT_subj$outcome
    #amount_blue[i, 1:t] <- DT_subj$amount_blue
    #amount_orange[i, 1:t] <- DT_subj$amount_orange
  }
  # Wrap into a list for Stan
  data_list <- list(
    N       = n_subj,
    T       = t_max,
    Tsubj   = t_subjs,
    choice  = choice,
    #outcome_blue = outcome_blue,
    outcome = outcome
    #amount_blue = amount_blue / payscale,
    #amount_orange = amount_orange / payscale
  )
  return(data_list)
}