source("/Users/mallorygiesie/github/data-cleaning/packages.R")
source("/Users/mallorygiesie/github/data-cleaning/cal_adapt_4models_allscenarios.R")

find_df <- function(rcp, gcm, sample_cell) {
    valid_gcms <- c("MIROC5", "HadGEM2ES", "CanESM2", "CNRM_CM5")
    if (!gcm %in% valid_gcms) {
      stop("Invalid value for gcm. Allowed options are: ", paste(valid_gcms, collapse = ", "))
    }
    rcp <- as.numeric(rcp)
    if (!rcp %in% c(45, 85)) {
      stop("Invalid value for rcp. Only 45 or 85 are allowed.")
    }
    df_name <- paste(gcm, paste0("rcp", rcp), sep = "_")
    df <- get(df_name, envir = globalenv())
    
    var_names <- grep(paste0("_", sample_cell, "$"), names(df), value = TRUE)
    
    combined_df <- select(df, time, one_of(var_names)) %>% 
      rename_all(~gsub(paste0("_", sample_cell, "$"), "", .))
    
    return(combined_df)
  }

find_df(45, "MIROC5", 2)




build_runs <- function(duration, rcp, gcm, sample_cell, run_type, start_year,
                       sample_window, 
                       precip_upper, precip_lower, 
                       min_temp_upper, min_temp_lower,
                       max_temp_upper, max_temp_lower,
                       min_rh_upper, min_rh_lower,
                       max_rh_upper, max_rh_lower,
                       wind_upper, wind_lower) {
  #Initializing empty list for the final size 
  
}


