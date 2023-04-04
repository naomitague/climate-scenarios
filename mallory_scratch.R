source("/Users/mallorygiesie/github/data-cleaning/packages.R")
source("/Users/mallorygiesie/github/data-cleaning/cal_adapt_4models_allscenarios.R")


# Generate arrays with random values between the specified percentiles
precip_upper <- runif(20, 0.8, 1)
precip_lower <- runif(20, 0, 0.2)
min_temp_upper <- runif(20, 0.5, 1)
min_temp_lower <- runif(20, 0, 0.5)
max_temp_upper <- runif(20, 0.5, 1)
max_temp_lower <- runif(20, 0, 0.5)
min_rh_upper <- runif(20, 0.5, 1)
min_rh_lower <- runif(20, 0, 0.5)
max_rh_upper <- runif(20, 0.5, 1)
max_rh_lower <- runif(20, 0, 0.5)
wind_upper <- runif(20, 0.5, 1)
wind_lower <- runif(20, 0, 0.5)


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

#find_df(45, "MIROC5", 2)

#Creating a table to read into the function

read_table <- function() 
  
build_runs <- function(rcp,
                        gcm,
                        sample_cell,
                        run_type,
                        start_date,
                        duration,
                        climate_table
                        # min_rh_upper = NULL,
                        # min_rh_lower = NULL,
                        # max_rh_upper = NULL,
                        # max_rh_lower = NULL,
                        # wind_upper = NULL,
                        # wind_lower = NULL
                       ) 
                       {
  run_samples <- vector("list", duration) #Initializing empty list where we'll put our samples 
  df <- find_df(rcp, gcm, sample_cell) #finding the correct gcm + rcp dataframe
  start_date <- as.Date(start_date, format = "%m/%d/%Y") #specifying start date format
  
  start <- (year(start_date) - sample_window) #first year lower sample window
  end <- (year(start_date) - sample_window) #last year higher sample window
  
  # Check if sample_window argument is not NULL
  if (!is.null(sample_window)) {
    # filter the data frame based on the start and end dates
    df <- df %>%
      filter(
        year(time) >= start) %>%
      filter(year(time) <= end)
  } else {
    df <- df
    warning(
      "sample_window argument is NULL. Specify sampling window, or else we'll pull from 1950-2100."
    )
  }
}

build_runs(rcp = 45, gcm = "MIROC5", sample_cell = 2, run_type = 'year',
           start_date = '01/01/2000', duration = 10, sample_window = 20)











