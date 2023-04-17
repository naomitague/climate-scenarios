# Mallory's Output:  buildruns.RMD
# run_samples list of arrays
# 1{[years/seasons that match]} 2{[years/seasons that match]} 3{[years/seasons that match]}



###################### Randomly select -----------------
# Randomly select one of each option
# Save as new list sample_grid_series
# -> Will be input to Victoria's function for other grid cell loop


# Using Mallory's output
run_samples <- list

# Turn into function ---------
randomly_select <- function(run_samples) {
  
  # Create blank list for randomly selected elements
  sample_grid_series <- list()
  
  # Randomly select one element of each list
  for (i in seq_along(run_samples)) {
    if (length(run_samples[[i]]) == 1) {
      sample_grid_series[[i]] <- run_samples[[i]]
    } 
    else {     # if only one option, select it (added this to fix random error)
      sample_grid_series[[i]] <- sample(run_samples[[i]], size = 1, replace = TRUE)
    }
  }
  
  # Assign the resulting list to a variable in the global environment
  assign("sample_grid_series", sample_grid_series, envir = .GlobalEnv)
  
  # Return resulting list
  return(sample_grid_series)
  
}

# TEST
randomly_select(run_samples)





######################## Stitch as new df ----------

# BY YEAR WORKS! -------------------------
# Cut each year from the climate model dataframe and stitch together
cut_stitch_function <- function(model_selection, 
                                year_selection = unlist(sample_grid_series),
                                grid_number) {
  
  
  # 1. Add year column to original data frame
  original_model <- model_selection %>%
    mutate(year = lubridate::year(time))       # Need to be able to filter by year
  
  # Empty data frame with column names
  col_names <- colnames(original_model)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # 2. Loop through years, select range by time and cut and save
  for (i in year_selection) {
    new_cut <- original_model[which(original_model$year == i), ]
    
    # 3. Add this cut to the existing dataframe for all cuts
    combined_cut_model <- rbind(combined_cut_model, new_cut)   # Double arrow to update global model

  } # END LOOP
  
  # Name file with model and grid number
  model_name <- deparse(substitute(model_selection))
  grid_name_formatted <- paste0(str_to_title(model_name), "_grid", grid_number)
  
  # Assign the resulting list to a variable in the global environment
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  return(combined_cut_model)
}

# TEST
cut_stitch_function(model_selection = CanESM2_rcp45, 
                    year_selection = (sample_grid_series), 
                    grid_number = 3)










# BY SEASON ------ Not set up yet

# Predefine seasons
months <- 1:12
seasons <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
             "Summer", "Summer", "Fall", "Fall", "Fall", "Winter")
season_data <- data.frame(month = months, season = seasons)

# stitch first season/year

cut_stitch_function <- function(model_selection, season_selection = NULL, year_selection) {
  
  # 1. Add season and year column to original data frame
  # Question about model dataframe structure: will it have month/year/season columns??
  original_model <- model_selection %>%
    mutate(year = lubridate::year(time)) %>%        # Need to be able to filter by year
    mutate(month = lubridate::month(time))          # Pull out the numeric month from a model
  original_model <- left_join(original_model, season_data, by = "month")    # Join seasonal column
  
  # 2. Select range by time and cut, save to new model
  new_cut <- original_model[which(original_model$season == season_selection &
                                    original_model$year == year_selection), ]
  
  # 3. Add this cut to the existing dataframe for all cuts
  combined_cut_model <- rbind(combined_cut_model, new_cut)   # Double arrow to update global model
  
  # End loop
  # Autonaming of file??
  newname <<- combined_cut_model
}



####################
# Loop through grid cells
####################

# Loop through each Grid --------------------

# save selected grid cells
# for each grid cell....
# Loop through each of sample_grid_series for each of the selected grids

# need a function to save the selected_grids
selected_grids <- list()

for(i in selected_grids) {
  cut_stitch_function(selected_model, sample_grid_series)
}



###################
# Convert to time series
###################

write_max_temp_data <- function(start_year, start_month, start_day, grid_name_formatted, model_selection) {
  
  # Construct file name with ".tmax" extension
  file_name <- paste0(grid_name_formatted, ".tmax")
  
  # Construct top line of file
  top_of_file <- sprintf("%d %d %d 1", start_year, start_month, start_day)
  
  # Write top line of file
  write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
  
  # Write max temperature data to file, appending to top line
  write.table(model_selection$max_temp_1, file = file_name, row.names = F, col.names = F, quote = F, append = T)
  
}

# TEST
write_max_temp_data(1990, 08, 14, grid_name_formatted, CanESM2_rcp45)

######################## LEAP YEARS -------
# Leap Years
# Selected start date
# Cut out a day or add a day for leap year???





