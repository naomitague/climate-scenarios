#####################
# COMBINED: Stitch as new df and convert to time series ----------
#####################

# NEED TO DO -----
# Naming the file by the original selected model name
# NEED input of grid_number for naming
# Automate to loop through all selected grids

# INPUTS -----
# our_gcm from build_runs.R function
# sample_grid_series from randomly_select.R function
# start_date
# grid_number from larger loop, still need to figure out saving this list to loop through



# FUNCTION -----
cut_stitch_ts <- function(model_selection = our_gcm, # dataframe will also need to loop per grid
                          series_selection = unlist(sample_grid_series),
                          start_date,
                          grid_number) { # grid_number is placeholder, 
                                         # will need to pull out from grid cells to name
  
  # CREATE FOLDER -----
  # Name files with model and grid number
  model_name <- deparse(substitute(model_selection))  # NEED TO PULL OUT ORIGINAL NAME
  grid_name_formatted <- paste0(str_to_title(model_name), "_grid_", grid_number)
  
  # For new .csv and time series files with shared name
  folder_name <- paste0(grid_name_formatted, "_time_series")
  dir.create(folder_name, showWarnings = FALSE)
  
  
  # CUT AND STITCH -----
  # Empty data frame with column names
  col_names <- colnames(model_selection)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Check if series_selection is by year or season_year
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- model_selection[which(model_selection$water_year == i), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      season_year <- strsplit(i, "_")
      wet_dry <- season_year[[1]][1]
      year <- season_year[[1]][2]
      new_cut <- model_selection[which(model_selection$season == wet_dry 
                                       & model_selection$water_year == year), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } # END IF
  
  
  # DEALING WITH UNORDERED LEAP YEAR -----
  # Remove all February 29ths in the dataframe
  combined_cut_model <- combined_cut_model[!(format(combined_cut_model$date, "%m-%d") == "02-29"), ]
  
  # Add here a loop of every 4 years (days)
  start_year <- year(start_date)
  end_year <- year(start_date + duration) #### WHAT IS DURATION CALLED?
  for (year in start_year:end_year) {
    if (leap_year(year)) # CHECK IF THIS IS RIGHT SETUP
      
      # Before/after date to average the variable columns
      before_date <- as.Date(paste0(year, "02-28"))
      after_date <- as.Date(paste0(year, "03-01"))
      
      # Calculate row to insert
      insert_row <- data.frame(date = as.Date(paste0(year, "02-29")))
      for (col in names(combined_cut_model[-1])) {
        
        insert_row[[col]] <- mean(c(combined_cut_model[combined_cut_model$date == before_date, col],
                                    combined_cut_model[combined_cut_model$date == after_date, col]))
      }
      combined_cut_model <- rbind(combined_cut_model, insert_row)
  }
  
  # Order dataframe by date for added leap years
  combined_cut_model <- combined_cut_model[order(combined_cut_model$date), ]
  
  # Save final dataframe as a .csv
  write.csv(combined_cut_model, 
            file.path(folder_name, paste0(grid_name_formatted, ".csv")), 
            row.names = FALSE)
  
  
  
  # CONVERT TO TIME SERIES -----
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")

  
  for (i in seq_along(file_type)) {
    # Construct file name with proper extension
    file_name <- file.path(folder_name, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%d %d %d 1", year(start_date), month(start_date), day(start_date))
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write variable data to file, appending to top line
    write.table(combined_cut_model[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
  }
}


# TEST
start_date <- as.Date("05/05/2020", format = "%m/%d/%Y")