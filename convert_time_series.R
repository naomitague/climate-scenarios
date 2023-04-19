###################
# Convert to time series
###################

# WIKI has info on naming files
# how to pull out NAME from cut_and_stitch to use as input here instead of our_gcm

# Mallory's date output: start_date

convert_time_series <- function(start_year, start_month, start_day, grid_name_formatted, our_gcm = our_gcm) {
  
  # Create folder for time series files with shared name
  folder_name <- paste0(grid_name_formatted, "_time_series")
  dir.create(folder_name, showWarnings = FALSE)
  
  ##### LEAP YEAR #################
  # ADD loop with start_date to add in/delete leap year day
  # if statement February = 29 and years match leap year
  # if year / 4 remainder = 0, then leap year
  # if input year =/ expected leap year then....
  # Cut out a day or add a day for leap year
  # IF extra day, delete Feb 29
  # IF short a day, repeat the previous day or average between previous/following day on Feb 29
  
  
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  #col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
  col_names <- c("max_temp_1", "min_temp_1", "precip_1", "max_humidity_1", "min_humidity_1", "wind_1")
  #col_names <- c("max_temp_2", "min_temp_2", "precip_2", "max_humidity_2", "min_humidity_2", "wind_2")
  # Currently multiple col_names because of the format of the dataframes, but ultimate plan is to separate by grid so they will be without the _num
  
  for (i in seq_along(file_type)) {
    # Construct file name with ".tmax" extension
    file_name <- file.path(folder_name, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%d %d %d 1", start_year, start_month, start_day)
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write max temperature data to file, appending to top line
    write.table(our_gcm[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
    
  }
}


# TEST
convert_time_series(1990, 08, 14, "new_name", our_gcm = our_gcm)



####################
# Loop through grid cells
####################

# Loop through each Grid --------------------

# save selected grid cells
# for each grid cell....
# Loop through each of sample_grid_series for each of the selected grids in cut_stitch_function
# Loop through convert_time_series for each grid cell

# need a function to save the selected_grids
selected_grids <- list()

for(i in seq_along(selected_grids)) {
  
  grid_number <- i
  grid_name_formatted <- paste0("grid_", grid_number)
  
  # loop through cut and stitch
  cut_and_stitch(our_gcm = our_gcm, 
                 year_selection = year_selection,
                 grid_number = grid_number)
  
  # loop through converting time series
  convert_time_series(start_year = start_year,
                      start_month = start_month,
                      start_day = start_day,
                      grid_name_formatted = grid_name_formatted,
                      our_gcm = our_gcm)
}
