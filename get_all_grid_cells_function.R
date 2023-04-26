

# Erica's Code:
# Output from randomly_select.R
# NEED input of grid_number for naming
# Currently not automated to loop through all selected grids

######################## Stitch as new df ----------

# Cutting selections BY YEAR-------------------------
# Cut each year from the climate model dataframe and stitch together
# Next problem: how to loop through EACH grid cell and add grid cell number



### BY SEASON OR YEAR -----
cut_and_stitch <- function(series_selection = unlist(sample_grid_series), # list of the dates we want (either year or seasons)
                           grid_number, # this function will go through one grid_number at a time
                           rcp = 45,
                           gcm = "MIROC5") { # vic modified inputs: the last 2 will be user inputs
  
  # finds the gcm for each grid cell for the rcp and the gcm of the sample grid cell
  our_gcm <- find_df(gcm = gcm, rcp = rcp, sample_cell = grid_number) # vic modified. may want to relabel sample_cell in the find_df func
  
  # Empty data frame with column names
  col_names <- colnames(our_gcm)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Check if series_selection is by season or year
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save for the time frames for the sample cell
    for (i in series_selection) {
      new_cut <- our_gcm[which(our_gcm$water_year == i), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save for the time frames for the sample cell
    for (i in season_selection) {
      season_year <- strsplit(i, "_")
      wet_dry <- season_year[[1]][1]
      year <- season_year[[1]][2]
      new_cut <- our_gcm[which(our_gcm$season == wet_dry & our_gcm$water_year == year), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } # END IF
  
  
  
  # Name the grid cell file with model and grid number
  grid_name_formatted <- paste0(gcm, "_", rcp, "_grid_", grid_number) # vic modified
  
  # Assign the resulting timeseries to a variable in the global environment
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  #return(combined_cut_model) # vic modified - i'm thinking we dont need to return something if we're putting stuff into the global environment?
}




## Vics Code:

# inputs: 
# sample_cell: sampling grid cell, 
# all_grid_cells: all grid cells of interest. this comes from the shiny app selections after looking at the caladapt api map
# sample_grid_series: the years or seasons/years for the sample grid cell that were used in the random selection based off of the filtered criteria
# gcm: which model we used
# rcp: which emissions scenario we used

# 1) sees which model we're using and if we've stitched by seasons or years in the sampling grid cell and 
# 2) for each grid cell: grabs those same run_types during the same time periods and models for all the other grid cell data frames
# 3) for each grid cell: calls the stitch function

# outputs: 1 timeseries file per grid cell? 

# this will be user input in the shiny app:
ui_grid_cells <- c(1)

getAllGridCells <- function(all_grid_cells) {
  
  for(i in seq_along(ui_grid_cells)) {
    
    cut_and_stitch(grid_number = all_grid_cells[i])
    
  }
  
}

getAllGridCells(all_grid_cells = ui_grid_cells)






# FUNCTION -----
cut_stitch_ts <- function(series_selection = unlist(sample_grid_series),
                          grid_number, # this function will go through one grid_number at a time
                          rcp = 45,
                          gcm = "MIROC5",
                          start_date = as.Date("05/05/2020", format = "%m/%d/%Y")) { # grid_number is placeholder, 
  # will need to pull out from grid cells to name
  
  # finds the gcm for each grid cell for the rcp and the gcm of the sample grid cell
  model_selection <- find_df(gcm = gcm, rcp = rcp, sample_cell = grid_number) # vic modified. may want to relabel sample_cell in the find_df func
  
  # CREATE FOLDER -----
  grid_name_formatted <- paste0(gcm, "_", rcp, "_grid_", grid_number) # vic modified
  
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



# vics:
ui_grid_cells <- c(1)

getAllGridCells <- function(all_grid_cells) {
  
  for(i in seq_along(ui_grid_cells)) {
    
    cut_stitch_ts(grid_number = all_grid_cells[i])
    
  }
  
}

getAllGridCells(all_grid_cells = ui_grid_cells)













# erica's 3rd function
# FUNCTION -----
cut_stitch_ts <- function(model_selection = our_gcm, # dataframe will also need to loop per grid
                          series_selection = unlist(sample_grid_series),
                          start_date,
                          duration, # WHAT IS THIS CALLED
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
  combined_cut_model <- combined_cut_model[!(format(combined_cut_model$time, "%m-%d") == "02-29"), ] # WORKS
  
  # Add here a loop of every 4 years (days)
  start_year <- year(start_date)
  end_year <- year(start_date + duration)
  for (year in start_year:end_year) {
    if (leap_year(year)) 
      
      # Before/after date to average the variable columns
      before_date <- as.Date(paste0(year, "-02-28")) # WORKS
    after_date <- as.Date(paste0(year, "-03-01"))
    
    # Calculate row to insert
    insert_row <- data.frame(time = as.Date(paste0(year, "-02-29"))) # WORKS IF LEAP YEAR
    for (col in names(combined_cut_model[-1])) {
      
      insert_row[[col]] <- mean(c(combined_cut_model[combined_cut_model$time == before_date, col],
                                  combined_cut_model[combined_cut_model$time == after_date, col]))
    }
    combined_cut_model <- rbind(combined_cut_model, insert_row)
  }
  
  # Order dataframe by date for added leap years
  combined_cut_model <- combined_cut_model[order(combined_cut_model$time), ]
  
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
duration = 5
start_date <- as.Date("2006-01-01")
cut_stitch_ts(
  start_date = start_date,
  grid_number = 2,
  duration = duration)



