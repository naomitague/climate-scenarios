

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
ui_grid_cells <- c(1,2)

getAllGridCells <- function(all_grid_cells) {
  
  for(i in seq_along(ui_grid_cells)) {
    
    cut_and_stitch(grid_number = all_grid_cells[i])
    
  }
  
}

getAllGridCells(all_grid_cells = ui_grid_cells)

