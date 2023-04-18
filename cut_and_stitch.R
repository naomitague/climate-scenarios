# Output from randomly_select.R
# NEED input of grid_number for naming
# Currently not automated to loop through all selected grids

######################## Stitch as new df ----------

# Cutting selections BY YEAR-------------------------
# Cut each year from the climate model dataframe and stitch together
cut_and_stitch <- function(model_selection,
                           year_selection = unlist(sample_grid_series),
                           grid_number) {
  
  
  # Add year column to original data frame
  original_model <- model_selection %>%        # Placeholder model to manipulate in function
    mutate(year = lubridate::year(time))       # Need to be able to filter by year
  
  # Empty data frame with column names
  col_names <- colnames(original_model)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Loop through years, select range by time and cut and save
  for (i in year_selection) {
    new_cut <- original_model[which(original_model$year == i), ]
    
    # Add this cut to the existing dataframe for all cuts
    combined_cut_model <- rbind(combined_cut_model, new_cut)
    
  } # END LOOP
  
  # Name file with model and grid number
  model_name <- deparse(substitute(model_selection))
  grid_name_formatted <- paste0(str_to_title(model_name), "_grid", grid_number)
  
  # Assign the resulting list to a variable in the global environment
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  return(combined_cut_model)
}

# TEST
cut_and_stitch(model_selection = CanESM2_rcp45, 
                    year_selection = (sample_grid_series), 
                    grid_number = 3)





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
  cut_stitch_function(model_selection, sample_grid_series, grid_number = i)
}



