# Output from randomly_select.R
# NEED input of grid_number for naming
# Currently not automated to loop through all selected grids

######################## Stitch as new df ----------

# Cutting selections BY YEAR-------------------------
# Cut each year from the climate model dataframe and stitch together
# Next problem: how to loop through EACH grid cell and add grid cell number



### BY SEASON OR YEAR -----
cut_and_stitch <- function(our_gcm = our_gcm,
                           series_selection = unlist(sample_grid_series),
                           grid_number) { # grid_number is placeholder, will need to pull out from grid cells to name
  
  # Empty data frame with column names
  col_names <- colnames(our_gcm)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Check if series_selection is by season or year
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- our_gcm[which(our_gcm$water_year == i), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in season_selection) {
      season_year <- strsplit(i, "_")
      wet_dry <- season_year[[1]][1]
      year <- season_year[[1]][2]
      new_cut <- our_gcm[which(our_gcm$season == wet_dry & our_gcm$water_year == year), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } # END IF
  
  
  
  # Name file with model and grid number
  model_name <- deparse(substitute(our_gcm))
  grid_name_formatted <- paste0(str_to_title(model_name), "_grid_", grid_number)
  
  # Assign the resulting list to a variable in the global environment
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  return(combined_cut_model)
}



# TEST
cut_and_stitch(our_gcm = our_gcm, 
               year_selection = (sample_grid_series), 
               grid_number = 3)