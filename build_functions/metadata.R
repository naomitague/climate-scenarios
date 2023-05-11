######### to add to workflow doc
# delete comment about needing to update units 
# add to user input data:
ui_caladapt_ind = 'Y' # mark as Y if using cal adapt data through this api. if uploading your own data, dont run this line
# delete "run" in criteria_list?
# ui_sample_cell will equal the NAME of the sample cell data frame (since this will be how shiny works) and then the base_data will equal the dataframe matching that name



# not ready yet but will output as stated in the technical documentation draft: https://docs.google.com/document/d/1G1UKEErrcdLWn4X61f-iy2ByZO3PCXRP/edit#heading=h.psoig0rfj0i9 AND maybe add the centroids of the cal adapt grid cells?
# also add what data each run is built off of (aka output from the randomly select and the number from the find matches output)

# and download itself in the root folder of the data download

# download itself in the root folder of the data download

get_metadata <- function() {
  
  # still need: all_cells, season definitions, scenario duration, available samples, desired_samples, historical_extremes included y/n, calibration included y/n
  
  # make a dataframe with the number of rows that we need which should match the number of runs built in the climate_vars_list
  climate_vars_df <- do.call(rbind, criteria_list) 

  # add in all the columns we need
  metadata <- climate_vars_df %>% 
    mutate(gcm_rcp = ifelse(
      ui_caladapt_ind == 'Y', paste0(ui_gcm, "_", ui_rcp), 'user input data')) %>% 
    mutate(sample_cell = names(ui_sample_cell)) %>% 
    mutate(all_cells = paste(ui_grid_cells, collapse = ",")) %>% # need to add
    #mutate(dry_season_def = '???') %>% # need to add
    #mutate(wet_season_def = '???') %>% # need to add
    mutate(scenario_start_date = ui_start_date) %>% 
    mutate(segment_type = ui_segment_type) %>% 
    #mutate(scenario_duration = 5) %>% # need to add
    mutate(sample_window = ui_sample_window) %>% # need to add
    merge(number_matches) %>% 
   # mutate(avail_samples = 10) %>% # need to add
    mutate(historical_extremes_incl = 'N') %>% # need to add
    mutate(calibration_included = 'N') # need to add
  
  View(metadata)

  # download in a root folder? or all folders?
  
}

get_metadata()
