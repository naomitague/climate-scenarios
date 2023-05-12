# ui_sample_cell will equal the NAME of the sample cell data frame (since this will be how shiny works) and then the base_data will equal the dataframe matching that name



# not ready yet but will output as stated in the technical documentation draft: https://docs.google.com/document/d/1G1UKEErrcdLWn4X61f-iy2ByZO3PCXRP/edit#heading=h.psoig0rfj0i9 AND maybe add the centroids of the cal adapt grid cells?
# also add what data each run is built off of (aka output from the randomly select and the number from the find matches output)

# and download itself in the root folder of the data download

# download itself in the root folder of the data download

get_metadata <- function() {
  
  # still need: all_cells, season definitions, scenario duration, available samples, desired_samples, historical_extremes included y/n, calibration included y/n
  
  # make a dataframe with the number of rows that we need which should match the number of runs built in the climate_vars_list
  climate_vars_df <- do.call(rbind, criteria_list) 

  # add in all the columns we need and save metadata to the global environment
  metadata <<- climate_vars_df %>% 
    mutate(gcm_rcp = ifelse(
      ui_caladapt_ind == 'Y', paste0(ui_gcm, "_", ui_rcp), 'NA: user input data')) %>% 
    mutate(sample_cell = ui_sample_cell) %>% 
    mutate(all_cells = paste(ui_grid_cells, collapse = ",")) %>% # need to add
    mutate(dry_season_def = 'not functional') %>% # need to add
    mutate(wet_season_def = 'not functional') %>% # need to add when we know more about this
    mutate(scenario_start_date = ui_start_date) %>% 
    mutate(segment_type = ui_segment_type) %>% 
    mutate(scenario_duration = ifelse(
      ui_segment_type == 'year', # if the user is building years
      length(criteria_list), # there are as many years as there are climate criteria
      round(length(criteria_list)/4,2))) %>% # otherwise the user is building by season so we divide by 4
    mutate(sample_window = ui_sample_window) %>%
    merge(number_matches) %>% 
    mutate(historical_extremes_incl = 'N') %>% # not yet functional
    mutate(calibration_included = 'N') # not yet functional

  # also save the file in the root folder
  write.csv(metadata, "all_grid_cell_output/metadata.csv", col.names = F)
  
}
