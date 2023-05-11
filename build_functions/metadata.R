# not ready yet but will output as stated in the technical documentation draft: https://docs.google.com/document/d/1G1UKEErrcdLWn4X61f-iy2ByZO3PCXRP/edit#heading=h.psoig0rfj0i9 AND maybe add the centroids of the cal adapt grid cells?
# also add what data each run is built off of (aka output from the randomly select and the number from the find matches output)

# and download itself in the root folder of the data download

# download itself in the root folder of the data download

get_metadata <- function(run_type = run_type,
                         rcp = rcp, 
                         gcm = our_gcm,
                         climate_vars_list = climate_vars_list,
                         sample_cell = sample_cell,
                         all_cells,
                         dry_season_def,
                         wet_season_def,
                         scenario_start_date = start_date,
                         scenario_duration,
                         sample_window = sample_window,
                         avail_samples,
                         desired_samples,
                         historical_extremes_incl,
                         calibration_included = 'N'
                         ) {
  
  # still need: all_cells, season definitions, scenario duration, available samples, desired_samples, historical_extremes included y/n, calibration included y/n
  
  # make a dataframe with the number of rows that we need which should match the number of runs built in the climate_vars_list
  climate_vars_df <- do.call(rbind, climate_vars_list) 

  # add in all the columns we need
  metadata <- climate_vars_df %>% 
    mutate(run_type = run_type) %>% # this should be fixed with mal's push
    mutate(rcp = rcp) %>% 
    mutate(gcm = gcm) %>% 
    mutate(all_cells = '???') %>% # need to add
    mutate(dry_season_def = '???') %>% # need to add
    mutate(wet_season_def = '???') %>% # need to add
    mutate(scenario_start_date = start_date) %>% 
    mutate(scenario_duration = 5) %>% # need to add
    mutate(sample_window = 50) %>% # need to add
    mutate(avail_samples = 10) %>% # need to add
    mutate(desired_samples = 5) %>% # need to add
    mutate(historical_extremes_incl = 'N') %>% # need to add
    mutate(calibration_included = 'N')# need to add
  
  View(metadata)

  # download in a root folder? or all folders?
  
}
