# input:
  # 


# desired output:
# for each run:
  # lower percentile for climate variable
  # upper percentile for climate variable
  # notes
  # gcm
  # rcp
  # run type (season or year)
  # dry season definition
  # wet season defintion
  # scenario start date
  # scenario duration
  # sampling window
  # sample cell
  # all cells
  # available resamples
  # desired resamples
  # historical extremes included (Y/N)
  # calibration included (Y/N)

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
