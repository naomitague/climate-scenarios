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
                         climate_table = climate_table,
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
  
  # make an empty dataframe with all the columns we need
  metadata <- data.frame(matrix(nrow = 0, ncol = 15))
  colnames(metadata) <- c("run_type", "rcp", "col3", "col4", "col5", "col6", "col7", "col8", "col9", "col10", "col11", "col12", "col13", "col14", "col15")
  
  # add in all the columns we need
  
    # from the build_runs() function:
  metadata$rcp = rcp
    
  
    # from the climate table:
  #  run
  #  precip_lwr
  #  precip_upr
  #  mintemp_lwr
  #  mintemp_upr
  #  maxtemp_lwr
  #  maxtemp_upr
   # minrh_lwr
   # minrh_upr
   # maxrh_lwr
   # maxrh_upr
   # notes
  
  # download in the root folder
  
}
