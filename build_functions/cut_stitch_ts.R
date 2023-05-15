#' Cut and Stitch Time Series
#' cut_stitch_ts()
#' Cut selected time periods, stitch as new data frame and convert to time series
#'
#' @param model_selection: loop through each grid cell, this is the base data frame to reference for time selections
#' @param series_selection: takes the sample_grid_series created from the randomly_select.R function
#' @param start_date 
#' @param df_names: from larger loop
#'
#' @return folder of time series files and a composite csv file
#' @export
#'

# Testing the filter used - located in the for loops for both year and season
# grid_cell_46896[format(as.Date(grid_cell_46896$time, format="%Y/%m/%d"), "%m-%d") >= format(as.Date(ui_start_date, format="%Y/%m/%d"),"%m-%d"), ]

cut_stitch_ts <- function(model_selection = ui_sample_cell,      # loop through dataframes of each grid cell
                          series_selection = unlist(sample_grid_series),
                          start_date = ui_start_date,
                          df_names,
                          root_folder)
{
  model_selection <- get(df_names, envir = globalenv()) 
  
  
  # CREATE FOLDER -----
  grid_name_formatted <- paste0(df_names, "_time_series")      # Create File/Folder Naming Convention
  folder_path <- file.path(root_folder, grid_name_formatted)  # For new .csv and time series files with shared name
  dir.create(folder_path, showWarnings = FALSE)
  
  # CREATE BLANK DATA FRAME ----
  col_names <- colnames(model_selection)       
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names   # Empty data frame with column names
  
  # CHECK DATE YYYY-MM-DD ----
  start_date <- as.Date(start_date, format = "%Y/%m/%d", origin = "1970-01-01")
  combined_cut_model$time <- as.Date(combined_cut_model$time , format = "%Y/%m/%d", origin = "1970-01-01")
  
  # CUT YEAR OR SEASON -----
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- model_selection[which(model_selection$water_year == i), ]  # Search matching time
      if (i == series_selection[1]) { # Filter the first sequence by the start date
        new_cut <- new_cut[format(as.Date(new_cut$time, format="%Y/%m/%d"), "%m-%d") >= format(as.Date(start_date, format="%Y/%m/%d"),"%m-%d"), ]
      }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    } # END YEARS LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      season_year <- strsplit(i, " ")
      year <- season_year[[1]][1]
      wet_dry <- season_year[[1]][2]
      new_cut <- model_selection[which(model_selection$season == wet_dry 
                                       & model_selection$water_year == year), ]
      if (i == series_selection[1]) {  # Filter the first sequence by the start date
        new_cut <- new_cut[format(as.Date(new_cut$time, format="%Y/%m/%d"), "%m-%d") >= format(as.Date(start_date, format="%Y/%m/%d"),"%m-%d"), ]
      }
      combined_cut_model <- rbind(combined_cut_model, new_cut) # Add this cut to the existing data frame for all cuts
    } # END SEASONS LOOP
    
  } # END IF
  
  
  # DEALING WITH UNORDERED LEAP YEAR -----
  combined_cut_model <- combined_cut_model[!(format(combined_cut_model$time, "%m-%d") == "02-29"), ] # Remove all February 29ths in the data frame

  combined_cut_model <- combined_cut_model %>% mutate(old_date = as.character(time)) # Add character of original date
  combined_cut_model <- combined_cut_model %>% select(-time) # Delete TIME column for next step to work
  
  leapyears <- seq(from = 1, to = nrow(combined_cut_model), by = 1460)   # sequence of 1460 rows (365 days * 4 years) to add a leap year row
  for(i in leapyears){    # For every 1460 days rows, add a row
    combined_cut_model <- combined_cut_model %>% 
      add_row(max_temp = mean(combined_cut_model$max_temp[i - 1], combined_cut_model$max_temp[i + 1]),
              precip = 0,
              max_humidity = mean(combined_cut_model$max_humidity[i-1], combined_cut_model$max_humidity[i+1]),
              min_humidity = mean(combined_cut_model$min_humidity[i-1], combined_cut_model$min_humidity[i+1]),
              wind = mean(combined_cut_model$wind[i-1], combined_cut_model$wind[i+1]),
              min_temp = mean(combined_cut_model$min_temp[i-1], combined_cut_model$min_temp[i+1]),
              water_year = combined_cut_model$water_year[i-1],
              season = combined_cut_model$season[i-1],
              .after = i)   # Set location of new row
  }
  
  # NEW DATE COLUMN -----
  num_rows <- nrow(combined_cut_model)
  dates <- seq(from = start_date, length.out = num_rows, by = "day")
  combined_cut_model$sequence_date <- dates
  
  # SAVING -----
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  write.csv(combined_cut_model,      # Save final data frame as a .csv
            file.path(folder_path, paste0(grid_name_formatted, ".csv")), 
            row.names = FALSE)

  
  # CONVERT TO TIME SERIES -----
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
  
  for (i in seq_along(file_type)) {
    # Construct file name with proper extension
    file_name <- file.path(folder_path, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%04d %02d %02d 1", year(start_date), month(start_date), day(start_date))
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write variable data to file, appending to top line
    write.table(combined_cut_model[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
  }
}

