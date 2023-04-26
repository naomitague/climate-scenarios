#' Build Runs Function
#'
#' This function builds runs of filtered data frames based on provided parameters.
#'
#' @param rcp Integer specifying the Representative Concentration Pathway (RCP) scenario.
#' @param gcm Character specifying the General Circulation Model (GCM).
#' @param sample_cell Integer specifying the sample cell.
#' @param our_gcm Optional data frame to use as input. If provided, other arguments (rcp, gcm, sample_cell) will be ignored.
#' @param run_type Character specifying the type of runs to build. Either "year" or "season".
#' @param start_date Character specifying the start date for building the runs in format "MM/DD/YYYY".
#' @param duration Integer specifying the duration of runs in years.
#' @param climate_table List of data frames containing climate variable information for filtering.
#' @param sample_window Function to generate sample window of data.
#'
#' @return A list of data frames that match their criteria, one for each run in the climate_table, ready to be used in the stitches function.
#'
#' @examples
#' build_runs(
#'   rcp = 45,
#'   gcm = "MIROC5",
#'   sample_cell = 2,
#'   start_date = '04/07/2023',
#'   sample_window = 10,
#'   duration = 10,
#'   climate_table = climate_vars_list,
#'   run_type = "season"
#' )
build_runs <- function(rcp = NULL,
                       gcm = NULL,
                       sample_cell = NULL,
                       our_gcm = NULL,
                       run_type,
                       start_date,
                       climate_table,
                       sample_window) {
  
  if (!is.null(our_gcm)) {
    # If our_gcm argument is provided, use it as the input data frame
    our_gcm <- our_gcm %>% mutate(season = ifelse(lubridate::month(time) %in%  c(11, 12, 1, 2, 3, 4), "wet", "dry"))
      
  } else if (!is.null(rcp) & !is.null(gcm) & !is.null(sample_cell)) {
    # If rcp, gcm, and sample_cell arguments are provided, find the data frame using find_df() function
    our_gcm <- find_df(rcp, gcm, sample_cell) %>% mutate(season = ifelse(lubridate::month(time) %in%  c(11, 12, 1, 2, 3, 4), "wet", "dry"))
  } else {
    stop("Either provide rcp, gcm, and sample_cell arguments or provide a dataframe using our_gcm argument.")
  }
  
  df_list <- list() #initalizing empty list
  
  season_list <- season_order(start_date, length(climate_table)) #creating a lis to specify which season to find percentiles from 
  
  for (i in 1:length(climate_vars_list)) {
    start_date <- as.Date(start_date, format = "%m/%d/%Y", origin = "1970-01-01")
    
    df <- sample_window(start_date, sample_window, our_gcm) #Calculating the correct sampling window for each run 
    
    if(run_type == "year") {
      runs <- filter_df(df, climate_table[[i]])
    }
    
    if(run_type == "season") {
      # Call filter_df at index i of the climate variables list
      runs <- filter_df(df, climate_table[[i]], season_list[[i]])
    }
    
    # Appends the output to the df_list
    df_list[[i]] <- runs
    print(df_list)
    start_date <- start_date + lubridate::years(1) # Add one year to start_date
  }
  
  # Returns the list of filtered years for each data frame that can be put into the stitches function
  return(df_list)
}


