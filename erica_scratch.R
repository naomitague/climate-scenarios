source("/Users/ericadale/data-cleaning/packages.R")
source("/Users/ericadale/data-cleaning/cal_adapt_4models_allscenarios.R")

# Predefine Seasons
months <- 1:12
seasons <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
             "Summer", "Summer", "Fall", "Fall", "Fall", "Winter")
season_data <- data.frame(month = months, season = seasons)


####################### If statement -----------------
# If statement: numeric = years, character = seasons
# Parse information for season "Fall_2016"


if (is.numeric(run_samples[[1]])) {
  
  # Search by years
  
} else if (is.character(run_samples[[1]])) {
  
  # Split the information "Fall_2016"
  seasons_years <- strsplit(run_samples, split = "_")
  
  # Search by seasons and years
  
}


###################### RANDOMLY SELECT -----------------
# Randomly select one of each option
# Save as new list sample_grid_series
# -> Will be input to Victoria's function for other grid cell loop


# Using Mallory's output
run_samples <- list

# BY YEAR WORKS! --------
# Create blank list for randomly selected years
sample_grid_series <- list()

# Randomly select one element of each list
for (i in seq_along(run_samples)) {
  if (length(run_samples[[i]]) == 1) {   # if only one option, select, else, sample
    sample_grid_series[[i]] <- run_samples[[i]]
  } 
  else {
    sample_grid_series[[i]] <- sample(run_samples[[i]], size = 1, replace = TRUE)
  }
}


# View the resulting list
print(sample_grid_series)
# Turn into function ---------
# CHECK IF THIS FUNCTION WORKS ------------------
randomly_select <- function(run_samples) {
  
  # Create blank list for randomly selected elements
  sample_grid_series <- list()
  
  # Randomly select one element of each list
  for (i in seq_along(run_samples)) {
    if (length(run_samples[[i]]) == 1) {   # if only one option, select, else, sample
      sample_grid_series[[i]] <- run_samples[[i]]
    } 
    else {
      sample_grid_series[[i]] <- sample(run_samples[[i]], size = 1, replace = TRUE)
    }
  }
  
  # Return resulting list
  return(sample_grid_series)
  
}

randomly_select(run_samples)


################ Stitch by year - WORKS! ------------

# Create original_model
original_model <- CanESM2_rcp45 %>%
  mutate(year = lubridate::year(time))         # Pull out the numeric month from a model
  # original_model <- left_join(CanESM2_rcp45, season_data, by = "month")    # Join seasonal column

# Empty data frame with column names
col_names <- colnames(original_model)
combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(combined_cut_model) <- col_names

# Save list of years
year_selection <- unlist(sample_grid_series)

# Run loop
for (i in year_selection) {
  new_cut <- original_model[which(original_model$year == i), ]
  
  # 3. Add this cut to the existing dataframe for all cuts
  combined_cut_model <- rbind(combined_cut_model, new_cut)   # Double arrow to update global model
  
}











# BY SEASON ------ Not set up yet

# Predefine seasons
months <- 1:12
seasons <- c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer",
             "Summer", "Summer", "Fall", "Fall", "Fall", "Winter")
season_data <- data.frame(month = months, season = seasons)

# stitch first season/year

cut_stitch_function <- function(model_selection, season_selection = NULL, year_selection) {
  
  # 1. Add season and year column to original data frame
  # Question about model dataframe structure: will it have month/year/season columns??
  original_model <- model_selection %>%
    mutate(year = lubridate::year(time)) %>%        # Need to be able to filter by year
    mutate(month = lubridate::month(time))          # Pull out the numeric month from a model
  original_model <- left_join(original_model, season_data, by = "month")    # Join seasonal column
  
  # 2. Select range by time and cut, save to new model
  new_cut <- original_model[which(original_model$season == season_selection &
                                    original_model$year == year_selection), ]
  
  # 3. Add this cut to the existing dataframe for all cuts
  combined_cut_model <- rbind(combined_cut_model, new_cut)   # Double arrow to update global model
  
  # End loop
  # Autonaming of file??
  newname <<- combined_cut_model
}




### # time series blurbs ----------------------

file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")

for (i in seq_along(file_type)) {
  print(file_type[i])
  print(col_names[i])
}



### LEAP YEAR
# create example data
df <- data.frame(
  year = c(2020, 2021, 2022, 2023),
  temp = c(25, 26, 27, 28)
)

# loop over years in dataset
for (i in seq_along(df$year)) {
  year <- df$year[i]
  expected_leap_year <- get_expected_leap_year(year)
  if (year == expected_leap_year) {
    if (leap_year(year) & nrow(filter(df, year == year & month == 2 & day == 29)) == 1) {
      # keep data for leap year with 29 days in February
    } else if (!leap_year(year) & nrow(filter(df, year == year & month == 2 & day == 29)) == 1) {
      # add day with average of dates before/after
      before <- filter(df, year == year & month == 2 & day < 29)$temp
      after <- filter(df, year == year & month == 2 & day > 29)$temp
      avg <- mean(c(before, after))
      df <- rbind(df, data.frame(year = year, month = 2, day = 29, temp = avg))
    } else {
      # keep data for non-leap year or leap year without 29 days in February
    }
  } else {
    if (nrow(filter(df, year == year & month == 2 & day == 29)) == 1) {
      # remove February 29th data for unexpected leap year
      df <- filter(df, !(year == year & month == 2 & day == 29))
    } else {
      # keep data for non-leap year
    }
  }
}


###### Adding season column to df -----------
# Define predetermined seasons
months <- 1:12
seasons <- c("Wet", "Wet", "Wet", "Wet", "Dry", "Dry", "Dry", "Dry", "Dry", "Dry", "Wet", "Wet")
season_data <- data.frame(month = months, season = seasons)

our_gcm <- our_gcm %>%
  mutate(year = lubridate::year(time)) %>%       # Need to be able to filter by year
  mutate(month = lubridate::month(time))          # Pull out the numeric month from a model

# Add season column
our_gcm <- left_join(our_gcm, season_data, by = "month")    # Join seasonal column


###### Cutting selections BY YEAR-------------------------
# Cut each year from the climate model dataframe and stitch together
# Next problem: how to loop through EACH grid cell

cut_and_stitch <- function(our_gcm = our_gcm,
                           year_selection = unlist(sample_grid_series),
                           grid_number) {
  
  
  # Add year column to original data frame
  original_model <- our_gcm %>%        # Placeholder model to manipulate in function
    mutate(year = lubridate::year(time))       # Need to be able to filter by year
  
  # ADD IN: Filter instead by water year
  
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


#####################
# Stitch as new df ----------
#####################


# NEED TO DO -----
# Naming the file by the original selected model name
# NEED input of grid_number for naming
# Automate to loop through all selected grids


# INPUTS -----
# our_gcm from build_runs.R function
# sample_grid_series from randomly_select.R function
# grid_number from larger loop, still need to figure out saving this list to loop through


# FUNCTION -----
cut_and_stitch <- function(our_gcm = our_gcm,
                           series_selection = unlist(sample_grid_series),
                           grid_number) { # grid_number is placeholder, 
  # will need to pull out from grid cells to name
  
  # Empty data frame with column names
  col_names <- colnames(our_gcm)
  combined_cut_model <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  colnames(combined_cut_model) <- col_names
  
  # Check if series_selection is by year or season_year
  if(all(is.numeric(series_selection))) {
    
    # YEARS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      new_cut <- our_gcm[which(our_gcm$water_year == i), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } else {
    
    # SEASONS: Loop through, select range by time and cut and save
    for (i in series_selection) {
      season_year <- strsplit(i, "_")
      wet_dry <- season_year[[1]][1]
      year <- season_year[[1]][2]
      new_cut <- our_gcm[which(our_gcm$season == wet_dry & our_gcm$water_year == year), ]
      
      # Add this cut to the existing dataframe for all cuts
      combined_cut_model <- rbind(combined_cut_model, new_cut)
      
    } # END LOOP
    
  } # END IF
  
  
  
  # Name file with model and grid number
  model_name <- deparse(substitute(our_gcm))  # NEED TO PULL OUT ORIGINAL NAME
  grid_name_formatted <- paste0(str_to_title(model_name), "_grid_", grid_number)
  
  # Assign the resulting list to a variable in the global environment
  assign(grid_name_formatted, combined_cut_model, envir = .GlobalEnv)
  
  return(combined_cut_model)
}

# Output: file: CanESM2_rcp45_grid_42

# TEST
cut_and_stitch(our_gcm = our_gcm, 
               # year_selection = sample_grid_series, 
               grid_number = 3)


###################
# Convert to time series-----
###################

# NEXT STEPS ----
# how to pull out NAME from cut_and_stitch output file to use as input here instead of our_gcm
## Answer: combine functions??

# INPUTS ----
# date output: start_date
# 

# FUNCTION ----
convert_time_series <- function(start_year, start_month, start_day, file_name, our_gcm = our_gcm) {
  
  # Create folder for time series files with shared name
  folder_name <- paste0(grid_name_formatted, "_time_series")
  dir.create(folder_name, showWarnings = FALSE)
  
  ##### LEAP YEAR #################
  # ADD loop with start_date to add in/delete leap year day
  
  # LIST expected year lengths with start_date and lubridate::leap_year
  # COMPARE expected year length [i] and actual year length [i]
  # IF expected[i] == actual[i], continue
  # IF expected[i] > actual[i], add Feb 29
  # IF expected[i] < actual[i], remove row Feb 29
  
  # if year / 4 remainder = 0, then leap year
  # if input year =/ expected leap year then....
  # Cut out a day or add a day for leap year
  # IF extra day, delete Feb 29
  # IF short a day, repeat the previous day or average between previous/following day on Feb 29
  
  
  # MAKE SURE every 4th year is a leap year
  # Total days of every 4 years matches expected
  
  
  # LOOP tied together file type and column name
  file_type <- c(".tmax", ".tmin", ".rain", ".relative_humidity_max", ".relative_humidity_min", ".wind")
  col_names <- c("max_temp", "min_temp", "precip", "max_humidity", "min_humidity", "wind")
  #col_names <- c("max_temp_1", "min_temp_1", "precip_1", "max_humidity_1", "min_humidity_1", "wind_1")
  #col_names <- c("max_temp_2", "min_temp_2", "precip_2", "max_humidity_2", "min_humidity_2", "wind_2")
  # Currently multiple col_names because of the format of the dataframes, but ultimate plan is to separate by grid so they will be without the _num
  
  for (i in seq_along(file_type)) {
    # Construct file name with proper extension
    file_name <- file.path(folder_name, paste0(grid_name_formatted, file_type[i]))
    
    # Construct top line of file
    top_of_file <- sprintf("%d %d %d 1", start_year, start_month, start_day)
    
    # Write top line of file
    write.table(top_of_file, file = file_name, row.names = F, col.names = F, quote = F)
    
    # Write variable data to file, appending to top line
    write.table(our_gcm[[col_names[i]]], file = file_name, row.names = F, col.names = F, quote = F, append = T)
    # NEEDS TO BE NOT our_gcm but the OUTPUT of cut_and_stitch
  }
}


# TEST
convert_time_series(1990, 08, 14, "new_name", our_gcm = our_gcm)


# LEAP YEARS -------------------

# New idea:
# Remove ALL Feb 29
# For every x rows (total of 4 years 365x4) add a row

### OLD idea:
# ADD loop with start_date to add in/delete leap year day
# inputs we already have
series_selection <- unlist(sample_grid_series)
start_year <- year(start_date)

# LIST if leap year or not with lubridate::leap_year
end_date <- start_date + years(100)
dates <- seq(start_date, end_date, by = "years")
is_leap <- leap_year(year(dates))
expected <- ifelse(is_leap, 366, 365)


# IF by year
actual <- leap_year(series_selection)

# Will be even more complicated with season??

# COMPARE expected year length [i] and actual year length [i]
for (i in expected) {
  # IF expected[i] == actual[i], continue
  if(expected[i] == actual[i]){
    #continue
  }
  # IF expected[i] > actual[i], add Feb 29
  if(expected[i] > actual[i]){
    # Add Feb 29 with average from day before/after
  }
  # IF expected[i] < actual[i], remove row Feb 29
  if(expected[i] < actual[i]){
    # remove Feb 29
  }
  
  
} # END for loop


# if year / 4 remainder = 0, then leap year
# if input year =/ expected leap year then....
# Cut out a day or add a day for leap year
# IF extra day, delete Feb 29
# IF short a day, repeat the previous day or average between previous/following day on Feb 29


# MAKE SURE every 4th year is a leap year
# Total days of every 4 years matches expected



# CHAT GPT GUESS BELOW:
# Determine if start year is a leap year
is_leap_year <- ifelse(start_year %% 4 == 0 & (start_year %% 100 != 0 | start_year %% 400 == 0), TRUE, FALSE)

# Determine number of days in the year
days_in_year <- ifelse(is_leap_year, 366, 365)

# Create a sequence of dates
dates <- seq(as.Date(paste(start_year, start_month, start_day, sep = "-")), 
             by = "day", 
             length.out = days_in_year)

# Loop through the dates and add leap year day if needed
for (i in seq_along(dates)) {
  # Determine if current year is a leap year
  current_year <- year(dates[i])
  is_leap_year <- ifelse(current_year %% 4 == 0 & (current_year %% 100 != 0 | current_year %% 400 == 0), TRUE, FALSE)
  
  # Add leap year day if current date is Feb 29 and current year is a leap year
  if (month(dates[i]) == 2 & day(dates[i]) == 29 & is_leap_year) {
    new_day <- lubridate::ymd(paste0(year(dates[i]), "-03-01"))
    new_days <- rep.int(new_day, 1)
    new_data <- data.frame(Date = new_days, Data = NA_real_)
    new_data$Data[1] <- mean(c(as.numeric(data[i - 1, 2]), as.numeric(data[i + 1, 2])))
    data <- rbind(data[1:i, ], new_data, data[(i+1):nrow(data), ])
    dates <- as.Date(data$Date)
    i <- i + 1
    
    # Delete Feb 29 if current year is not a leap year
  } else if (month(dates[i]) == 2 & day(dates[i]) == 29 & !is_leap_year) {
    data <- data[-i, ]
    dates <- as.Date(data$Date)
    n <- n - 1
    i <- i - 1
    
    # Add or remove a day if current year has a day added or removed
  } else if (is_added_day | is_removed_day) {
    if (is_added_day) {
      new_day <- lubridate::ymd(paste0(year(dates[i]), "-", month(dates[i]), "-", day(dates[i]) + 1))
      new_days <- rep.int(new_day, 1)
      new_data <- data.frame(Date = new_days, Data = NA_real_)
      new_data$Data[1] <- mean(c(as.numeric(data[i, 2]), as.numeric(data[i + 1, 2])))
      data <- rbind(data[1:i, ], new_data, data[(i+1):nrow(data), ])
      dates <- as.Date(data$Date)
      i <- i + 1
    } else {
      data <- data[-i, ]
      dates <- as.Date(data$Date)
      n <- n - 1
      i <- i - 1
    }
  }
}

####################
# Loop through grid cells --------------
####################

# save selected grid cells
# for each grid cell....
# Loop through each of sample_grid_series for each of the selected grids in cut_stitch_function
# Loop through convert_time_series for each grid cell

# need a function to save the selected_grids
selected_grids <- list() # grid_cell_dataframe names?
grid_number <- list() # how to connect selected_grids and grid_number? Or pull out the number for name?

for(i in seq_along(selected_grids)) {
  
  grid_number <- i
  grid_name_formatted <- paste0("grid_", grid_number)
  
  # loop through cut and stitch to time series
  cut_and_stitch_ts(our_gcm = selected_grids[i], # Pull out name of next grid cell
                 series_selection = unlist(sample_grid_series),
                 start_date,
                 grid_number = grid_number[i])
}



# LEAP YEAR -----
# create a sample dataframe with dates
datelist <- data.frame(date = c("2024-01-01", "2024-02-28", "2024-02-29", "2024-03-01"))

# loop through each row of the dataframe
for (i in 1:nrow(datelist)) {
  
  # extract the date from the row
  date <- datelist$date[i]
  date <- as.Date(date)
  
  # check if the date is February 29th
  if (format(date, "%m-%d") == "02-29") {
    
    # delete the row
    df <- df[-i]
    
    # decrement the row counter
    i <- i - 1
    
  }
  
}

# print the resulting dataframe
print(df)


# create a sample dataframe with dates spanning 100 years
start_year <- 1923
end_year <- 2022
dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
df <- data.frame(date = dates)

# filter out February 29th dates
df <- df[!(format(df$date, "%m-%d") == "02-29"), ]

# print the resulting dataframe
print(df)


# New attempt
# create a sample dataframe with dates spanning 100 years
start_year <- 1923
end_year <- 2022
dates <- seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day")
df <- data.frame(date = dates, col1 = runif(length(dates)), col2 = rnorm(length(dates)))

# loop through each year in the dataframe
for (year in start_year:end_year) {
  
  # check if the year is a leap year
  if (leap_year(year) =) {
    
    # get the date before and after February 29th
    before_date <- as.Date(paste0(year, "-02-28"))
    after_date <- as.Date(paste0(year, "-03-01"))
    
    # calculate the row to insert
    insert_row <- data.frame(date = as.Date(paste0(year, "-02-29")))
    for (col in names(df)[-1]) {
      insert_row[[col]] <- mean(c(df[df$date == before_date, col], df[df$date == after_date, col]))
    }
    
    # insert the row into the dataframe
    df <- rbind(df, insert_row)
    
  }
  
}

# sort the dataframe by date
df <- df[order(df$date), ]

# print the resulting dataframe
print(df)
