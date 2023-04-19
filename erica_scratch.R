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
