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

