# Mallory's Output:  buildruns.RMD
# run_samples list of arrays
# 1{[years/seasons that match]} 2{[years/seasons that match]} 3{[years/seasons that match]}


######################
# Randomly select one of each option
# Save as new list sample_grid_series
# -> Will be input to Victoria's function for other grid cell loop
######################

# Using Mallory's output
run_samples <- list


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


# Turn into function
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


#######################
# If statement: numeric = years, character = seasons
# Parse information for season "Fall_2016"
#######################

if (is.numeric(run_samples[[1]])) {
  
  # Search by years
  
} else if (is.character(run_samples[[1]])) {
  
  # Split the information "Fall_2016"
  seasons_years <- strsplit(run_samples, split = "_")
  
  # Search by seasons and years

  }




######################
# Leap Years
# Stitch as new df
# Convert to time series
######################




####################
# Loop through grid cells
####################