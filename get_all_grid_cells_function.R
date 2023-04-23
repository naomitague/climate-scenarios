# inputs: 
  # sample_cell: sampling grid cell, 
  # all_grid_cells: all grid cells of interest. this comes from the shiny app selections after looking at the caladapt api map
  # sample_grid_series: the years or seasons/years for the sample grid cell that were used in the random selection based off of the filtered criteria
  # gcm: which model we used
  # rcp: which emissions scenario we used

# 1) sees which model we're using and if we've stitched by seasons or years in the sampling grid cell and 
# 2) for each grid cell: grabs those same run_types during the same time periods and models for all the other grid cell data frames
# 3) for each grid cell: calls the stitch function

# outputs: 1 timeseries file per grid cell? 

# this will be user input in the shiny app:
ui_grid_cells <- c(1,2)

getAllGridCells <- function(all_grid_cells, year_selection, our_gcm) {
  
  for(i in length(ui_grid_cells)) {
    
    cut_and_stitch(our_gcm = our_gcm, 
                   year_selection = year_selection, 
                   grid_number = all_grid_cells[i])
    
  }
  
}

getAllGridCells(all_grid_cells = ui_grid_cells, year_selection = sample_grid_series, our_gcm = our_gcm)
