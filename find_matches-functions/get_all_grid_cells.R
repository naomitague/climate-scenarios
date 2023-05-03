#' Get all grid cells
#'
#' This function takes in a list of all grid cells and loops through the "cut_stitch_ts" function for each grid cell.
#'
#' @param all_grid_cells A vector of grid cell names
#' @return None
#' @examples
#' ui_grid_cells <- c(1,2)
#' get_all_grid_cells(all_grid_cells = ui_grid_cells)

get_all_grid_cells <- function(all_grid_cells) {
  
  for(i in seq_along(all_grid_cells)) {
    
    # loop's through the cut/stitch / time series function 
    cut_stitch_ts(grid_number = all_grid_cells[i])
    
  }
  
}

