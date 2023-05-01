# # vics function:
# ui_grid_cells <- c(1,2) # these are the grid cell names that the user will input at some point
# 
# getAllGridCells <- function(all_grid_cells) {
#   
#   for(i in seq_along(ui_grid_cells)) {
#     
#     # loop's through the cut/stitch / time series function 
#     cut_stitch_ts(grid_number = all_grid_cells[i])
#     
#   }
#   
# }
# 
# getAllGridCells(all_grid_cells = ui_grid_cells)

# questions vic has: are the years in the random selec function the water years? i think we'd want this since we're subsetting our dataframe by water year.

#' Get all grid cells
#'
#' This function retrieves all grid cells and loops through the cut/stitch/time series function for each grid cell.
#'
#' @param all_grid_cells A vector of grid cell names
#'
#' @return None
#'
#' @examples
#' ui_grid_cells <- c(1,2)
#' getAllGridCells(all_grid_cells = ui_grid_cells)
#'
#' @importFrom somePackage cut_stitch_ts
#'
#' @export
get_all_grid_cells_function <- function(all_grid_cells) {
  
  for(i in seq_along(all_grid_cells)) {
    
    # loop's through the cut/stitch / time series function 
    cut_stitch_ts(grid_number = all_grid_cells[i])
    
  }
  
}

