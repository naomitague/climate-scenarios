find_all_grid_cells <- function() {
  
  pattern <- "^grid_cell_.*"
  ui_df_names <<- ls(pattern = pattern, envir = globalenv())
  
  #my_list <- mget(df_names)
  return(ui_df_names)
}
