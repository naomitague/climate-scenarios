#View(ca_catalog_rs())

####### FOR THE RMD
# user selects grid cells and rcp and model
library(sf)
library(leaflet)
library(caladaptr)
library(ggplot2)
library(ggrepel)
library(dplyr)
ui_grid_cells <- c(23696, 46896, 46898)
ui_rcp <- 85
ui_gcm <- 'MIROC5'
#########

# get all the data from this GCM and RCP, including historical
get_all_grid_cell_data <- function(grid_cell_id, lat, lon) {
  
  # Wind
  wind_slug <- paste0('wspeed_day_', ui_gcm , '_', 'rcp', ui_rcp)
  wind_slug_historical <- paste0('wspeed_day_', ui_gcm , '_historical')
  wind_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
    ca_slug(c(wind_slug, wind_slug_historical))
 
   wind_tbl <- wind_data %>% 
    ca_getvals_tbl(quiet = TRUE) %>% 
    select(dt, val) %>% 
    mutate(val = as.numeric(val)) %>% 
    rename(wind = val)
   
   # Min Relative Humidity
   minrh_slug <- paste0('relhumid-min_day_', ui_gcm , '_', 'rcp', ui_rcp)
   minrh_slug_historical <- paste0('relhumid-min_day_', ui_gcm , '_historical')
   minrh_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(minrh_slug, minrh_slug_historical))
   
   minrh_tbl <- minrh_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(min_humidity = val)
   
   # max relative humidity 
   maxrh_slug <- paste0('relhumid-max_day_', ui_gcm , '_', 'rcp', ui_rcp)
   maxrh_slug_historical <- paste0('relhumid-max_day_', ui_gcm , '_historical') 
   maxrh_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(maxrh_slug, maxrh_slug_historical))
   
   maxrh_tbl <- maxrh_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(max_humidity = val)
   
   # min temperature
   mintemp_slug <- paste0('tasmin_day_', ui_gcm , '_', 'rcp', ui_rcp)
   mintemp_slug_historical <- paste0('tasmin_day_', ui_gcm , '_historical')
   mintemp_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(mintemp_slug, mintemp_slug_historical))
   
   mintemp_tbl <- mintemp_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(min_temp = val)
   
   # max temperature
   maxtemp_slug <- paste0('tasmax_day_', ui_gcm , '_', 'rcp', ui_rcp)
   maxtemp_slug_historical <- paste0('tasmax_day_', ui_gcm , '_historical')
   maxtemp_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(maxtemp_slug, maxtemp_slug_historical))
   
   maxtemp_tbl <- maxtemp_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(max_temp = val)
   
   # precipitation 
   pr_slug <- paste0('pr_day_', ui_gcm , '_', 'rcp', ui_rcp)
   pr_slug_historical <- paste0('pr_day_', ui_gcm , '_historical')
   pr_data <- ca_loc_pt(coords = c(lon, lat)) %>% 
     ca_slug(c(pr_slug, pr_slug_historical))
   
   pr_tbl <- pr_data %>% 
     ca_getvals_tbl(quiet = TRUE) %>% 
     select(dt, val) %>% 
     mutate(val = as.numeric(val)) %>% 
     rename(precip = val)
   
  # join all climate variables together into one climate table 
   joined_tbl <- wind_tbl %>%
     inner_join(minrh_tbl, by = "dt") %>%
     inner_join(maxrh_tbl, by = "dt") %>%
     inner_join(mintemp_tbl, by = "dt") %>%
     inner_join(maxtemp_tbl, by = "dt") %>%
     inner_join(pr_tbl, by = "dt") %>% 
     mutate(dt = as_date(dt)) %>% 
     rename(time = dt) %>% 
     mutate(water_year = ifelse(month(time) > 9, year(time) + 1, year(time))) %>% 
     mutate(season = ifelse(lubridate::month(time) %in%  c(11, 12, 1, 2, 3, 4), "wet", "dry"))
   
   assign(paste0("grid_cell_", grid_cell_id), joined_tbl, envir = .GlobalEnv)
  
}


