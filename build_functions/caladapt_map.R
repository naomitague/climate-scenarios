
caladapt_map <- function() {

  # cal adapt polygons and grid cell ids
  caladaptpolygons <- ca_locagrid_geom()

  # map criteria
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = caladaptpolygons,
                label = ~id,  # Display the id column as label
                labelOptions = labelOptions())

  # display the map
  map
}