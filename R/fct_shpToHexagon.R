shpToHexagon <- function(shp, cellsize = 0.8, square = F, crs = 4326){
  hex <- sf::st_make_grid(
    shp, 
    cellsize = cellsize, 
    square = square
  ) %>% 
    base::data.frame(., hexagon = paste0("H", stringr::str_pad(1:length(.), 3, pad = "0"))) %>% 
    sf::st_as_sf() %>% 
    sf::st_set_crs(crs)
  return(hex)
}