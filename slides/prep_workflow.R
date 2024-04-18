# import data
chargers_sf <- 
  readr::read_delim("./data/charging_points_ger.csv", 
                    delim =";") %>% 
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>% 
  sf::st_as_sf(.,
               coords = c("longitude","latitude"),
               crs = 4326)

attributes_districts <- 
  readr::read_delim("./data/attributes_districts.csv",
                    delim = ";") 



nrw_districts <- 
  sf::read_sf("./data/VG250_KRS.shp") %>% 
  dplyr::mutate(district_id = as.numeric(AGS)) %>% 
  sf::st_transform(crs = 3035)  %>% 
  dplyr::filter(as.integer(substr(district_id, 1, 1))== 5)
# 




setwd("C:/Users/stroppan/Documents/gesis-workshop-geospatial-techniques-R-2024")



sampling_area <-
  osmdata::getbb(
    "Nordrhein-Westfalen", 
    format_out = "sf_polygon"
  ) %>% 
  .$multipolygon %>% 
  sf::st_transform(3035) 

fake_coordinates <-
  sf::st_sample(sampling_area, 1000) %>% 
  sf::st_sf() %>% 
  dplyr::mutate(
    id_2 = 
      stringi::stri_rand_strings(10000, 10) %>% 
      sample(1000, replace = FALSE)
  )


sampling_area_attributes <-
  # load district shapefile
  sf::read_sf("./data/VG250_KRS.shp") %>% 
  # transform crs
  sf::st_transform(3035) %>% 
  # some data cleaning
  dplyr::mutate(district_id = as.numeric(AGS)) %>% 
  dplyr::select(district_id) %>% 
  # reduce to area of nrw: x lies within y
  sf::st_join(.,
              sampling_area, 
              join = sf::st_within, 
              left = FALSE) %>% 
  # add attribute table
  dplyr::left_join(. , 
                   readr::read_delim("./data/attributes_districts.csv",
                                     delim = ";"), 
                   by = "district_id") 
summary(sampling_area_attributes)



sampling_area_attributes <-
  readr::read_delim("./data/charging_points_ger.csv", 
                  delim =";") %>% 
  filter(!is.na(longitude) & !is.na(latitude)) %>% 
  sf::st_as_sf(
    .,    
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>% 
  sf::st_transform(. , crs = 3035) %>% 
  sf::st_join(., sampling_area_attributes %>%  dplyr::select(district_id), join = sf::st_within) %>% 
  dplyr::group_by(district_id) %>%
  dplyr::summarise(charger_count = n()) %>% 
  sf::st_drop_geometry() %>% 
  left_join(sampling_area_attributes, ., by = "district_id") %>% 
  dplyr::mutate(charger_dens = (charger_count*1000) / population)




sf::st_area(sampling_area_attributes) %>% 
  head(4)

sampling_area_attributes %>% 
  sf::st_transform(4326) %>% 
  sf::st_area(.) %>% 
  head(4)

# calculation population density
sampling_area_attributes <-
  sampling_area_attributes %>% 
  # calculate area of districts (areas will always
  # be calculatednin units according to the CRS )
  dplyr::mutate(area = sf::st_area(.)) %>% 
  # change unit to square kilometers
  dplyr::mutate(area_km2 = units::set_units
                (area, km^2)) %>% 
  # recode variable as numeric
  dplyr::mutate(area_km2 = as.numeric
                (area_km2)) %>% 
  # calculate population density
  dplyr::mutate(pop_dens = population/
                  area_km2)


district_linked_df <-
  sampling_area_attributes %>%
  # keeping just the variables we want
  dplyr::select(publictransport_meandist,charger_dens,pop_dens) %>% 
  # since we want to join district to
  # respondent defining coordintes first
  sf::st_join(fake_coordinates,
              # district data second
              . ,
              # some points may lie on the border
              # choosing intersects therefore
              join = sf::st_intersects) %>% 
  # drop our coordinates for data protection
  sf::st_drop_geometry()


# Num Charging Stations in Buffer
library(sf)

# Assuming you have loaded the necessary libraries and datasets
charger_nrw <-
readr::read_delim("./data/charging_points_ger.csv", 
                  delim =";") %>% 
  filter(!is.na(longitude) & !is.na(latitude)) %>% 
  sf::st_as_sf(
    .,    
    coords = c("longitude", "latitude"),
    crs = 4326
  ) %>% 
  sf::st_transform(. , crs = 3035) %>%
  sf::st_intersection(. ,sampling_area)

# Create 500m buffers around the fake coordinates
# Create 500m buffers around the fake coordinates
buffers <- fake_coordinates %>%
  sf::st_buffer(dist = 2000)

# Perform intersection between buffers and points_sf
inter <- sf::st_intersects(buffers, charger_nrw)

# Count points within each buffer
fake_coordinates <- fake_coordinates %>%
  mutate(num_charger = lengths(inter))





# Pop Dens in Buffer

inhabitants_nrw <-
  z11::z11_get_100m_attribute(Einwohner) %>% 
  terra::crop(. , sampling_area)


# ppoulation for 2000m buffer
population_buffers <- 
  terra::extract(
    inhabitants_nrw, 
    fake_coordinates %>% 
      sf::st_buffer(2000) %>% 
      terra::vect(), 
    fun = mean,
    na.rm = TRUE
  )

# spatially link with buffers on the fly
coordinate_linked_df <-
  fake_coordinates %>% 
  dplyr::mutate(population_buffer = population_buffers[[2]])


nrw_pt_trainstops <- sf::st_read("./data/nrw_pt_osmtrainstops.shp")



nrw_pt_stops <-
  osmdata::getbb(
    "Nordrhein-Westfalen" 
  ) %>% 
  osmdata::opq(timeout = 25*100) %>% 
  osmdata::add_osm_feature(key = "public_transport", value = "stop_position") %>% 
  osmdata::osmdata_sf()


nrw_pt_stops_ <-
  nrw_pt_stops$osm_points %>%  
  tibble::as_tibble() %>%  
  sf::st_as_sf() %>%  
  sf::st_transform(3035) 

nrw_pt_trainstops <-
  nrw_pt_stops_ %>% 
  dplyr::filter(train == "yes")

nrw_pt_trainstops %>% 
 dplyr::select() %>% 
  sf::st_write(., "./data/nrw_pt_osmtrainstops.shp", delete_layer = T)

new <- sf::st_read("./data/nrw_pt_osmtrainstops.shp", crs = 3035)

nearest_station <- 
  sf::st_nearest_feature(fake_coordinates,new)

dist <-
sf::st_distance(fake_coordinates, 
                new[nearest_station,], by_element=TRUE)

new <-
  fake_coordinates %>% 
  mutate(dist_km = as.numeric(
    dist) / 1000)


#####





chargers_sf <- 
  readr::read_delim("./data/charging_points_ger.csv", 
                    delim =";") %>% 
  dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>% 
  sf::st_as_sf(.,
               coords = c("longitude","latitude"),
               crs = 4326) %>% 
  sf::st_transform(3035)
































cologne_pt_stops


sf::st_write(nrw_pt_stops)

cologne_pt_stops