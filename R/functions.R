#' Returns bid-rent data for a given CBSA
#'
#' @param geoid_cbsa FIPS code for the CBSA of interest
#' @param tracts Spatial sf dataframe of census tracts
#' @param city_centers DF of city center lat long
#' @param cbsa_to_county_crosswalk Mapping of the counties that make up a CBSA
#' @param buffer_distance Bid-rent radius to use in miles
#'
#' @return dataframe with bid-rent data for a given CBSA
#'
get_bid_rent_data <- function(
  geoid_cbsa,
  tracts,
  city_centers,
  cbsa_to_county_crosswalk,
  buffer_distance = 20) {

  meters_per_mile <- 1609.34

  # Convert city center to sf point and set CRS
  city_center <- st_point(c(
      city_centers %>% filter(cbsa == geoid_cbsa) %>% pull(Longitude) %>% nth(1),
      city_centers %>% filter(cbsa == geoid_cbsa) %>% pull(Latitude) %>% nth(1))
    ) %>%
    st_sfc() %>%
    st_set_crs(4269)

  if (st_is_empty(city_center)) {
    print("NO lat/longitude found")
    return(tibble::tibble())
  }

  # Calculate the buffer from the city center
  buffer_distance_meters <- buffer_distance * meters_per_mile
  city_buffer <- st_buffer(city_center, dist = buffer_distance_meters)

  cbsa_counties <- cbsa_to_county_crosswalk %>%
    filter(cbsacode == geoid_cbsa) %>%
    pull(geoid_co) %>%
    unique()

  if (length(cbsa_counties) == 0) {
    print("No matching entry in the CBSA crosswalk")
    return(tibble::tibble())
  }

  if (length(cbsa_counties) == 0) {
    return(tibble::tribble())
  }

  cbsa_tracts <- tracts %>%
    mutate(geoid_co = paste0(STATEFP, COUNTYFP)) %>%
    filter(geoid_co %in% cbsa_counties) %>%
    sf::st_as_sf()

  if (nrow(cbsa_tracts) == 0) {
    print("CBSA tracts has length of 0")
    return(tibble::tribble())
  }

  # Find intersecting tracts
  intersecting_tracts <- cbsa_tracts %>%
    rowwise() %>%
    mutate(
      is_in_buffer = any(st_intersects(geometry, city_buffer)),
      distance_to_buffer_centroid = as.numeric(st_distance(st_centroid(geometry), city_center, by_element = TRUE) / meters_per_mile),
      # Calculate the tract area and include only the proportion that is not water
      tract_area = as.numeric(st_area(geometry)/meters_per_mile) * (ALAND / (AWATER + ALAND))
    ) %>%
    filter(is_in_buffer == TRUE) %>%
    sf::st_as_sf()

  # Load ACS variables
  acs_vars <- c(
    "median_gross_1_bed" = "B25031_003",
    "housing_units" = "B25001_001"
  )

  acs_st <- stringr::str_sub(cbsa_counties, 1, 2)
  acs_co <- stringr::str_sub(cbsa_counties, 3, 5)

  acs_dta <- get_acs("tract", variables = acs_vars, year = 2022, state = acs_st, county = acs_co)

  bid_rent_dta <- inner_join(
      acs_dta,
      intersecting_tracts,
      by = "GEOID"
    ) %>%
    select(GEOID, variable, estimate, distance_to_buffer_centroid, AWATER, tract_area) %>%
    sf::st_drop_geometry() %>%
    distinct() %>%
    tidyr::pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      housing_density = housing_units / tract_area,
      geoid_cbsa = geoid_cbsa
    )

  return(bid_rent_dta)

}


#' Load and bind all data into a tibble
#'
#' @return tibble of combined data files
loadAllBidRentData <- function() {

  data_filenames <- list.files(here::here("data"))
  all_data <- tibble::tribble()

  # Loop through all files in the data folder
  for (file in data_filenames) {

    if (stringr::str_starts(file, "bid_rent_")) {

      file_data <- readr::read_csv(col_types = "c", here::here(paste("data/", file, sep="")))

      # assign first file to all_data variable
      if (nrow(all_data) == 0) {
        all_data <- file_data
      }
      # bind files to all_data variable
      else {
        all_data <- dplyr::bind_rows(all_data, file_data)
      }
    }
  }

  return(all_data)
}
