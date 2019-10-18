# Open Geocoding API from MapQuest

#' Geocoding with MapQuest Open Geolocation API
#'
#' @param address character vector of street address to geocode
#'
#' @return tibble with one row and two columns: lat and lon

get_latlon <- function(address){
  
  httr::GET(url = "http://open.mapquestapi.com/geocoding/v1/address", query = list(
    key = Sys.getenv("MapQuest_API_Key"),
    location = address
  )) -> raw
  
  tibble::tibble(lat = httr::content(raw)$results[[1]]$locations[[1]]$latLng$lat,
         lon = httr::content(raw)$results[[1]]$locations[[1]]$latLng$lng)
  
}
