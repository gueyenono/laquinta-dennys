# ~ *********************************** ~ #
# ~ * ~ SCRAPE DENNY'S USA LOCATIONS ~ * ~ #
# ~ *********************************** ~ #

# Load packages -----------------------------------------------------------

library(rvest)
library(dplyr)
library(purrr)
library(furrr)
library(tidyr)
library(stringr)
library(ggmap)
library(magrittr)


# Is it legal to scrape from dennys.com? Yes! -----------------------------

robotstxt::paths_allowed("http://www.dennys.com")


# Data scraping -----------------------------------------------------------

# > Create master data frame (tibble) with state names and urls (excluding Washington DC) ----

master <- tibble(state = state.name, 
                 state_url = paste0("https://locations.dennys.com/", state.abb))


# > Get city names and urls ----

# Function definition

# state_url <- master$state_url[1]

scrape_city_info <- function(state_url){
  
  state_html <- read_html(state_url)
  
  city_name <- state_html %>%
    html_nodes(css = ".Directory-listLinkText") %>%
    html_text()
  
  city_url <- state_html %>%
    html_nodes(css = ".Directory-listLink") %>%
    html_attr(name = "href") %>%
    str_c("https://locations.dennys.com/", .)
  
  tibble(city = city_name, city_url)
  
}

possibly_scrape_city_info <- possibly(scrape_city_info, NULL)

# Use the funciton

master %<>%
  mutate(city_info = map(state_url, possibly_scrape_city_info)) %>%
  unnest(cols = city_info)


# > Get location addresses

# Function definition

scrape_location <- function(city_url){
  
  city_html <- read_html(city_url)
  
  city_html %>%
    html_nodes(css = ".LocationName-geo") %>%
    html_text()
  
}

possibly_scrape_location <- possibly(scrape_location, otherwise = NULL)

# Use the function

plan(multisession)

master %<>%
  mutate(location = future_map(city_url, possibly_scrape_location)) %>%
  unnest(cols = location) %>%
  select(-contains("url"))


# Scrape and add Washington, DC data ----------------------------------------------

dc_location <- read_html("https://locations.dennys.com/DC/WASHINGTON") %>%
  html_nodes(css = ".LocationName-geo") %>%
  html_text()

master %<>%
  bind_rows(tibble(state = "Washington, DC", city = NA, location = dc_location))



# Geocode locations (2500 queries per day for standard accounts) -----------------------------------------

register_google(key = Sys.getenv("Google_Maps_API_Key"), 
                account_type = "standard")

lonlat <- geocode(location = master$location, output = "latlon", source = "google") # Some addresses were not geocoded

# Retry the geocoding of addresses that failed

na_index <- which(is.na(lonlat$lon) | is.na(lonlat$lat))
lonlat_retry <- geocode(location = master$location[na_index], output = "latlon", source = "google") # Only 1 out of 15 addresses were geocoded
lonlat_success <- which(lonlat_retry$lon & lonlat_retry$lat)
lonlat[na_index[lonlat_success], ] <- lonlat_retry[lonlat_success, ] # Append lonlat tibble

# Add lonlat to master

master %<>% bind_cols(lonlat)


# Export dataset ----------------------------------------------------------

readr::write_rds(master, "data-scraping/dennys-locations.rds")


