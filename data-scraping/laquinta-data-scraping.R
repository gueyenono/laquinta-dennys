# ~ ************************************** ~ #
# ~ * ~ SCRAPE LA QUINTA USA LOCATIONS ~ * ~ #
# ~ ************************************** ~ #

# Load packages -----------------------------------------------------------

library(rvest)
library(dplyr)
library(purrr)
library(furrr)
library(tidyr)
library(stringr)
library(ggmap)
library(magrittr)
library(tictoc)
source("R/get_latlon.R")


# Is it legal to scrape from wyndhamhotels.com? Yes! -----------------------------

robotstxt::paths_allowed("https://www.wyndhamhotels.com")


# Scrape hotel location urls --------------------------------------

main_html <- read_html("https://www.wyndhamhotels.com/laquinta/locations")

location_urls <- main_html %>%
  html_nodes(css = ".headline-d~ .property-list a:nth-child(1)") %>%
  html_attr(name = "href") %>%
  str_c("https://www.wyndhamhotels.com", .) %>%
  str_subset(pattern = "overview")


# Data scraping -----------------------------------------------------------

# > Function definition ----

scrape_laquinta_location <- function(location_url){
  
  read_html(location_url) %>%
    html_node(".hidden-xs p") %>%
    html_text()
  
}

possibly_scrape_laquinta_location <- possibly(scrape_laquinta_location, otherwise = NA)


# > Apply the function ----

plan(multiprocess)

tic()
master <- tibble(location_url = future_map_chr(location_urls, possibly_scrape_laquinta_location)) # 28.22s
toc()


# > Data manipulation (extraction of parts of street address)

master %<>%
  tidyr::extract(col = location_url, 
                 into = c("street_address", "city", "state", "zip_code"), 
                 regex = "(.*), (.*), (.*), (.*)",
                 remove = FALSE)

# Save LaQuinta addresses before geocoding
saveRDS(master, here::here("data-scraping/laquinta-data-before-geocoding.rds"))

# Geocode locations with MapQuest Open Geocoding API (5000 queries per day for standard accounts) -----------------------------------------

possibly_get_latlon <- possibly(get_latlon, otherwise = NA)

tic()
lonlat <- future_map_dfr(master$location_url[1:5], possibly_get_latlon) # the get_latlon function needs my API Key
toc()

master %<>%
  bind_cols(lonlat)
