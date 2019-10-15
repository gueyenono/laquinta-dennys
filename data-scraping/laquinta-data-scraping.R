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


# Is it legal to scrape from dennys.com? Yes! -----------------------------

robotstxt::paths_allowed("https://www.wyndhamhotels.com/laquinta")



# Data scraping -----------------------------------------------------------

main_html <- read_html("https://www.wyndhamhotels.com/laquinta/locations")
