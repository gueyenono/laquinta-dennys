library(ggmap)
library(dplyr)


# Set the Google Static Maps API Key
register_google(key = Sys.getenv("Google_API_Key"))

# Import and merge data -------------------------------------------------------------
# Data was scraped on 10/20/2019

lq <- readr::read_rds("data-scraping/laquinta-locations.rds") # LaQuinta
de <- readr::read_rds("data-scraping/dennys-locations.rds") # Denny's

lq2 <- lq %>%
  select(lon, lat) %>%
  mutate(franchise = "La Quinta")

de2 <- de %>%
  select(lon, lat) %>%
  mutate(franchise = "Denny\'s")

dat <- bind_rows(lq2, de2)


# Where are the LaQuinta's and the Denny's in the US? ---------------------

usa_map <- get_map(location = "united states", zoom = 4, maptype = "terrain", source = "google", color = "color")
p <- ggmap(usa_map)

# LaQuinta locations
p + geom_point(data = lq, aes(x = lon, y = lat), color = "red", alpha = 0.25)

# Denny's locations
p + geom_point(data = de, aes(x = lon, y = lat), color = "blue", alpha = 0.25)

# Both

p +
  geom_point(data = dat, aes(x = lon, y = lat, color = franchise), alpha = 0.2, size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "LaQuinta and Denny's locations in the USA") +
  theme_void() -> map_lq_dennys

ggsave(here::here("viz/map-laquinta-dennys.png"), plot = map_lq_dennys, width = 7, height = 7, units = 'cm', scale = 2, dpi = 600)  
