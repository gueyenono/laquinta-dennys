library(ggmap)
library(dplyr)
library(purrr)
library(geosphere)


# Set the Google Static Maps API Key
register_google(key = Sys.getenv("Google_API_Key"))

# Import and merge data -------------------------------------------------------------
# Data was scraped on 10/20/2019

# La Quinta locations

lq_raw <- readr::read_rds("data-scraping/laquinta-locations.rds") 

lq <- lq_raw %>%
  # filter(state %in% state.abb) %>% # keep US locations only
  mutate(city_state = paste(city, state, sep = ", "),
         franchise = "La Quinta") %>%
  select(franchise, street_address, city, state, city_state, lon, lat)

# Denny's locations

de_raw <- readr::read_rds("data-scraping/dennys-locations.rds")

lookup_vector <- {names(state.abb) <- state.name; state.abb}

de <- de_raw %>%
  # filter(state %in% state.abb) %>% # keep US locations only
  mutate(franchise = "Denny\'s",
         state_abb = recode(state, !!!lookup_vector),
         city_state = paste(city, state_abb, sep = ", ")) %>%
  select(franchise, street_address = location, city, state = state_abb, city_state, lon, lat)


# Combine La Quinta and Denny's locations

dat_all <- bind_rows(lq, de)

# Filter for common cities

common_cities <- intersect(de$city_state, lq$city_state) # Vector of cities where both La Quinta and Denny's operate
L

dat_common <- dat_all %>% 
  tidyr::drop_na() %>%
  filter(state %in% state.abb, 
         city_state %in% common_cities)

# Where are the LaQuinta's and the Denny's in the US? ---------------------

usa_map <- get_map(location = "united states", zoom = 4, maptype = "terrain", source = "google", color = "color")
p <- ggmap(usa_map)

# LaQuinta locations

p + geom_point(data = lq, aes(x = lon, y = lat), color = "blue", alpha = 0.25) +
  labs(title = "LaQuinta locations in the USA") +
  theme_void() -> lq_map

ggsave(here::here("viz/map-laquinta.png"), plot = lq_map, width = 7, height = 7, units = 'cm', scale = 2, dpi = 600)  


# Denny's locations

p + geom_point(data = de, aes(x = lon, y = lat), color = "red", alpha = 0.25) +
  labs(title = "Denny\'s locations in the USA") +
  theme_void() -> de_map

ggsave(here::here("viz/map-dennys.png"), plot = de_map, width = 7, height = 7, units = 'cm', scale = 2, dpi = 600)  


# Both

# All locations
p +
  geom_point(data = dat_all, aes(x = lon, y = lat, color = franchise), alpha = 0.2, size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "LaQuinta and Denny's locations in the USA", subtitle = "All locations") +
  theme_void() -> map_lq_dennys_all

ggsave(here::here("viz/map-laquinta-dennys-all.png"), plot = map_lq_dennys_all, width = 7, height = 7, units = 'cm', scale = 2, dpi = 600)  

# Common locations
p +
  geom_point(data = dat_common, aes(x = lon, y = lat, color = franchise), alpha = 0.2, size = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(title = "LaQuinta and Denny's locations in the USA", subtitle = "Common cities") +
  theme_void() -> map_lq_dennys_common

ggsave(here::here("viz/map-laquinta-dennys-common.png"), plot = map_lq_dennys_common, width = 7, height = 7, units = 'cm', scale = 2, dpi = 600)  



# Spatial analysis --------------------------------------------------------

# Split data by city
dat_split <- dat_common %>%
  select(franchise, lon, lat, city_state) %>%
  group_split(city_state)

# Compute distances in each common city

dist_list <- map(dat_split, function(df){
  
  i_lst <- map(c("La Quinta", "Denny\'s"), ~ which(.x == df$franchise))
  i <- expand.grid(i_lst[[1]], i_lst[[2]])
  dist_matrix <- geosphere::distm(as.matrix(df[, c("lon", "lat")]))
  map2_dbl(.x = i[,1], .y = i[,2], ~ dist_matrix[.x, .y])
  
})


# Visualize distribution of distances

qplot(x = flatten_dbl(dist_list), geom = "density")
