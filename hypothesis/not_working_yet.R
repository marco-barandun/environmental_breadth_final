library(ggplot2)
library(maps)
library(sf)
library(rnaturalearth)

# Prepare world land data
world_land <- ne_countries(scale = "medium", returnclass = "sf")
world_land <- subset(world_land, !name %in% c("Antarctica"))

set.seed(123)
n_shapes <- 5000

# Generate points with normal distribution in latitude
latitudes <- rnorm(n_shapes, mean = 0, sd = 50)  # Adjust sd for spread
# Generate random longitudes
longitudes <- runif(n_shapes, min = -180, max = 180)

# Combine into a data frame
shapes <- data.frame(lon = longitudes, lat = latitudes)

# Convert points to sf object and filter to keep only those on land
shapes_sf <- st_as_sf(shapes, coords = c("lon", "lat"), crs = st_crs(world_land))
world_land_valid <- sf::st_make_valid(world_land)
shapes_on_land <- st_intersection(shapes_sf, world_land_valid)

size_function <- function(lat) {
  # Normalize latitude: 0 at south pole, 90 at equator, 180 at north pole
  normalized_lat <- lat + 90
  
  # Adjusted sine wave for both hemispheres
  if (lat < 0) {
    # Southern Hemisphere: Increase size from the south pole, peaking around 60°S, then decrease towards the equator
    size <- sin((normalized_lat - 30) * pi / 180)
  } else {
    # Northern Hemisphere: Increase size from the equator to the north pole
    size <- sin(lat * pi / 90)
  }
  
  # Normalize size to be between 0 and 1
  size <- (size + 1) / 10
  
  return(size)
}

# Apply the size function to each shape
shapes_on_land$size <- sapply(st_coordinates(shapes_on_land)[,2], size_function)

# Plot the map with shapes
ggplot() +
  geom_sf(data = world_land, fill = "lightblue") +
  geom_sf(data = shapes_on_land, aes(size = size), shape = 21, fill = "red", alpha = 0.5) +
  coord_sf() +
  theme_void()



