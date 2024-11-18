## DO NOT EDIT
## This file was used to combine the LMPs and cities in order to assign an LMP to every city. The final csv was exported as cities_and_lmps.csv which is used in other files.

library(geosphere)

cities_df = read.csv("data/cal_cities_lat_long.csv")
lmps_df = read.csv("data/LMPLocations.csv")
names(cities_df) = tolower(names(cities_df))

# Create a matrix of city coordinates
city_coords = as.matrix(cities_df[, c("longitude", "latitude")])

# Create a matrix of LMP coordinates
lmp_coords = as.matrix(lmps_df[, c("longitude", "latitude")])

# Calculate distances for all city-LMP pairs
distance_matrix = outer(
  1:nrow(city_coords),
  1:nrow(lmp_coords),
  Vectorize(function(i, j) distHaversine(city_coords[i, ], lmp_coords[j, ]))
)

# Find the closest LMP and its distance for each city
cities_df$closest_lmp = lmps_df$name[apply(distance_matrix, 1, which.min)]
cities_df$closest_distance = apply(distance_matrix, 1, min)

write.csv(cities_df, "data/cities_and_lmps.csv", row.names = FALSE)
