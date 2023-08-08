# Load the library
library(terra)

# Create a data frame with lat and lon columns
df <- data.frame(
  lat = c(45.5017, 51.5074, 40.7128),
  lon = c(-73.5673, -0.1278, -74.0060),
  location = c("Montreal", "London", "New York")
)

print(df)
#        lat      lon location
# 1  45.5017 -73.5673 Montreal
# 2  51.5074  -0.1278   London
# 3  40.7128 -74.0060 New York

# Convert the data frame to a SpatVector
v <- vect(df)

# Set the CRS if you know it (e.g., WGS84)
crs(v) <- "EPSG:4326"

#once vector file is created, extract the values from raster

#create a stack with all rasters
file_names=c()
raster_stack=rast(file_names)

#ensure that geographic projections match between raster and vect file

#then use extract function in terra to get values for the sites
