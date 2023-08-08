# Load the library
install.packages('terra')
install.packages('rgdal')
install.packages('maptools')
install.packages("rgeos")
library(terra)
library(dplyr)
library(rgdal)
library('sp')
library('maptools')

repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" #JULIA, place your repo directory here 
REPO_dirs=c(repo_path1, repo_path2) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

#Bring in polbis data from github folder 
polbis.data.bysite <- read.csv("data/polbis.data.bysite.csv")
polbis.data.bysite <- as.data.frame(polbis.data.bysite)
str(polbis.data.bysite)

# Create a data frame with lat and lon columns
site.latlong <- polbis.data.bysite %>% select(gpslatdec, gpslongdec)
site.latlong <- subset(site.latlong[2:12,])
str(site.latlong)

print(site.latlong)

# Convert the data frame to a SpatVector
v <- vect(site.latlong, geom = c("gpslongdec", "gpslatdec"))
# Set the CRS if you know it (e.g., WGS84)
crs(v) <- "WGS84"

#once vector file is created, extract the values from raster
#create a stack with all rasters
file_names=c()
raster_stack=rast(file_names)

#ensure that geographic projections match between raster and vect file

#then use extract function in terra to get values for the sites
