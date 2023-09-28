# The data processing involved loading and preparing two key datasets - outplant data and wild occurrence data - to support habitat suitability modeling.
# The outplant data (polbis.data.csv) contained rows for each individual outplanted sapling with coordinates and vigor status. This was summarized to get mean coordinates and proportion of healthy plants per outplant site.
# The wild occurrence data (polbis.wild.locs_corrected_coords.csv) contained locations of wild P. bisattenuata individuals. This was also summarized to get mean coordinates per site.
# Two final wild site datasets where created: one using all available wild sites; and one where one wild site (Makaleha) was excluded due to genetic evidence it may be a distinct subspecies.
# The outplant site data was supplemented with coordinates from matching wild sites where needed.
# Two final outplant site datasets were created - one with all sites, and one subset to sites with >50% healthy plants.
# Climate rasters (temperature, precipitation, cloud cover) were processed and cropped to the extent of Kauai island for two time periods - 2017-2021 covering the outplanting study period, and 1991-2020 representing long-term climate conditions.
# The final multi-raster stacks for each time period were saved as GeoTIFFs to provide predictor variables for habitat suitability modeling.
# The data processing steps prepared the necessary data frames of site points and environmental predictor rasters for subsequent habitat suitability modeling and analysis.

repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" 
REPO_dirs=c(repo_path1, repo_path2) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

library(terra)
library(dplyr)

#########################
#process outplant data, with rows for individual plants and associated coordinates
#load outplant data
outplant_df=read.csv("data/polbis.data.csv")
#View(outplant_df)

#####################
#summarize by site
# Assume your dataframe is named df
outplant_site_df <- outplant_df %>%
  group_by(site) %>%
  summarise(
    lat = mean(gpslatdec, na.rm = TRUE),
    lon = mean(gpslongdec, na.rm = TRUE),
    healthy_proportion = mean(X21.vigor == "healthy", na.rm = TRUE)
  )
outplant_site_df

#########################
#load location of wild plants
wild_df=read.csv("data/polbis.wild.locs_corrected_coords.csv")
# View(wild_df)
#####################
#summarize by site
# Assume your dataframe is named df
wild_site_df <- wild_df %>%
  group_by(site) %>%
  summarise(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE)
  )
#View(wild_site_df)
write.csv(wild_site_df, "data/SDM_data/wild_site_df.csv", row.names = F)

#create data subset excluding Makaleha given the large genetic differences indicating wild plants in that location may be a distinct subspecies 

wild_site_no_makaleha_df=wild_site_df[wild_site_df$site!="Makaleha",]
write.csv(wild_site_no_makaleha_df, "data/SDM_data/wild_site_no_makaleha_df.csv", row.names = F)

############################
#add missing data to outplant sites using wild species info for same sites
outplant_site_df[outplant_site_df$site=="Laauhihaihai", c("lat", "lon")]=wild_site_df[wild_site_df$site=="Laauhihaihai(gulch)", c("lat", "lon")]
outplant_site_df[outplant_site_df$site=="PuuKolo", c("lat", "lon")]=wild_site_df[wild_site_df$site=="PuuKolo", c("lat", "lon")]

write.csv(outplant_site_df, "data/SDM_data/outplant_site_df.csv", row.names = F)

#create a subset of outplant sites that only include sites where the majority of plants are healthy
healthy_outplant_site_df=outplant_site_df[outplant_site_df$healthy_proportion>0.5,]
write.csv(healthy_outplant_site_df, "data/SDM_data/healthy_outplant_site_df.csv", row.names = F)

##########################
##########################
#process spatial environmental predictors for the period during outplanting study (2017-2021), to be used for modeling outplant locations
period=c(2017:2021)
MAP_raster=rast(paste0("data/climate.rasters/rain_yearly_1990_2023/", period, ".tif"))
MAP_raster=mean(MAP_raster)
MAT_raster=rast(paste0("data/climate.rasters/Tmean_yearly_1990_2023/", period, ".tif"))
MAT_raster=mean(MAT_raster)
clouds_2pm=rast("data/climate.rasters/cl_frq_ann_14")
clouds_2pm=terra::project(clouds_2pm, MAP_raster)

pred_stack=c(MAT_raster, MAP_raster, clouds_2pm)
Kauai = c(-159.82,-159.26, 21.84, 22.25)
pred_stack=crop(pred_stack, Kauai)
pred_stack[[1]]=pred_stack[[1]]/10 #temperature was in decimal degrees
names(pred_stack)=c("MAT", "MAP", "Clouds")

plot(pred_stack)

writeRaster(pred_stack, "data/SDM_data/predictor_stack_MAT_MAP_CC_2017_2021.tif", gdal=c("compress=LZW"), overwrite=T)

#process spatial environmental predictors for 30 years to characterize long term climate and to be used for modeling wild plant locations
period=c(1991:2020)
MAP_raster=rast(paste0("data/climate.rasters/rain_yearly_1990_2023/", period, ".tif"))
MAP_raster=mean(MAP_raster)
MAT_raster=rast(paste0("data/climate.rasters/Tmean_yearly_1990_2023/", period, ".tif"))
MAT_raster=mean(MAT_raster)
clouds_2pm=rast("data/climate.rasters/cl_frq_ann_14")
clouds_2pm=terra::project(clouds_2pm, MAP_raster)

pred_stack=c(MAT_raster, MAP_raster, clouds_2pm)
Kauai = c(-159.82,-159.26, 21.84, 22.25)
pred_stack=crop(pred_stack, Kauai)
pred_stack[[1]]=pred_stack[[1]]/10 #temperature was in decimal degrees
names(pred_stack)=c("MAT", "MAP", "Clouds")

plot(pred_stack)
writeRaster(pred_stack, "data/SDM_data/predictor_stack_MAT_MAP_CC_1991_2020.tif", gdal=c("compress=LZW"), overwrite=T)


########################
#check results
plot(pred_stack[[1]])
points(outplant_site_df[, c("lon", "lat")], pch = 19, cex = 0.5)
text(x = outplant_site_df$lon, y = outplant_site_df$lat, # Adding labels
     cex = 0.7, labels = outplant_site_df$site, pos = 3) # To position the text above the point, you can change this to adjust position

plot(pred_stack[[1]])
points(wild_site_df[, c("lon", "lat")], pch = 19, cex = 0.5)
text(x = wild_site_df$lon, y = wild_site_df$lat, # Adding labels
     cex = 0.7, labels = wild_site_df$site, pos = 3) # To position the text above the point, you can change this to adjust position

# #tmp
# tmp_extract=terra::extract(pred_stack, outplant_site_df[, c('lon', 'lat')], cells=T)
# outplant_site_df=cbind(outplant_site_df, tmp_extract)
# View(outplant_site_df)
# outplant_site_df = outplant_site_df %>% select(-cell)


