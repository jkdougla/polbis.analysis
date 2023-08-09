wd="D:/projects/2021_ntbg/"
setwd(wd)
years=c(2010:2020)
months=c(1:12)

library(stringr)
library(rgdal)
library(terra)
months=str_pad(months, 2, "left", "0")


merged_spdf=readOGR("data/site_coordinates.gpkg", layer="site_coordinates")
merged_spdf=spTransform(merged_spdf, CRS(SRS_string = "EPSG:4326"))
merged_vect=vect(merged_spdf)
merged_spdf_xy=terra::geom(merged_vect)[,c(3,4)]

merged_spdf_coords=cbind(merged_spdf_xy, merged_spdf@data)
write.csv(merged_spdf_coords, "station_coords.csv", row.names = F)

jnk=expand.grid(months, years)
month_years=paste0(jnk$Var2, "_", jnk$Var1)

#############################
#do precip
clim_directory="D:/data/climate_data/2019_UH_clim_data/P_obs_month_1920_2020_mm/"

#allocate output df to memory
ppt_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years)))
names(ppt_extracted_val_df)=month_years
#View(ppt_extracted_val_df)

year="2014"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    file_name=paste0(clim_directory, year, "_", month, ".tif")
    if (file.exists(file_name)){
      month_ppt_raster=rast(file_name)
      extracted_vector=terra::extract(month_ppt_raster, merged_spdf_xy)
    }else{
      cat("no data \n")
      extracted_vector=NA
    }
    ppt_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(ppt_extracted_val_df)=paste0("ppt_", names(ppt_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, ppt_extracted_val_df)
#View(merged_spdf@data)

#############################
#do SPI
clim_directory="D:/data/climate_data/2019_UH_clim_data/hawaii_1920_2019_spi/SPI_6mo/"

#allocate output df to memory
spi_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years)))
names(spi_extracted_val_df)=month_years
#View(spi_extracted_val_df)

year="2009"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    file_name=paste0(clim_directory, "spi6_", year, "_", month, ".tif")
    if (file.exists(file_name)){
      month_spi_raster=rast(file_name)
      extracted_vector=terra::extract(month_spi_raster, merged_spdf_xy)
    }else{
      cat("no data \n")
      extracted_vector=NA
    }
    spi_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(spi_extracted_val_df)=paste0("spi_", names(spi_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, spi_extracted_val_df)
#View(merged_spdf@data)


######################################
#now temperature
#tmean
tmean_clim_directory="D:/data/climate_data/2019_UH_clim_data/daily_T_1990_2018/processed/Tmean_daily_1990_2018/"
tmean_extracted_val_df=data.frame(matrix(NA, nrow = nrow(merged_spdf_xy), ncol = length(month_years))) #allocate output df to memory
names(tmean_extracted_val_df)=month_years

year="2021"
month="10"
for (year in years){
  for (month in months){
    cat("doing ", year, " ", month, "\n")
    raster_files=list.files(path = tmean_clim_directory, pattern = paste0("All_Islands_", year, "_", month), full.names = T)
    raster_files=grep(pattern = ".tif$", raster_files, value = T)
    if (length(raster_files)>0){
      month_clim_stack=rast(raster_files)
      extracted_vector=terra::extract(month_clim_stack, merged_spdf_xy)
      extracted_vector=apply(extracted_vector, 1, mean, na.rm=T)/100
    }else{
      cat("no data \n")
      extracted_vector=NA
    }
    tmean_extracted_val_df[,paste0(year, "_", month)]=extracted_vector
  }
}

names(tmean_extracted_val_df)=paste0("tmean_", names(tmean_extracted_val_df))
merged_spdf@data=cbind(merged_spdf@data, tmean_extracted_val_df)

###############################
###############################
#save output
names(merged_spdf@data)
View(merged_spdf@data)

site_GIS_data_file=paste0("data/station_raster_clim_info.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}
writeOGR(obj=merged_spdf, dsn=site_GIS_data_file, layer="climate_info", driver="GPKG")

write.csv(merged_spdf@data, "data/station_raster_clim_info.csv", row.names = F)


