#set up working directory for project
wd="D:/projects/2021_ntbg/"
setwd(wd)

#load necessary libraries
library(terra)
data_dir="D:/projects/2021_spatial_prevalence/data/processed/"
data_subdirs=list.dirs(data_dir)
re_extract_data=T

periods=c("annual", "dry", "wet")
lists_collection_month=list(c(1:12), c(5:10), c(1:4, 11, 12))

template_raster=rast(paste0("D:/data/higap_data/CAH_HabStatus/distance_to_disturbance_90m.tif"))

cl_month_preds=c("cl_frq_01", "cl_frq_02", "cl_frq_03", "cl_frq_04", "cl_frq_05", 
                 "cl_frq_06", "cl_frq_07", "cl_frq_08", "cl_frq_09", "cl_frq_10", 
                 "cl_frq_11", "cl_frq_12") 
rh_month_preds=c("rh_01", "rh_02", "rh_03", "rh_04", "rh_05", "rh_06", "rh_07", 
                 "rh_08", "rh_09", "rh_10", "rh_11", "rh_12") 
r_net_month_preds=c("r_net_01", "r_net_02", "r_net_03", "r_net_04", "r_net_05", "r_net_06", "r_net_07", "r_net_08", 
                    "r_net_09", "r_net_10", "r_net_11", "r_net_12")
cl_sw_month_preds=c("cl_sw_01", "cl_sw_02", "cl_sw_03", "cl_sw_04", "cl_sw_05", "cl_sw_06", "cl_sw_07", "cl_sw_08", 
                    "cl_sw_09", "cl_sw_10", "cl_sw_11", "cl_sw_12") #solar radiation
staterf_mm_month_preds=c("staterf_mm_01", 
                         "staterf_mm_02", "staterf_mm_03", "staterf_mm_04", "staterf_mm_05", 
                         "staterf_mm_06", "staterf_mm_07", "staterf_mm_08", "staterf_mm_09", 
                         "staterf_mm_10", "staterf_mm_11", "staterf_mm_12")
tmax_month_preds=c("tmax_01", "tmax_02", "tmax_03", "tmax_04", "tmax_05", "tmax_06", 
                   "tmax_07", "tmax_08", "tmax_09", "tmax_10", "tmax_11", "tmax_12")
tmin_month_preds=c("tmin_01", "tmin_02", "tmin_03", "tmin_04", "tmin_05", "tmin_06", 
                   "tmin_07", "tmin_08", "tmin_09", "tmin_10", "tmin_11", "tmin_12")

wind_hourly=c("wind_sd_01", "wind_sd_02", "wind_sd_03", "wind_sd_04", "wind_sd_05", "wind_sd_06", "wind_sd_07",
              "wind_sd_08", "wind_sd_09", "wind_sd_10", "wind_sd_11", "wind_sd_12",
              "wind_sd_13", "wind_sd_14", "wind_sd_15", "wind_sd_16", "wind_sd_17",
              "wind_sd_18", "wind_sd_19", "wind_sd_20", "wind_sd_21", "wind_sd_22",
              "wind_sd_23", "wind_sd_24")
preds_monthly_sets=c(cl_month_preds, rh_month_preds, r_net_month_preds, cl_sw_month_preds, staterf_mm_month_preds, tmax_month_preds, tmin_month_preds)
preds_monthly_set_names=c("cl", "rh", "r_net", "cl_sw", "staterf_mm", "tmax", "tmin")


if (re_extract_data){
  #get datasheet with coordinates
  site_DF=read.csv("data/Site Level GPS Points2.csv")
  xy=site_DF[,c(1:2)]
  plot(xy) #spans two projections (zone 4 and 5)
  
  
  library(sf)
  library(sp)
  spdf <- SpatialPointsDataFrame(coords = xy, data = site_DF,
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  
  site_spdf=spTransform(spdf, CRS("+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs"))
  xy=as.data.frame(coordinates(site_spdf))
  
  library(rgdal)
  site_GIS_data_file=paste0("data/site_coordinates.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
  if (file.exists(site_GIS_data_file)){file.remove(site_GIS_data_file)}
  writeOGR(obj=site_spdf, dsn=site_GIS_data_file, layer="site_coordinates", driver="GPKG")
  
  
  data_subdir = data_subdirs[7]
  for (data_subdir in data_subdirs){
    cat("doing ", data_subdir, "\n")
    raster_files=list.files(data_subdir, pattern = ".tif", full.names = T)
    raster_files=grep(raster_files, pattern = "_metric.tif$", invert = T, value = T)
    all_dir_raster=rast(raster_files)
    raster_names=basename(raster_files)
    raster_names=gsub(raster_names, pattern = ".tif", replacement = "")
    names(all_dir_raster)=raster_names
    #names(all_dir_raster)
    tmp_extracted_data <- terra::extract(all_dir_raster, xy, na.rm=T)
    if (data_subdir == data_subdirs[1]){
      extracted_data=tmp_extracted_data
    }else{
      extracted_data=cbind(extracted_data, tmp_extracted_data[,-1])
    }
    
    #create metric rasters for spatial projections
    period = periods[2]
    if (length(grep(pattern = "month_raster$", x=data_subdir))>0){
      cat("doing projection rasters for ", data_subdir, "\n")
      for (period in periods){
        raster_stack=rast(raster_files[lists_collection_month[[which(period==periods)]]])
        raster_mean=mean(raster_stack, na.rm=T) #mean
        raster_min=min(raster_stack, na.rm=T) #min
        raster_max=max(raster_stack, na.rm=T) #max
        
        var_name=sub(pattern = "_month_raster$", replacement="", x=basename(data_subdir))  
        
        dir.create(paste0(data_dir, "projection_rasters/"), showWarnings = F)
        file_name=paste0(data_dir, "projection_rasters/", var_name, "_", period, "_max.tif")
        writeRaster(raster_max, file_name, overwrite=T,  gdal=c("COMPRESS=LZW"))
        
        file_name=paste0(data_dir, "projection_rasters/", var_name, "_", period, "_min.tif")
        writeRaster(raster_min, file_name, overwrite=T,  gdal=c("COMPRESS=LZW"))
        
        file_name=paste0(data_dir, "projection_rasters/", var_name, "_", period, "_mean.tif")
        writeRaster(raster_mean, file_name, overwrite=T,  gdal=c("COMPRESS=LZW"))
      }
    }
  }
  
  View(extracted_data)
  
  extracted_data=cbind(site_spdf@data, extracted_data)
  site_spdf@data=extracted_data
  View(site_spdf@data)
  dput(names(extracted_data))
  # c("X", "Y", "Site", "Native.n", "Native.pos", "Amakhi.n", "Amakihi.pos", 
  #   "Location", "Location2", "Island", "Elevation", "Year", "Start.date", 
  #   "End.date", "Year2", "St.date.2", "End.date.2", "ID", "dist_to_all_streams") 
  
  stream_preds=c("dist_to_non_perennial", "dist_to_perennial", "fenced_raster")
  habitat_qual_preds=c("distance_to_bad_habitat_90m", "distance_to_disturbance_90m")
  substrate_preds=c("ash_raster_wgs84", "HI_substrate_age_wgs84_aligned", "HI_substrate_age_wgs84_aligned_reclass")
  topo_preds=c("aspect", "flowdir", "roughness", "slope", "TPI", "TRI", "islands") 
  wind_preds=c("wind_max_24hr", "wind_max_day", "wind_max_night")
  
  vector_output_file=paste0("data/site_spatialVector_w_preds_logDist.gpkg") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
  if (file.exists(vector_output_file)){file.remove(vector_output_file)}
  library(rgdal)
  writeOGR(obj=site_spdf, dsn=vector_output_file, layer="prev_DF", driver="GPKG") # this is in geographical projection
  
  DF_output_file=paste0("data/site_df_w_preds_logDist.csv") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
  write.csv(extracted_data, DF_output_file, row.names = F)
  DF_output_rfile=paste0("data/site_df_w_preds_logDist.Rdata") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
  save(list = c("extracted_data"), file = DF_output_rfile) #
  
  
}else{
  #extracted_data=read.csv(paste0("data/processed/site_df_w_preds.csv")) #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
  load(paste0("data/processed/site_df_w_preds.Rdata")) #extracted_data
}

# ###############################################
# #now get month specific data
# #DF_output_file=paste0("data/site_df_w_preds.csv") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
# #extracted_data=read.csv(DF_output_file)
# #View(extracted_data)
# extracted_data$Start.date=strptime(as.character(extracted_data$Start.date),format="%m/%d/%Y")
# extracted_data$End.date=strptime(as.character(extracted_data$End.date),format="%m/%d/%Y")
# library(lubridate)
# extracted_data$Start.month=month(extracted_data$Start.date)
# extracted_data$End.month=month(extracted_data$End.date)
# 
# ######
# #find out the months encompased on each collection effort
# #I could have vectorized this but was lazy :)
# list_collection_months=list()
# for (i in c(1:dim(extracted_data)[1])){
#   collection_months=seq(from=floor_date(extracted_data$Start.date[i], "month"), to=floor_date(extracted_data$End.date[i], "month"), by = "month")
#   collection_months=month(collection_months)
#   list_collection_months[[i]]=collection_months
# }
# 
# preds_monthly_set_name = preds_monthly_set_names[1]
# i=12
# pred_monthly_sets=c()
# for (preds_monthly_set_name in preds_monthly_set_names){
#   cat("doing ", preds_monthly_set_name, "\n")
#   pred_monthly_set=get(paste0(preds_monthly_set_name, "_month_preds"))
#   pred_vals=c()
#   for (i in c(1:dim(extracted_data)[1])){
#     cat("doing obs ", i, "\n")
#     list_collection_month=list_collection_months[[i]]
#     month_vals=extracted_data[i, pred_monthly_set[list_collection_month]]
#     mean_month_vals=mean(as.numeric(month_vals), na.rm=T)
#     pred_vals=c(pred_vals, mean_month_vals)
#   }
#   extracted_data[,preds_monthly_set_name]=pred_vals
# }

################################
#calc annual, seasonal means
#yearly
list_collection_month=c(1:12)

period = periods[1]
preds_monthly_set_name = preds_monthly_set_names[1]
for (period in periods){
  list_collection_month=lists_collection_month[[which(period==periods)]]
  for (preds_monthly_set_name in preds_monthly_set_names){
    cat("doing ", preds_monthly_set_name, "\n")
    pred_monthly_set=get(paste0(preds_monthly_set_name, "_month_preds"))
    
    mean_vals=apply(extracted_data[, pred_monthly_set[list_collection_month]], 1, mean, na.rm=T)
    min_vals=apply(extracted_data[, pred_monthly_set[list_collection_month]], 1, min, na.rm=T)
    max_vals=apply(extracted_data[, pred_monthly_set[list_collection_month]], 1, max, na.rm=T)
    
    extracted_data[,paste0(preds_monthly_set_name, "_", period, "_mean")]=mean_vals
    extracted_data[,paste0(preds_monthly_set_name, "_", period, "_min")]=min_vals
    extracted_data[,paste0(preds_monthly_set_name, "_", period, "_max")]=max_vals
  }
}


#View(extracted_data)
#names(extracted_data)
extracted_data_reduced=extracted_data
extracted_data_reduced=extracted_data_reduced[,!(names(extracted_data_reduced) %in% c(preds_monthly_sets, wind_hourly))]
#View(extracted_data_reduced)
#dput(names(extracted_data_reduced))

DF_output_file=paste0("data/site_df_w_preds_simplified_by_collection_period_logDist.csv") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
write.csv(extracted_data_reduced, DF_output_file, row.names = F)

DF_output_file=paste0("data/site_df_w_preds_simplified_by_collection_period_logDist.Rdata") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
save("extracted_data_reduced", file = DF_output_file)

##########################
#reduced preds without collection specific
extracted_data_reduced_no_collection_period_preds=extracted_data_reduced[,!(names(extracted_data_reduced)%in%preds_monthly_set_names)]
#names(extracted_data_reduced_no_collection_period_preds)

DF_output_file=paste0("data/site_df_w_preds_simplified_no_collection_period_logDist.csv") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
write.csv(extracted_data_reduced_no_collection_period_preds, DF_output_file, row.names = F)

DF_output_file=paste0("data/site_df_w_preds_simplified_no_collection_period_logDist.Rdata") #paste0(wd, shp_outdir, sp_name, img_name, ".gpkg")
save("extracted_data_reduced_no_collection_period_preds", file = DF_output_file)
