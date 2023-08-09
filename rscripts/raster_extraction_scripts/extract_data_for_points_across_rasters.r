library(terra)

data_dir="D:/data/climate_data/HCDP_data/rain_monthly_1990_2023/data/"
polygon_file_name="D:/projects/2021_ntbg/data/bysite.coordinates.gpkg"
output_name="D:/projects/2021_ntbg/data/Precip_monthly_data.csv" #_degreesCx10

data_dir="D:/data/climate_data/HCDP_data/Tmean_monthly_1990_2023/data/"
polygon_file_name="D:/projects/2021_ntbg/data/bysite.coordinates.gpkg"
output_name="D:/projects/2021_ntbg/data/Tmean_monthly_data_degreesCx10.csv" #

data_dir="D:/data/climate_data/HCDP_data/Tmax_monthly_1990_2023/data/"
polygon_file_name="D:/projects/2021_ntbg/data/bysite.coordinates.gpkg"
output_name="D:/projects/2021_ntbg/data/Tmax_monthly_data_degreesCx10.csv" #

# data_dir="D:/data/climate_data/2019_UH_clim_data/P_month_1990_2022/data_map/"
all_tifs=list.files(data_dir, pattern=".tif$", full.names = T, recursive = T)
AOI_pol=vect(polygon_file_name)
AOI_pol=project(AOI_pol, rast(all_tifs[1])) #make sure polygon file has the same geographical projection

library(tidyr)
img_name = all_tifs[1]
for (img_name in all_tifs){
  img=rast(img_name)
  simple_img_name=gsub(x = img_name, pattern = "^.*/", replacement = "")
  simple_img_name=gsub(x = simple_img_name, pattern = ".tif$", replacement = "")
  extracted_vals=terra::extract(img, AOI_pol, fun=mean, method="simple", touches=T)
  extracted_vals=extracted_vals[,2]
  # variables=AOI_pol$name #name of polygons
  variables=paste0("site", c(1:nrow(AOI_pol))) #name of polygons
  
  extracted_vals_df=tibble(!!!setNames(extracted_vals, variables)) #https://stackoverflow.com/questions/69883317/how-to-create-a-one-row-data-frame-tibble-from-2-vectors-one-with-the-desired
  extracted_vals_df=cbind(simple_img_name, extracted_vals_df)
  
  if (img_name == all_tifs[1]){
    all_extracted_vals_df=extracted_vals_df
  }else{
    all_extracted_vals_df=rbind(all_extracted_vals_df, extracted_vals_df)
    
  }
  cat("done with ", simple_img_name, "\n")
}

# View(all_extracted_vals_df)

write.csv(all_extracted_vals_df, output_name, row.names = F)