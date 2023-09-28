# Species distribution models (SDMs) were created using the R package flexsdm. Due to the limited number of occurrence records for P. bisattenuata, an ensemble of small models (ESM) approach was used, which has been shown to perform well for species with sparse data.
# A 5km buffer was created around occurrence points to define the geographic background sampling area. 100 background points were randomly sampled per presence point within this buffer area.
# Occurrence data was thinned to remove duplicate points within the same raster cells to minimize spatial autocorrelation. Models were fit using the thinned data if at least 5 non-overlapping points remained.
# Four model types were fit using the thinned occurrence data and sampled background points:
# ESM with generalized linear models (GLM)
# ESM with generalized additive models (GAM)
# Tuned ESM using maximum entropy model (Maxent)
# Tuned ESM using boosted regression trees (GBM)
# The Maxent and GBM models were tuned using 10-fold cross-validation over hyperparameter grids to optimize model performance.
# All models used k-fold cross validation with 4 folds and 10 replicates for partitioning and model evaluation. Models were fit using three predictor variables: mean annual temperature, mean annual precipitation, and cloud cover frequency.
# Model performance was evaluated using the mean TSS statistic from cross-validation. Thresholds were defined based on maximizing TSS.
# Final model predictions were made across the extent of Kauai island based on the raster stacks of environmental predictors, resulting in habitat suitability maps and binary suitable/unsuitable maps.

repo_path1="D:/projects/2021_ntbg/polbis_analysis/"
repo_path2="~/Desktop/ohe.mauka/Polyscias Kauai 2022/polbis.analysis/" 
REPO_dirs=c(repo_path1, repo_path2) 
repo_dir=REPO_dirs[min(which(dir.exists(REPO_dirs)))] 
setwd(repo_dir)

set.seed(10)
model_type="wild_noMak" #outplant wild healthy_outplant wild_noMak

library(dplyr)
library(terra)
library(flexsdm)

#if modeling outplant data, use environmental predictors that represent the period of outplant study, if not, use long term environmental predictors
if (model_type=="outplant"){
  sp_data=read.csv("data/SDM_data/outplant_site_df.csv")
  sdm_output_dir="analysis/sdm_outputs/outplants/"
  pred_stack=rast("data/SDM_data/predictor_stack_MAT_MAP_CC_2017_2021.tif")
}
if (model_type=="healthy_outplant"){
  sp_data=read.csv("data/SDM_data/healthy_outplant_site_df.csv")
  sdm_output_dir="analysis/sdm_outputs/healthy_outplants/"
  pred_stack=rast("data/SDM_data/predictor_stack_MAT_MAP_CC_2017_2021.tif")
}
if (model_type=="wild"){
  sp_data=read.csv("data/SDM_data/wild_site_df.csv")
  sdm_output_dir="analysis/sdm_outputs/wildlocs/"
  pred_stack=rast("data/SDM_data/predictor_stack_MAT_MAP_CC_1991_2020.tif")
}
if (model_type=="wild_noMak"){
  sp_data=read.csv("data/SDM_data/wild_site_no_makaleha_df.csv")
  sdm_output_dir="analysis/sdm_outputs/wildlocs_noMak/"
  pred_stack=rast("data/SDM_data/predictor_stack_MAT_MAP_CC_1991_2020.tif")
}

sp_data
proj_str="v20230928"
#############################
# SDMs were created using package flexsdm:
# package info below:
# Velazco, S.J.E., Rose, M.B., Andrade, A.F.A., Minoli, I., Franklin, J. (2022). flexsdm: An R package for supporting a comprehensive and flexible species distribution modelling workflow. Methods in Ecology and Evolution, 13(8) 1661-1669. https://doi.org/10.1111/2041-210X.13874
# Given the small number of records for the species, we used ESM (ensemble of small models) approach, which have been shown to work best for species with limited data.
# Breiner F.T., A. Guisan, A. Bergamini and M.P. Nobis. 2015. Overcoming limitations of modelling rare species by using ensembles of small models. Methods in Ecology and Evolution, 6,1210-1218.
# Breiner F.T., Nobis M.P., Bergamini A., Guisan A. 2018. Optimizing ensembles of small models for predicting the distribution of species with few occurrences. Methods in Ecology and Evolution. https://doi.org/10.1111/2041-210X.12957

sp_data <- sp_data %>% rename(y = lat,x = lon)
sp_data = sp_data %>% select(c(x, y, site))
dir.create(sdm_output_dir, showWarnings = F, recursive = T)

#####################
#geographic region
cat("setting calibration area \n")
cores_for_parallel=8

region_rast=pred_stack[[1]]
Kauai = c(-159.82,-159.26, 21.84, 22.25)
region_rast=crop(region_rast, Kauai)
region_rast=region_rast>=0
plot(region_rast)
regions=as.polygons(region_rast)
regions=project(regions,"epsg:4326")
# plot(regions, col="black")

#important: using a 5 km buffer around presence points to generate background points
calibration_area <- calib_area(
  data = sp_data,
  x = "x",
  y = "y",
  method = c("buffer", width = 5000), 
  crs = crs(regions)
)

plot(regions, main = "")
plot(calibration_area, add = TRUE)
# plot(ca_1, add = TRUE)
points(sp_data[, c("x", "y")], pch = 19, cex = 0.5)

############################
#data filtering (thinning)
#environmental filtering based on overlapping points: remove duplicate points within the same raster cells
# sp_data$idd <- 1:nrow(sp_data)
cat("data filtering \n")

tmp=extract(pred_stack, y = sp_data[,c("x", "y")], cells=T)
cat(paste0("number of non overlapping points: ", length(unique(tmp$cell)), "\n"))
thinned_sp_data=sp_data[!duplicated(tmp$cell),]

if (length(unique(tmp$cell))>=5){
  ########################
  #data partitioning (training and evaluation)
  thinned_sp_data$pr_ab <- 1 # Add a column with 1 to denote that this is presences only data
  
  #####################
  #background and pa sampling:
  cat("PA sampling \n")
  
  #create pseudo absence points 100 times the number of presence points 
  set.seed(10)
  psa <- sample_pseudoabs(
    data = thinned_sp_data,
    x = "x",
    y = "y",
    n = nrow(thinned_sp_data)*100, # selecting number of pseudo-absence points that is equal to number of presences
    method = "random",
    rlayer = pred_stack,
    calibarea = calibration_area
  )
  
  #plot final data used
  output_name=paste0(sdm_output_dir, proj_str, "_occ_and_psa_points.jpg")
  jpeg(output_name, width = 900, height = 700)
  plot(pred_stack[[1]], main = "Presence points")
  points(tibble(psa[,c("x", "y")]), cex = .5, pch = 19, col="grey")
  points(tibble(thinned_sp_data[,c("x", "y")]), cex = .7, pch = 19)
  plot(calibration_area, add = TRUE)
  plot(regions, add=T)
  text(x = thinned_sp_data$x, 
       y = thinned_sp_data$y, 
       labels = thinned_sp_data$site, 
       pos = 3, # To position the text above the point, you can change this to adjust position
       cex = 0.7) # Text size
  
  dev.off()
  
  #########################
  #extract environmental predictors for both presence and background points
  cat("extract vars \n")
  thinned_sp_data = thinned_sp_data %>% select(-site)
  all_points <- bind_rows(tibble(thinned_sp_data), psa)
  
  ex_spp <- sdm_extract(
    data = all_points,
    x = "x",
    y = "y",
    env_layer = pred_stack, # Raster with environmental variables
    variables = NULL, # Vector with the variable names of predictor variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
    filter_na = TRUE
  )
  
  #remove incomplete cases
  ex_spp=ex_spp[complete.cases(ex_spp),]
  ex_spp
  
  ################################################################
  ################################################################
  ################################################################
  #################################################################
  #model fitting
  ###################################
  # fit models using rep_kfold partition using 4 folds and 10 replicates
  set.seed(10)
  ex_spp2 <- part_random(
    data = ex_spp,
    pr_ab = "pr_ab",
    method = c(method = "rep_kfold", folds = 4, replicates = 10)
  )
  ex_spp2
  
  ########################################
  #model fitting
  ########################################
  save_spatial_preds=function(model_name){
    spat_pred <- sdm_predict(models = get(model_name), pred = pred_stack, thr = "max_sens_spec", con_thr = FALSE, predict_area = NULL)
    #plot(spat_pred)
    # Plot to see this layers
    spat_pred_rst <- terra::rast(spat_pred)
    plot(spat_pred_rst)
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_suitability.jpg")
    jpeg(output_name, width = 900, height = 700)
    plot(spat_pred_rst[[1]])
    points(tibble(thinned_sp_data[,c("x", "y")]), cex = .7, pch = 19)
    dev.off()
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_bin.jpg")
    jpeg(output_name, width = 900, height = 700)
    plot(spat_pred_rst[[2]])
    points(tibble(thinned_sp_data[,c("x", "y")]), cex = .7, pch = 19)
    dev.off()
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_suitability.tif")
    writeRaster(spat_pred_rst[[1]], output_name, overwrite=T, gdal=c("compress=LZW"))
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_bin.tif")
    writeRaster(spat_pred_rst[[2]], output_name, overwrite=T, gdal=c("compress=LZW"))
  }
  
  ##########################
  ##########################
  ##########################
  ###glm
  cat("model fitting \n")
  model_name="esm_glm"
  cat("Fitting", model_name, "\n")
  
  esm_glm_ran=tryCatch({ # Error-prone code wrapped in tryCatch
    esm_glm <- esm_glm(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL
    )
    # esm_gam$esm_model
    # esm_gam$predictors
    # esm_gam$performance
    esm_glm$performance[,c("threshold", "TSS_mean")]
    #View(esm_gam$performance)
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_performance.csv")
    write.csv(esm_glm$performance, output_name, row.names = F)
    #################################
    #spatial prediction
    save_spatial_preds(model_name)
    model_ran=T
  }, warning = function(war) {
    cat("Caught a warning running", model_name, "\n")
    model_ran=T
    return(model_ran)
  }, error = function(err) {
    cat("Caught an error running", model_name, "\n")
    model_ran=F
    return(model_ran)
  }, finally = {
    cat("Executed trycatch block \n") # This code will run regardless of whether there was an error or not
  })
  
  ##########################
  ##########################
  ##########################
  ###gam
  model_name="esm_gam"
  cat("Fitting", model_name, "\n")
  esm_gam_ran=tryCatch({ # Error-prone code wrapped in tryCatch
    esm_gam <- esm_gam(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL
    )
    # esm_gam$esm_model
    # esm_gam$predictors
    # esm_gam$performance
    esm_gam$performance[,c("threshold", "TSS_mean")]
    #View(esm_gam$performance)
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_performance.csv")
    write.csv(esm_gam$performance, output_name, row.names = F)
    #################################
    #spatial prediction
    save_spatial_preds(model_name)
    model_ran=T
  }, warning = function(war) {
    cat("Caught a warning running", model_name, "\n")
    model_ran=T
    return(model_ran)
  }, error = function(err) {
    cat("Caught an error running", model_name, "\n")
    model_ran=F
    return(model_ran)
  }, finally = {
    cat("Executed trycatch block \n") # This code will run regardless of whether there was an error or not
  })
  
  ###########################################
  ###########################################
  ###########################################
  ##tunned GBM 
  model_name="tuned_esm_MAX"
  cat("Fitting", model_name, "\n")
  tuned_esm_MAX_ran=tryCatch({ # Error-prone code wrapped in tryCatch
    
    # Hyper-parameter values for tuning maxent model
    tune_grid <-
      expand.grid(
        regmult = seq(0.1, 3, 0.5),
        classes = c("l", "lq", "lqh")
      )
    
    MAX_t=tune_max(data = ex_spp2,
                   response = "pr_ab",
                   predictors = c("MAT", "MAP", "Clouds"),
                   partition = ".part",
                   grid=tune_grid,
                   thr = NULL,
                   n_cores=cores_for_parallel)
    
    # # Outputs
    # MAX_t$model
    # MAX_t$predictors
    # MAX_t$performance
    # #View(MAX_t$performance)
    # MAX_t$hyper_performance
    # MAX_t$data_ens
    
    ##now run ESM Maxent model with fine tuned parameters
    tuned_esm_MAX <- esm_max(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL,
      regmult=MAX_t$performance$regmult,
      classes=MAX_t$performance$classes
    )
    # tuned_esm_MAX$esm_model
    # tuned_esm_MAX$predictors
    # tuned_esm_MAX$performance
    tuned_esm_MAX$performance[,c("threshold", "TSS_mean")]
    #View(tuned_esm_MAX$performance)
    
    #untuned for comparison
    ##tuned MAX
    esm_MAX <- esm_max(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL
    )
    esm_MAX$performance[,c("threshold", "TSS_mean")]
    
    #################################
    #spatial prediction
    save_spatial_preds(model_name)
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_", model_name, "_performance.csv")
    write.csv(tuned_esm_MAX$performance, output_name, row.names = F) #last_output_name
    model_ran=T
  }, warning = function(war) {
    cat("Caught a warning running", model_name, "\n")
    model_ran=T
    return(model_ran)
  }, error = function(err) {
    cat("Caught an error running", model_name, "\n")
    model_ran=F
    return(model_ran)
  }, finally = {
    cat("Executed trycatch block \n") # This code will run regardless of whether there was an error or not
  })
  
  ###########################################
  ###########################################
  ###########################################
  ##tunned GBM 
  model_name="tuned_esm_GBM"
  cat("Fitting", model_name, "\n")
  tuned_esm_GBM_ran=tryCatch({ # Error-prone code wrapped in tryCatch
    # Hyper-parameter values for tuning for GBM model
    tune_grid <-
      expand.grid(
        n.trees = c(20, 50, 100),
        shrinkage = c(0.1, 0.5, 1),
        n.minobsinnode = c(1, 3, 5, 7, 9)
      )
    
    gbm_t=tune_gbm(data = ex_spp2,
                   response = "pr_ab",
                   predictors = c("MAT", "MAP", "Clouds"),
                   partition = ".part",
                   grid=tune_grid,
                   thr = NULL,
                   n_cores=cores_for_parallel)
    
    # # Outputs
    # gbm_t$model
    # gbm_t$predictors
    # gbm_t$performance
    # #View(gbm_t$performance)
    # gbm_t$hyper_performance
    # gbm_t$data_ens
    
    # Graphical exploration of performance of each hyper-parameter setting
    graphics.off()
    require(ggplot2)
    pg <- position_dodge(width = 0.5)
    ggplot(gbm_t$hyper_performance, aes(factor(n.minobsinnode),
                                        TSS_mean,
                                        col = factor(shrinkage)
    )) +
      geom_errorbar(aes(ymin = TSS_mean - TSS_sd, ymax = TSS_mean + TSS_sd),
                    width = 0.2, position = pg
      ) +
      geom_point(position = pg) +
      geom_line(
        data = gbm_t$tune_performance,
        aes(as.numeric(factor(n.minobsinnode)),
            TSS_mean,
            col = factor(shrinkage)
        ), position = pg
      ) +
      facet_wrap(. ~ n.trees) +
      theme(legend.position = "bottom")
    
    ##tuned ESM GBM
    tuned_esm_GBM <- esm_gbm(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL,
      n_trees=gbm_t$performance$n.trees,
      n_minobsinnode=gbm_t$performance$n.minobsinnode,
      shrinkage=gbm_t$performance$shrinkage
    )
    # tuned_esm_GBM$esm_model
    # tuned_esm_GBM$predictors
    # tuned_esm_GBM$performance
    tuned_esm_GBM$performance[,c("threshold", "TSS_mean")]
    #View(tuned_esm_GBM$performance)
    
    #
    #untuned for comparison
    ##tuned GBM
    esm_GBM <- esm_gbm(
      data = ex_spp2,
      response = "pr_ab",
      predictors = c("MAT", "MAP", "Clouds"),
      partition = ".part",
      thr = NULL
    )
    esm_GBM$performance[,c("threshold", "TSS_mean")]
    
    #################################
    #spatial prediction
    save_spatial_preds(model_name)
    
    output_name=paste0(sdm_output_dir, proj_str, "_current_tuned_esm_GBM_performance.csv")
    write.csv(tuned_esm_GBM$performance, output_name, row.names = F) #last_output_name
    model_ran=T
  }, warning = function(war) {
    cat("Caught a warning running", model_name, "\n")
    model_ran=T
    return(model_ran)
  }, error = function(err) {
    cat("Caught an error running", model_name, "\n")
    model_ran=F
    return(model_ran)
  }, finally = {
    cat("Executed trycatch block \n") # This code will run regardless of whether there was an error or not
  })
  
  output_name=paste0(sdm_output_dir, proj_str, "_summary_output.csv")
  sp_model_summary=data.frame(esm_glm_ran, esm_gam_ran, tuned_esm_MAX_ran, tuned_esm_GBM_ran)
  write.csv(sp_model_summary, output_name, row.names = F)
}else{
  cat("too few points to model \n")
  # sp_model_summary=data.frame(sp_nm_str)
  # write.csv(sp_model_summary, last_output_name, row.names = F)
}