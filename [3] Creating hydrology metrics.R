##############################################################################################################################
##############################################################################################################################
###[3] Creating hydrology metrics.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#####Loading libraries
library(RSAGA)
library(raster)

#####Specifying resolution
Raw_DEM_reso = 10

#####Find saga_cmd.exe file
env = rsaga.env(path = "./Raw/saga-7.0.0_x64")

#####Create directories
if(!dir.exists("./Hydrology_Processed/hydro_metrics_prep")){dir.create("./Hydrology_Processed/hydro_metrics_prep")}
if(!dir.exists("./Hydrology_Processed/hydro_metrics_prep/SagaGrid")){dir.create("./Hydrology_Processed/hydro_metrics_prep/SagaGrid")}

if(!dir.exists("./Predictor_Variables")){dir.create("./Predictor_Variables")}
if(!dir.exists("./Predictor_Variables/Hydrology_metrics")){dir.create("./Predictor_Variables/Hydrology_metrics")}
if(!dir.exists("./Predictor_Variables/Hydrology_metrics")){dir.create("./Predictor_Variables/Hydrology_metrics")}
if(!dir.exists(paste0("./Predictor_Variables/Hydrology_metrics/", Raw_DEM_reso, "DEM"))){dir.create(paste0("./Predictor_Variables/Hydrology_metrics/", Raw_DEM_reso, "DEM"))}

#########################################
#########################################
#####Grid>Tools>Proximity Grid
rsaga.get.usage(lib = "grid_tools", module = "Proximity Grid", env = env)

rsaga.geoprocessor(lib = "grid_tools", module = "Proximity Grid", env = env, param = list(
  FEATURES = paste0("./Hydrology_Processed/water_bodies", Raw_DEM_reso, "_", Raw_DEM_reso, ".tif"),
  DISTANCE = "./Hydrology_Processed/hydro_metrics_prep/SagaGrid/body_dist"
  ))

rsaga.geoprocessor(lib = "grid_tools", module = "Proximity Grid", env = env, param = list(
  FEATURES = paste0("./Hydrology_Processed/water_channels", Raw_DEM_reso, "_", Raw_DEM_reso, ".tif"),
  DISTANCE = "./Hydrology_Processed/hydro_metrics_prep/SagaGrid/channel_dist"
))


sdat_files <- list.files(path="./Hydrology_Processed/hydro_metrics_prep/SagaGrid", pattern="\\.sdat$")
for (i in 1:length(sdat_files)){
  file_name <- gsub(x=sdat_files[i], pattern = "sdat", replacement = "tif")
  temp_raster <- raster(paste0("./Hydrology_Processed/hydro_metrics_prep/SagaGrid/", sdat_files[i]))
  raster::writeRaster(temp_raster, filename = paste0("./Hydrology_Processed/hydro_metrics_prep/", file_name), 
                      drivername = "GTiff", overwrite = TRUE)
}


#########################################
#########################################
#####Masking layer to DEM extent
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

#Distance to Water bodies 
wb_dist <- raster("./Hydrology_Processed/hydro_metrics_prep/body_dist.tif")
wb_dist <- crop(wb_dist, DEM); wb_dist <- mask(wb_dist, DEM)

raster::writeRaster(wb_dist, filename = paste0("./Predictor_Variables/Hydrology_metrics/", Raw_DEM_reso, "DEM/wb_dist.tif"),
                    drivername = "GTiff", overwrite = TRUE)

#Distance to Water channels
wc_dist <- raster("./Hydrology_Processed/hydro_metrics_prep/channel_dist.tif")
wc_dist <- crop(wc_dist, DEM); wc_dist <- mask(wc_dist, DEM)
raster::writeRaster(wc_dist, filename = paste0("./Predictor_Variables/Hydrology_metrics/", Raw_DEM_reso, "DEM/st_dist.tif"),
                    drivername = "GTiff", overwrite = TRUE)


end_time <- Sys.time()
print(paste("End of [3] Creating hydrology metrics.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
