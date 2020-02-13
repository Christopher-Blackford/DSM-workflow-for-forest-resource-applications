##############################################################################################################################
##############################################################################################################################
###[1] Filtering DEM.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#####Loading libraries
library(RSAGA)
library(raster)

#####Find saga_cmd.exe file
env = rsaga.env(path = "./Raw/saga-7.0.0_x64")

#####Specifying resolution
Raw_DEM_reso = 10
#####Specifying filter window radius (this can be multiple numbers if you want to test different filtering windows)
Filter_type <- 100

#####Load in DEM
Raw_DEM <- paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif")
reso <- unique(res(raster(Raw_DEM)))

#####Create directories
if(!dir.exists("./DEM_Processed")){dir.create("./DEM_Processed")}
if(!dir.exists("./DEM_Processed/SagaGrid")){dir.create("./DEM_Processed/SagaGrid")}
if(!dir.exists("./DEM_Processed/R")){dir.create("./DEM_Processed/R")}
if(!dir.exists(paste0("./DEM_Processed/R/", reso, "DEM"))){dir.create(paste0("./DEM_Processed/R/", reso, "DEM"))}

file.remove(list.files(path= "./DEM_Processed/SagaGrid", full.names = TRUE)) #Removing previous temporary filter files
#########################################
#########################################
#Filtering DEM settings
rsaga.get.usage(lib = "grid_filter", module = "Simple Filter", env = env)

Moving_window_shape <- 1 #0 = Square, 1 = Circle

for (i in 1:length(Filter_type)){
  
  #Smoothed file directories
  Smooth_dir <- paste0("./DEM_Processed/SagaGrid/", reso, "DEM", Filter_type[i])
  
  ###Smooth
  rsaga.geoprocessor(lib = "grid_filter", module = "Simple Filter", env = env, param = list(
    INPUT = Raw_DEM,
    RESULT = Smooth_dir,
    METHOD = 0, #0 = Smooth
    KERNEL_TYPE = Moving_window_shape,
    KERNEL_RADIUS = Filter_type[i]
  ))
  
  }

####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
#Converting from Saga grid to tif format

sdat_files <- list.files(path="./DEM_Processed/SagaGrid", pattern="\\.sdat$")
for (i in 1:length(sdat_files)){
  file_name <- gsub(x=sdat_files[i], pattern = "sdat", replacement = "tif")
  temp_raster <- raster(paste0("./DEM_Processed/SagaGrid/", sdat_files[i]))
  raster::writeRaster(temp_raster, filename = paste0("./DEM_Processed/R/", reso, "DEM/", file_name), drivername = "GTiff", overwrite = TRUE)
}

end_time <- Sys.time()
print(paste("End of [1] Filtering DEM.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
