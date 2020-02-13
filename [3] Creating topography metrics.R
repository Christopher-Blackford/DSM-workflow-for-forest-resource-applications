##############################################################################################################################
##############################################################################################################################
###[3] Creating topography metrics.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#Loading libraries
library(RSAGA)
library(raster)

###To get information on libraries available, modules available, and a specific module format
#rsaga.get.libraries(path = env$modules)
#rsaga.get.modules(lib = "terrain_analysis", env = env)
#rsaga.get.usage(lib = "ta_morphometry", module = "Terrain Surface Convexity", env = env)

#####Specifying resolution and filter
Raw_DEM_reso = 10
Filter_type = 100
DEM_Subtraction_value = 30

#####Find saga_cmd.exe file
env <- rsaga.env(path = "./Raw/saga-7.0.0_x64") #directory of saga folder

#####Create directories for burn file
if(!dir.exists("./Hydrology_Processed/burn_files")){dir.create("./Hydrology_Processed/burn_files")}
#####Create directories for topo metrics
if(!dir.exists("./DEM_Processed/topo_metrics_prep")){dir.create("./DEM_Processed/topo_metrics_prep")}

if(!dir.exists("./Predictor_Variables")){dir.create("./Predictor_Variables")}
if(!dir.exists("./Predictor_Variables/Topography_metrics")){dir.create("./Predictor_Variables/Topography_metrics")}
if(!dir.exists(paste0("./Predictor_Variables/Topography_metrics/",  Raw_DEM_reso, "DEM", Filter_type))){dir.create(paste0("./Predictor_Variables/Topography_metrics/", Raw_DEM_reso, "DEM", Filter_type))}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####Create burn file if it doesn't already exist
if(!file.exists(paste0("./Hydrology_Processed/burn_files/burn", DEM_Subtraction_value, "_r", Raw_DEM_reso, "_f", Filter_type ,".tif"))){
  DEM_file <- raster(paste0("./DEM_Processed/R/",  Raw_DEM_reso, "DEM/", Raw_DEM_reso, "DEM", Filter_type, ".tif"))
  hydro_raster <- raster(paste0("./Hydrology_Processed/", Raw_DEM_reso, "hydro_raster.tif"))
  
  #Subtracting hydrology from DEM
  df <- matrix(c(1, 99, DEM_Subtraction_value), nrow = 1, ncol = 3)
  hydro_raster <- raster::reclassify(hydro_raster, df, include.lowest = TRUE)
  #Burning in hydrology
  burn_file <- overlay(DEM_file, hydro_raster, fun=function(r1, r2){return(r1-r2)})
  
  #Writing out layer, recording degree of burn and resolution of result
  writeRaster(burn_file, paste0("./Hydrology_Processed/burn_files/burn", DEM_Subtraction_value, "_r", unique(res(hydro_raster)), "_f", Filter_type ,".tif"), drivername = "GTiff", overwrite = TRUE)
  rm(DEM_Subtraction_value, DEM_file, df, burn_file, hydro_raster)
  }else(print("Loading previously generated burn file"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~REDUNDANT

###Defining working directories for DEM files to be loaded into SAGA
DEM_file <- paste0("./DEM_Processed/R/",  Raw_DEM_reso, "DEM/", Raw_DEM_reso, "DEM", Filter_type, ".tif")
DEM_hydro <- paste0("./Hydrology_Processed/burn_files/burn30_r", Raw_DEM_reso, "_f", Filter_type,".tif")

#Remove previous files
file.remove(list.files(path= "./DEM_Processed/topo_metrics_prep", full.names = TRUE))

#########################################
#########################################
#####Terrain Analysis>Morphometry>Slope, Aspect, Curvature
rsaga.get.usage(lib = "ta_morphometry", module = "Slope, Aspect, Curvature", env = env)
#asp - Aspect
#slp - Slope 
#gen_curve - General curvature 
#prof_curve - Profile curvature 
#plan_curve - Plan curvature 
#tan_curve - Tangential curvature 
#min_curve - Minimum curvature
#max_curve - Maximum curvature
#tot_curve - Total curvature
rsaga.geoprocessor(lib = "ta_morphometry", module = "Slope, Aspect, Curvature", env = env, param = list(
  ELEVATION = DEM_file,
  ASPECT = "./DEM_Processed/topo_metrics_prep/asp",
  SLOPE = "./DEM_Processed/topo_metrics_prep/slp",
  C_GENE = "./DEM_Processed/topo_metrics_prep/gen_curve",
  C_PROF = "./DEM_Processed/topo_metrics_prep/prof_curve",
  C_PLAN = "./DEM_Processed/topo_metrics_prep/plan_curve",
  C_TANG = "./DEM_Processed/topo_metrics_prep/tan_curve",
  C_MINI = "./DEM_Processed/topo_metrics_prep/min_curve",
  C_MAXI = "./DEM_Processed/topo_metrics_prep/max_curve",
  C_TOTA = "./DEM_Processed/topo_metrics_prep/tot_curve"
  ))


#########################################Hydrology DEM processing
#########################################
#####Terrain Analysis>Hydrology>SAGA wetness Index
rsaga.get.usage(lib = "ta_hydrology", module = "SAGA Wetness Index", env = env)
#ca - Catchment area
#cs - Catchment slope
#mca - Modified catchment area
#twi - Topographic wetness index
rsaga.geoprocessor(lib = "ta_hydrology", module = "SAGA Wetness Index", env = env, param = list(
  DEM = DEM_hydro,
  AREA = "./DEM_Processed/topo_metrics_prep/ca",
  SLOPE = "./DEM_Processed/topo_metrics_prep/cs",
  AREA_MOD = "./DEM_Processed/topo_metrics_prep/mca",
  TWI = "./DEM_Processed/topo_metrics_prep/twi"
  ))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Upslope and Downslope Curvature
rsaga.get.usage(lib = "ta_morphometry", module = "Upslope and Downslope Curvature", env = env)
#loc_curve - Local curvature
#up_curve - Upslope curvature
#lup_curve - Local upslope curvature
#dwn_curve - Downslope curvature
#ldwn_curve - Local downslope curvature
rsaga.geoprocessor(lib = "ta_morphometry", module = "Upslope and Downslope Curvature", env = env, param = list(
  DEM = DEM_file,
  C_LOCAL = "./DEM_Processed/topo_metrics_prep/loc_curve",
  C_UP = "./DEM_Processed/topo_metrics_prep/up_curve",
  C_UP_LOCAL = "./DEM_Processed/topo_metrics_prep/lup_curve",
  C_DOWN = "./DEM_Processed/topo_metrics_prep/dwn_curve",
  C_DOWN_LOCAL = "./DEM_Processed/topo_metrics_prep/ldwn_curve"
  ))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Multiresolution Index of Valley Bottom Flatness (MRVBF)
rsaga.get.usage(lib = "ta_morphometry", module = "Multiresolution Index of Valley Bottom Flatness (MRVBF)", env = env)
#mrrtf - Multiresolution ridgetop flatness
#mrvbf - Multiresolution valley bottom flatness
rsaga.geoprocessor(lib = "ta_morphometry", module = "Multiresolution Index of Valley Bottom Flatness (MRVBF)", env = env, param = list(
  DEM = DEM_file,
  MRRTF = "./DEM_Processed/topo_metrics_prep/mrrtf",
  MRVBF = "./DEM_Processed/topo_metrics_prep/mrvbf"
  ))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Multi-Scale Topographic Position Index
rsaga.get.usage(lib = "ta_morphometry", module = "Multi-Scale Topographic Position Index (TPI)", env = env)
#ms_tpi - Multi-scale topographic position index
rsaga.geoprocessor(lib = "ta_morphometry", module = "Multi-Scale Topographic Position Index (TPI)", env = env, param = list(
  DEM = DEM_file,
  TPI = "./DEM_Processed/topo_metrics_prep/ms_tpi"
  ))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Real Surface Area
rsaga.get.usage(lib = "ta_morphometry", module = "Real Surface Area", env = env)
#rsa - Real surface area
rsaga.geoprocessor(lib = "ta_morphometry", module = "Real Surface Area", env = env, param = list(
  DEM = DEM_file,
  AREA = "./DEM_Processed/topo_metrics_prep/rsa"
  ))


#########################################
#########################################
#####Terrain Analysis>Lightning, Visibility>Topographic Openness
rsaga.get.usage(lib = "ta_lighting", module = "Topographic Openness", env = env)
#tpo - Topographic positive openness
#tno - Topographic negative opennes
rsaga.geoprocessor(lib = "ta_lighting", module = "Topographic Openness", env = env, param = list(
  DEM = DEM_file,
  POS = "./DEM_Processed/topo_metrics_prep/tpo",
  NEG = "./DEM_Processed/topo_metrics_prep/tno"
))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Terrain Ruggedness Index (TRI)
rsaga.get.usage(lib = "ta_morphometry", module = "Terrain Ruggedness Index (TRI)", env = env)
#tri_11 - Terrain ruggedness index
rsaga.geoprocessor(lib = "ta_morphometry", module = "Terrain Ruggedness Index (TRI)", env = env, param = list(
  DEM = DEM_file,
  TRI = "./DEM_Processed/topo_metrics_prep/tri"
))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Terrain Surface Convexity
rsaga.get.usage(lib = "ta_morphometry", module = "Terrain Surface Convexity", env = env)
#ts_conc11 - Terrain surface concavity
#ts_conv3 - Terrain surface convexity
rsaga.geoprocessor(lib = "ta_morphometry", module = "Terrain Surface Convexity", env = env, param = list(
  DEM = DEM_file, 
  CONVEXITY = "./DEM_Processed/topo_metrics_prep/ts_conv", 
  TYPE = 0))
rsaga.geoprocessor(lib = "ta_morphometry", module = "Terrain Surface Convexity", env = env, param = list(
  DEM = DEM_file, 
  CONVEXITY = "./DEM_Processed/topo_metrics_prep/ts_conc", 
  TYPE = 1))


#########################################
#########################################
#####Terrain Analysis>Morphometry>Terrain Surface Texture
#rsaga.get.usage(lib = "ta_morphometry", module = "Terrain Surface Texture", env = env)
#ts_text11 - Terrain Surface texture
#rsaga.geoprocessor(lib = "ta_morphometry", module = "Terrain Surface Texture", env = env, param = list(
 # DEM = DEM_file,
  #TEXTURE = "./DEM_Processed/topo_metrics_prep/ts_text"
  #))


#########################################
#########################################
#####Terrain Analysis>Channels>Valley Depth
rsaga.get.usage(lib = "ta_channels", module = "Valley Depth", env = env)
#val_depth - Valley depth
rsaga.geoprocessor(lib = "ta_channels", module = "Valley Depth", env = env, param = list(
  ELEVATION = DEM_file,
  VALLEY_DEPTH = "./DEM_Processed/topo_metrics_prep/val_depth"
  ))


#########################################
#########################################
#####Tool Chains>Terrain Analysis>Upslope Height, Slope, Aspect
rsaga.get.usage(lib = "terrain_analysis", module = "Upslope Height, Slope, Aspect", env = env)
rsaga.get.usage(lib = "upslope_height", module = "Upslope Height, Slope, Aspect", env = env)
#up_ht - Upslope height
#rsaga.geoprocessor(lib = "terrain_analysis", module = "Upslope Height, Slope, Aspect", env = env, param = list(
rsaga.geoprocessor(lib = "terrain_analysis", module = "Upslope Height, Slope, Aspect", env = env, param = list( #with the different library name
  DEM = DEM_file,
  HEIGHT = "./DEM_Processed/topo_metrics_prep/up_ht"
  ), display.command = TRUE
  #check.module.exists = FALSE #This ensures it won't throw a warning since the module name doesn't match up to how it's presented in SAGA
  )


#########################################
#########################################
#####Terrain Analysis>Morphometry>Relative Heights and Slope Positions
rsaga.get.usage(lib = "ta_morphometry", module = "Relative Heights and Slope Positions", env = env)
#nslp_ht - Normalized height
#sslp_ht - Standardized height
#mslp_pos - Mid-slope position
rsaga.geoprocessor(lib = "ta_morphometry", module = "Relative Heights and Slope Positions", env = env, param = list(
  DEM = DEM_file,
  NH = "./DEM_Processed/topo_metrics_prep/nslp_ht",
  SH = "./DEM_Processed/topo_metrics_prep/sslp_ht",
  MS = "./DEM_Processed/topo_metrics_prep/mslp_pos"
))



####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
####################################################################################################################################################################
#Converting from Saga grid to tif format

sdat_files <- list.files(path="./DEM_Processed/topo_metrics_prep", pattern="\\.sdat$") #doesn't search subdirectories
for (i in 1:length(sdat_files)){
  file_name <- gsub(x=sdat_files[i], pattern = "sdat", replacement = "tif")
  temp_raster <- raster(paste0("./DEM_Processed/topo_metrics_prep/", sdat_files[i]))
  raster::writeRaster(temp_raster, filename = paste0("./Predictor_Variables/Topography_metrics/", Raw_DEM_reso, "DEM", Filter_type, "/", file_name), overwrite = TRUE)
}


end_time <- Sys.time()
print(paste("End of [3] Creating topography metrics.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
