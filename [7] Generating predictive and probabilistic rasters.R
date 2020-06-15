##############################################################################################################################
##############################################################################################################################
###[7] Generating predictive and probabilistic rasters.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#Loading libraries
library(raster); library(rgdal); library(rgeos)
library(caret); library(doParallel)

#####Specifying resolution, filter and whether to create probabilistic rasters (only applicable for Random Forest or or another model that records class probabilities)
Raw_DEM_reso = 10
Filter_type = 100
Create_prob_raster = TRUE #Can be TRUE or FALSE

if(!dir.exists("./temp")){dir.create("./temp")} #These lines are useful as they store potentially large raster temporary files in a recognizable location instead of default C drive
rasterOptions(tmpdir="./temp")

###Load in Machine learning model (choose MLM type and attribute you are looking at)
MLM_abbrev <- "RF" #Can be "RF", "SVM", or "kNN"
Attribute <- "M" #Can be "M" or "T"

if (MLM_abbrev == "RF"){MLM <- "Random_Forest"}
if (MLM_abbrev == "SVM"){MLM <- "Support_Vector_Machine_R"}
if (MLM_abbrev == "kNN"){MLM <- "k_Nearest_Neighbour"}

#Load in Machine Learning model output
Model_results <- readRDS(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Results/", Attribute, "_", MLM_abbrev, ".rds"))

#Create directories for predicted rasters
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Raw"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Raw"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities/layers"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities/layers"))}

#Running in parallel
detectCores()
registerDoParallel(detectCores()-1)
getDoParWorkers()

#Acquiring continuous environmental covariates
Topo_Covariates <- list.files(path= paste0("./Predictor_Variables/Topography_metrics/", Raw_DEM_reso, "DEM", Filter_type), pattern="\\.tif$", full.names = TRUE)
Hydro_Covariates <- list.files(path= paste0("./Predictor_Variables/Hydrology_metrics/", Raw_DEM_reso, "DEM"), pattern="\\.tif$", full.names = TRUE)
Bio_continuousCovariates <- list.files(path= paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous"), pattern="\\.tif$", full.names = TRUE)
Dist_Covariates <- list.files(path= paste0("./Predictor_Variables/DistanceFields/", Raw_DEM_reso, "DEM"), pattern="\\.tif$", full.names = TRUE)
#Acquiring categorical environmental covariates
Geo_Covariates <- list.files(path= paste0("./Predictor_Variables/Geology_metrics/", Raw_DEM_reso, "DEM"), pattern="\\.tif$", full.names = TRUE)
Bio_categoricalCovariates <- list.files(path= paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/categorical"), pattern="\\.tif$", full.names = TRUE)

#Converting categorical to binary
Cat_stack <- stack(c(Geo_Covariates, Bio_categoricalCovariates))

library(RStoolbox)
Cat_layers = NULL

for(i in 1:length(Cat_stack@layers)){
  temp_layer <- oneHotEncode(Cat_stack[[i]], classes=c(Cat_stack[[i]]@data@min:Cat_stack[[i]]@data@max))
  Cat_layers <- append(Cat_layers, temp_layer)
}
rm(temp_layer, Cat_stack)

#Stacking covariates
Covariates <- c(Topo_Covariates, Hydro_Covariates, Bio_continuousCovariates, Dist_Covariates, Cat_layers)
Covariates <- raster::stack(Covariates)
##

#Probabaility rasters based on vote distribution of trees in Random Forest. Should only run this with Random Forest or another model that records class probabilities
#####Write out raster
print("Writing raster")

#Predicted soil map based final decision of MLM
raw_start <- Sys.time()
writeRaster(x=raster::predict(Covariates, Model_results, type = "raw"), filename= paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Raw/", Attribute, "_", MLM_abbrev, ".tif"), 
            format="GTiff", overwrite=TRUE)
raw_end <- Sys.time(); capture.output(raw_end - raw_start)

if (Create_prob_raster == TRUE){
prob_start <- Sys.time()
writeRaster(x=raster::predict(Covariates, Model_results, type = "prob", index = 1:nlevels(Model_results)), filename= paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities/layers/", Attribute, "_", MLM_abbrev, "_prob.tif"),
            format="GTiff", overwrite=TRUE, bylayer=TRUE, suffix = levels(Model_results))
prob_end <- Sys.time(); capture.output(prob_end - prob_start)
  
#Load in rasters and stack them
max_start <- Sys.time()
Max_certainty <- list.files(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities/layers"),
                            pattern = paste0(Attribute, "_", MLM_abbrev), full.names = TRUE)
Max_certainty <- raster::stack(Max_certainty)
Max_certainty <- raster::stackApply(Max_certainty, indices = rep(1, nlayers(Max_certainty)), fun = max)
  
writeRaster(Max_certainty, paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/", MLM, "/Predicted_layers/Probabilities/", Attribute, "_", MLM_abbrev, "_ClassCertainty.tif"),
              format="GTiff", overwrite=TRUE)
max_end <- Sys.time(); capture.output(max_end - max_start)
}

end_time <- Sys.time()
print(paste("End of [7] Generating predictive and probabilistic rasters.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END