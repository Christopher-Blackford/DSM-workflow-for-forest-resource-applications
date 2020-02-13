##############################################################################################################################
##############################################################################################################################
###[8] Generating entropy rasters.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#Loading libraries
library(raster); library(rgdal); library(rgeos)
library(caret); library(doParallel); library(ClusterR)

#####Specifying resolution and filter
Raw_DEM_reso = 10
Filter_type = 100
Attribute <- "M" #Can be "M" or "T" in our case

#Create directories for model output
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Random_Forest/Predicted_layers/Entropy"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Random_Forest/Predicted_layers/Entropy"))}

#Get all the probabilistic rasters for each class of your soil attribute
Probs <- list.files(path=paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Random_Forest/Predicted_layers/Probabilities/layers"),
                                pattern=paste0(Attribute, "."), full.names = TRUE)

Probs_ras <- raster::stack(Probs)

#Calculate entropy of each pixel using probabilistic raster. Entropy as defined in Zhu (1997)
f1 <- function(x){x*log(x+0.000001)}
beginCluster(10)
entropy <- clusterR(Probs_ras, calc, args=list(f1))
entropy <- clusterR(entropy, calc, args=list(sum))
f2 <- function(x){(-1)*x/log(length(Probs))}
entropy <- calc(entropy, fun = f2)
entropy[entropy < 0] <- 0
plot(entropy)
endCluster()

#Write out entropy raster
writeRaster(entropy, filename= paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Random_Forest/Predicted_layers/Entropy/Entropy_", Attribute, ".tif"), 
            format="GTiff", overwrite=TRUE)


end_time <- Sys.time()
print(paste("End of [8] Generating entropy raster.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
