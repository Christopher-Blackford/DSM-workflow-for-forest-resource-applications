##############################################################################################################################
##############################################################################################################################
###[3] Creating biota metrics.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#####Loading libraries
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)

#####Specifying resolution
Raw_DEM_reso = 10

#####Create directories
if(!dir.exists("./Predictor_Variables")){dir.create("./Predictor_Variables")}
if(!dir.exists("./Predictor_Variables/Biota_metrics")){dir.create("./Predictor_Variables/Biota_metrics")}
if(!dir.exists(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM"))){dir.create(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM"))}
if(!dir.exists(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous"))){dir.create(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous"))}
if(!dir.exists(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/discrete"))){dir.create(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/discrete"))}


#####Load in DEM
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

#####Load in FRI
Biota <- readOGR(dsn="./Raw/Biota_Raw/HF-601-2D.gdb",layer="Polygon_Forest")
Biota <- Biota[c("OHT", "UHT", "OLEADSPC", "ULEADSPC")]

unique(Biota@data$OLEADSPC)
unique(Biota@data$ULEADSPC)
unique(Biota@data$OSI) #Empty
unique(Biota@data$USI) #Empth
unique(Biota@data$OHT)
unique(Biota@data$UHT)
unique(Biota@data$STKG)

#Projecting
Biota@proj4string <- DEM@crs #These are the same projection, just named differently which is why it's okay to do this

#Set raster resolution to DEM resolution
r <- raster(ncol=(extent(DEM)@xmax - extent(DEM)@xmin)/Raw_DEM_reso, nrow=(extent(DEM)@ymax - extent(DEM)@ymin)/Raw_DEM_reso)
extent(r) <- extent(DEM)

#Convert to raster
for (i in 1:ncol(Biota)){
  Biota_ras <- rasterize(Biota, r, colnames(Biota@data)[i]) #Need to add "fun" argument to rasterize function to account for cells that overlap polygon boundaries. 
                                                                #But in practice sine we are using high res data I don't think it matter
  reso <- paste(round(res(Biota_ras)), collapse = " ")
  reso <- gsub(x = reso, pattern = " ", replacement = "_")
  if(i == 1| i == 2){writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous/bio_", colnames(Biota@data)[i], ".tif"), drivername = "GTiff", overwrite = TRUE)
  }else{writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/discrete/bio_", colnames(Biota@data)[i], ".tif"), drivername = "GTiff", overwrite = TRUE)}
  
  print(paste0("Done ", i))
  }


end_time <- Sys.time()
print(paste("End of [3] Creating biota metrics ", capture.output(end_time - start_time)))
#####
####
###
##
#END

