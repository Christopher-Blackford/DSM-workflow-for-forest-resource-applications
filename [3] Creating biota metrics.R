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
if(!dir.exists(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/categorical"))){dir.create(paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/categorical"))}

#####Load in DEM
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

#####Load in FRI
Biota <- readOGR(dsn="./Raw/Biota_Raw/HF-601-2D.gdb", layer="Polygon_Forest")
Biota <- Biota[c("OHT", "UHT", "OLEADSPC", "ULEADSPC")]

unique(Biota@data$OHT)
unique(Biota@data$UHT)
unique(Biota@data$OLEADSPC)
unique(Biota@data$ULEADSPC)

#Projecting
#These files just happen to be in the same projection

#Set raster resolution to DEM resolution
r <- raster(ncol=(extent(DEM)@xmax - extent(DEM)@xmin)/Raw_DEM_reso, nrow=(extent(DEM)@ymax - extent(DEM)@ymin)/Raw_DEM_reso)
extent(r) <- extent(DEM)


#OHT
Biota_ras <- rasterize(Biota, r, "OHT") #Need to add "fun" argument to rasterize function to account for cells that overlap polygon boundaries. 
#But in practice sine we are using high res data I don't think it matters

writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous/bio_OHT.tif"), 
            drivername = "GTiff", overwrite = TRUE)

#UHT
Biota_ras <- rasterize(Biota, r, "UHT") #Need to add "fun" argument to rasterize function to account for cells that overlap polygon boundaries. 
#But in practice sine we are using high res data I don't think it matters

writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/continuous/bio_UHT.tif"), 
            drivername = "GTiff", overwrite = TRUE)

#OLEADSPC
Biota_ras <- rasterize(Biota, r, "OLEADSPC") #Need to add "fun" argument to rasterize function to account for cells that overlap polygon boundaries. 
#But in practice sine we are using high res data I don't think it matters

writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/categorical/bio_OLEADSPC.tif"), 
            drivername = "GTiff", overwrite = TRUE)

#ULEADSPC
Biota_ras <- rasterize(Biota, r, "ULEADSPC") #Need to add "fun" argument to rasterize function to account for cells that overlap polygon boundaries. 
#But in practice sine we are using high res data I don't think it matters

writeRaster(Biota_ras, paste0("./Predictor_Variables/Biota_metrics/", Raw_DEM_reso, "DEM/categorical/bio_ULEADSPC.tif"), 
            drivername = "GTiff", overwrite = TRUE)

end_time <- Sys.time()
print(paste("End of [3] Creating biota metrics ", capture.output(end_time - start_time)))
#####
####
###
##
#END