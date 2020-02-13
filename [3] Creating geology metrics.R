##############################################################################################################################
##############################################################################################################################
###[3] Creating geology metrics.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

######Loading libraries
library(rgdal)
library(raster)
library(maptools)
library(rgeos)

#####Specifying resolution
Raw_DEM_reso = 10
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

#####Create directories
if(!dir.exists("./Predictor_Variables")){dir.create("./Predictor_Variables")}
if(!dir.exists("./Predictor_Variables/Geology_metrics")){dir.create("./Predictor_Variables/Geology_metrics")}
if(!dir.exists(paste0("./Predictor_Variables/Geology_metrics/", Raw_DEM_reso, "DEM"))){dir.create(paste0("./Predictor_Variables/Geology_metrics/", Raw_DEM_reso, "DEM"))}

#####Load Geology data
Bed_geo <- readOGR("./Raw/Geology_Raw", "ON_Bedrock_GeologySteph")
Qua_geo <- readOGR("./Raw/Geology_Raw", "ON_Quaternary Geology_CombinedTill") #Null geometry at FID = 16141, automatically removes. Maybe ask Kara? Probably not huge deal

#Extent to clip polygon file
clip_box <- as(extent(DEM), 'SpatialPolygons')
crs(clip_box) <- crs(DEM)

#Bedrock geology to raster
Bed_geo <- spTransform(Bed_geo, DEM@crs)
Bed_geo <- crop(Bed_geo, clip_box)
Bedrock_df <- data.frame(UNITNAME_P = unique(Bed_geo@data$UNITNAME_P), code = 1:length(unique(Bed_geo@data$UNITNAME_P)))
Bed_geo <- sp::merge(Bed_geo, Bedrock_df)

r <- raster(ncol=DEM@ncols, nrow=DEM@nrows)
extent(r) <- extent(DEM)
Bed_geo <- rasterize(Bed_geo, r, 'NEW_UNIT_N')

#Quaternary geology to raster
Qua_geo <- spTransform(Qua_geo, DEM@crs)
Qua_geo <- crop(Qua_geo, clip_box)
Quatern_df <- data.frame(UNIT_NAME = unique(Qua_geo@data$UNIT_NAME), code = 1:length(unique(Qua_geo@data$UNIT_NAME)))
Qua_geo <- sp::merge(Qua_geo, Quatern_df)

r <- raster(ncol=DEM@ncols, nrow=DEM@nrows)
extent(r) <- extent(DEM)
Qua_geo <- rasterize(Qua_geo, r, 'Unit_TillC')
plot(Qua_geo)

#########################################
#########################################
#####Masking layer

#Write out bedrock geology layer
Bed_geo_mask <- mask(Bed_geo, DEM)
raster::writeRaster(Bed_geo_mask, filename = paste0("./Predictor_Variables/Geology_metrics/", Raw_DEM_reso, "DEM/Bed_geo.tif"), 
                    drivername = "GTiff", overwrite = TRUE)

#Write out quaternary geology layer
Qua_geo_mask <- mask(Qua_geo, DEM)
raster::writeRaster(Qua_geo_mask, filename = paste0("./Predictor_Variables/Geology_metrics/", Raw_DEM_reso, "DEM/Qua_geo.tif"), 
                    drivername = "GTiff", overwrite = TRUE)


end_time <- Sys.time()
print(paste("End of [3] Creating geology metrics.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END

