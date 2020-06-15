##############################################################################################################################
##############################################################################################################################
###[2] Creating hydrology files.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#####Loading libraries
library(rgdal)
library(raster)
library(maptools)
library(rgeos)

#####Specifying resolution
Raw_DEM_reso = 10

#####Load in DEM
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif")) 

#####Create directories
if(!dir.exists("./Hydrology_Processed")){dir.create("./Hydrology_Processed")}
if(!dir.exists("./Hydrology_Processed/shapefiles")){dir.create("./Hydrology_Processed/shapefiles")}

if(!dir.exists("./temp")){dir.create("./temp")} #These lines are useful as they store potentially large raster temporary files in a recognizable location instead of default C drive
rasterOptions(tmpdir="./temp")

#####Generate waterbody/waterchannel extent as slightly larger than DEM extent to incorporate hydrology immediately outside of DEM extent
WaterBody_extent <- extent(DEM); WaterChannel_extent <- extent(DEM)

WaterBody_extent@xmin <- WaterBody_extent@xmin - 4750; WaterChannel_extent@xmin <- WaterChannel_extent@xmin - 3780 #This value was decided on from a pilot study
WaterBody_extent@ymin <- WaterBody_extent@ymin - 4750; WaterChannel_extent@ymin <- WaterChannel_extent@ymin - 3780
WaterBody_extent@xmax <- WaterBody_extent@xmax + 4750; WaterChannel_extent@xmax <- WaterChannel_extent@xmax + 3780
WaterBody_extent@ymax <- WaterBody_extent@ymax + 4750; WaterChannel_extent@ymax <- WaterChannel_extent@ymax + 3780

##############################################################################################################################
##############################################################################################################################

#####Load in water bodies layer. Data source: https://data.ontario.ca/dataset/ontario-hydro-network-waterbody
water_bodies <- readOGR("./Raw/Hydrology_Raw/Water_bodies", "OHN_WATERBODY"); backup_waterbodies <- water_bodies
#water_bodies <- water_bodies[!(water_bodies$WBDY_TYPE == "River"),] #Removing rivers from water bodies file - ending up deciding against this

water_bodies@data$ID <- row.names(water_bodies)
water_bodies_df <- water_bodies@data; water_bodies_df$ID <- row.names(water_bodies@data)
water_bodies <- spTransform(water_bodies, DEM@crs)

#Interset with study area
p <- as(WaterBody_extent, 'SpatialPolygons'); p@proj4string <- DEM@crs
water_bodies <- gIntersection(water_bodies, p, byid = TRUE)

#Rejoining attribute table to write out clipped water body layer
row.names(water_bodies) <- gsub(row.names(water_bodies), pattern = " 1", replacement = "")
temp <- data.frame(ID = row.names(water_bodies))
row.names(temp) <- row.names(water_bodies)

water_bodies <- SpatialPolygonsDataFrame(water_bodies, temp)
water_bodies <- sp::merge(water_bodies, water_bodies_df)
writeOGR(water_bodies, dsn = "./Hydrology_Processed/shapefiles", layer = "water_bodies",
         driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)

#Convert to raster
water_bodies@data$wb_present <- 1

r <- raster(ncol=(WaterBody_extent@xmax - WaterBody_extent@xmin)/Raw_DEM_reso, nrow=(WaterBody_extent@ymax - WaterBody_extent@ymin)/Raw_DEM_reso)
extent(r) <- extent(WaterBody_extent)
r@crs <- DEM@crs

water_bodies <- rasterize(water_bodies, r, 'wb_present') #Creates 368 312 500 cells

reso <- paste(round(res(water_bodies)), collapse = " ")
reso <- gsub(x = reso, pattern = " ", replacement = "_")
writeRaster(water_bodies, paste0("./Hydrology_Processed/water_bodies", reso, ".tif"), drivername = "GTiff", overwrite = TRUE)


##############################################################################################################################
##############################################################################################################################


#####Laoad in water channels layer. Data source: https://data.ontario.ca/dataset/ontario-integrated-hydrology-data
waterchannel_pt1 <- "./Raw/Hydrology_Raw/Water_channels/part1/IH_FNC1.gdb" #Far North Central 1
waterchannel_pt2 <- "./Raw/Hydrology_Raw/Water_channels/part2/IH_FNE.gdb" #Far North East
waterchannel_pt1 <- readOGR(dsn=waterchannel_pt1,layer="EnhancedWatercourse")
waterchannel_pt2 <- readOGR(dsn=waterchannel_pt2,layer="EnhancedWatercourse")

unioned <- gUnion(waterchannel_pt1, waterchannel_pt2); backup <- unioned
unioned <- spTransform(unioned, DEM@crs)

#Interset with study area
p <- as(WaterChannel_extent, 'SpatialPolygons'); p@proj4string <- DEM@crs
water_channels <- gIntersection(unioned, p, byid = TRUE)

#Rejoining attribute table to write out
temp <- data.frame(ID = row.names(water_channels))
row.names(temp) <- row.names(water_channels)

water_channels <- SpatialLinesDataFrame(water_channels, temp)

writeOGR(water_channels, dsn = "./Hydrology_Processed/shapefiles", layer = "water_channels",
         driver = "ESRI Shapefile", verbose = TRUE, overwrite = TRUE, morphToESRI = TRUE)

#Convert to raster
water_channels@data$wc_present <- 1

r <- raster(ncol=(WaterChannel_extent@xmax - WaterChannel_extent@xmin)/Raw_DEM_reso, nrow=(WaterChannel_extent@ymax - WaterChannel_extent@ymin)/Raw_DEM_reso)
extent(r) <- extent(WaterChannel_extent)
r@crs <- DEM@crs

water_channels <- rasterize(water_channels, r, 'wc_present') #Creates 368 312 500 cells

reso <- paste(round(res(water_channels)), collapse = " ")
reso <- gsub(x = reso, pattern = " ", replacement = "_")
writeRaster(water_channels, paste0("./Hydrology_Processed/water_channels", reso, ".tif"), drivername = "GTiff", overwrite = TRUE)


##############################################################################################################################
##############################################################################################################################

###Merging water channels and water bodies together
#water_channels_resample <- resample(water_channels, water_bodies, "ngb") #I don't think resample is needed at this point
hydro_raster <- raster::merge(water_bodies, water_channels)
hydro_raster[is.na(hydro_raster[])] <- 0

hydro_raster <- crop(hydro_raster, DEM)
hydro_raster <- mask(hydro_raster, DEM) #need to mask hydro_raster to DEM extent because it will get burned into the DEM
#water bodies and channels stay at larger extent to calculate distance to water bodies and streams

writeRaster(hydro_raster, filename = paste0("./Hydrology_Processed/", unique(res(hydro_raster)), "hydro_raster.tif"),  #consider refering to created raster resolution instead of tautologically refering to given resolution
            drivername = "GTiff", overwrite = TRUE)

end_time <- Sys.time()
print(paste("End of [2] Creating hydrology files.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
