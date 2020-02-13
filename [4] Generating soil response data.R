##############################################################################################################################
##############################################################################################################################
###[4] Generating soil response data.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#####Loading libraries
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)

#####Specifying resolution - just using DEM to clip
Raw_DEM_reso = 10
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))
DEM_extent <- as(extent(DEM), 'SpatialPolygons'); DEM_extent@proj4string <- DEM@crs

#####Create directories
if(!dir.exists("./Models")){dir.create("./Models")}
if(!dir.exists("./Models/Soil_data")){dir.create("./Models/Soil_data")}


########################################################################
#####Load original soil data files

###AFRIT Folder
new_fri_pts <- readOGR("./Raw/Response_data/AFRIT_Hearst_files", "new_fri_pts")
Hearst_Plots <- read.csv("./Raw/Response_data/AFRIT_Hearst_files/AFRIT-Hearst_Plots_and_Attributes.csv")

###Hearst_eFRI Folder
Calibration_Plots_2D <- readOGR("./Raw/Response_data/Hearst_eFRI_files/HF-601-2D.gdb", "Calibration_Plots_2D")
PLOT_MASTER <- read.csv("./Raw/Response_data/Hearst_eFRI_files/PLOT_MASTER.csv")
Calibration_Plots_2D <- sp::merge(Calibration_Plots_2D, PLOT_MASTER)
rm(PLOT_MASTER)

###Growth and Yield Data Folder
GY_csv1 <- read.csv("./Raw/Response_data/Growth_Yield_files/GY_tblSoilSample.csv")
GY_csv2 <- read.csv("./Raw/Response_data/Growth_Yield_files/GY_tmpFCLatLong.csv")
GY_csv <- merge(GY_csv1, GY_csv2, all = F); rm(GY_csv1, GY_csv2)

NFI_csv <- read.csv("./Raw/Response_data/Growth_Yield_files/NFI_tblPlot.csv")

###ELC/FEC data
ELC1 <- read.csv("./Raw/Response_data/ELC_FEC_files/ELC_FEC_tbl_Location_Data.csv")
ELC2 <- read.csv("./Raw/Response_data/ELC_FEC_files/ELC_FEC_tbl_Plot_Pedons.csv")
ELC <- merge(ELC1, ELC2, by = "Orig_Plot_Name")
rm(ELC1, ELC2)


########################################################################
#####Making plot data spatial

###Hearst plots
Hearst_Plots <- Hearst_Plots[c("POINT_X", "POINT_Y", "MOISTURE.REGIME", "EffECTIvE.TEXTURE")]
xy <- subset(Hearst_Plots, select = c(POINT_X, POINT_Y))
Hearst_Plots <- SpatialPointsDataFrame(coords = xy, data = Hearst_Plots, proj4string = DEM@crs)

###ELC plots
ELC <- ELC[c("Orig_Plot_Name", "latdd83", "longdd83", "moisture_regime", "effective_texture_for_moisture")]
ELC <- dplyr::rename(ELC, MOISTURE_R=moisture_regime, TEXT=effective_texture_for_moisture)

xy <- subset(ELC, select = c(longdd83, latdd83))
ELC <- SpatialPointsDataFrame(coords = xy, data = ELC, proj4string = crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"))
ELC <- spTransform(ELC, DEM@crs)
ELC <- ELC[c("MOISTURE_R", "TEXT")]
ELC@data$MOISTURE_R <- as.factor(ELC@data$MOISTURE_R)

###Calibration plots line to points
Calibration_Plots_2D@data$coord.x <- 0
Calibration_Plots_2D@data$coord.y <- 0
for (i in 1:nrow(Calibration_Plots_2D@data)){
  coord_total <- Calibration_Plots_2D@lines[[i]]@Lines[[1]]@coords
  #Find midpoint since need to convert line to point and this is approximately where soil data was taken
  coord.x <- (coord_total[1,1] + coord_total[2,1])/2
  coord.y <- (coord_total[1,2] + coord_total[2,2])/2
  
  Calibration_Plots_2D@data$coord.x[i] <- coord.x
  Calibration_Plots_2D@data$coord.y[i] <- coord.y
}
xy <- subset(Calibration_Plots_2D@data, select = c(coord.x, coord.y))
Calibration_Plots_2D <- SpatialPointsDataFrame(coords = xy, data = Calibration_Plots_2D@data, proj4string = Calibration_Plots_2D@proj4string)
rm(coord.x, coord.y, coord_total)

###Growth and Yield plots
GY_csv <- GY_csv[c("longitude", "latitude", "MoistRegimeCode")]
GY_csv <- dplyr::rename(GY_csv, MOISTURE_R=MoistRegimeCode)

xy <- subset(GY_csv, select = c(longitude, latitude))
GY_spatial <- SpatialPointsDataFrame(coords = xy, data = GY_csv, proj4string = crs("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")) #Covariates@crs may need to be replaced with CRS(Covariates)
GY_spatial <- spTransform(GY_spatial, DEM@crs)
GY_spatial <- GY_spatial[c("MOISTURE_R")]


NFI_csv <- NFI_csv[c("CentreZone", "CentreEast", "CentreNorth", "MoistRegimeCode")]
NFI_csv <- dplyr::rename(NFI_csv, MOISTURE_R=MoistRegimeCode)

xy <- subset(NFI_csv, select = c(CentreEast, CentreNorth))
NFI_spatial <- SpatialPointsDataFrame(coords = xy, data = NFI_csv, proj4string = crs(DEM)) #Covariates@crs may need to be replaced with CRS(Covariates)
NFI_spatial <- NFI_spatial[NFI_spatial@data$CentreZone == 17,]
NFI_spatial <- NFI_spatial[c("MOISTURE_R")]
rm(GY_csv, NFI_csv)

###Setting crs/projection
new_fri_pts@proj4string <- DEM@crs #These are the same projection, just stored differently which is why it's okay to do this
Calibration_Plots_2D@proj4string <- DEM@crs #These are the same projection, just stored differently which is why it's okay to do this


########################################################################
#####Organizing spatial data

###Clipping all data to study extent
new_fri_pts <- new_fri_pts[DEM_extent,]
Hearst_Plots <- Hearst_Plots[DEM_extent,]
Calibration_Plots_2D <- Calibration_Plots_2D[DEM_extent,]
GY_spatial <- GY_spatial[DEM_extent,]; NFI_spatial <- NFI_spatial[DEM_extent,]
ELC <- ELC[DEM_extent,]

#Selecting only releveant columns
new_fri_pts <- new_fri_pts[c("MR", "TextClass")]
Hearst_Plots <- Hearst_Plots[c("MOISTURE.REGIME", "EffECTIvE.TEXTURE")]
Calibration_Plots_2D <- Calibration_Plots_2D[c("MST_REG_1", "SOIL_TXT_1")]

#Making column names the same so datasets can be merged
new_fri_pts@data <- dplyr::rename(new_fri_pts@data, TEXT=TextClass, MOISTURE_R=MR)
Hearst_Plots@data <- dplyr::rename(Hearst_Plots@data, MOISTURE_R=MOISTURE.REGIME, TEXT=EffECTIvE.TEXTURE)
Calibration_Plots_2D@data <- dplyr::rename(Calibration_Plots_2D@data, TEXT=SOIL_TXT_1, MOISTURE_R=MST_REG_1)

#####Creating X and Y coordinates for each point file
new_fri_pts@data$X <- new_fri_pts@coords[,1]; new_fri_pts@data$Y <- new_fri_pts@coords[,2]
Hearst_Plots@data$X <- Hearst_Plots@coords[,1]; Hearst_Plots@data$Y <- Hearst_Plots@coords[,2];
Calibration_Plots_2D@data$X <- Calibration_Plots_2D@coords[,1]; Calibration_Plots_2D@data$Y <- Calibration_Plots_2D@coords[,2]

GY_spatial@data$X <- GY_spatial@coords[,1]; GY_spatial@data$Y <- GY_spatial@coords[,2]
NFI_spatial@data$X <- NFI_spatial@coords[,1]; NFI_spatial@data$Y <- NFI_spatial@coords[,2]

ELC@data$X <- ELC@coords[,1]; ELC@data$Y <- ELC@coords[,2]

#####Adding dataset identifier
GY_spatial@data$dataset <- "GY"
NFI_spatial@data$dataset <- "NFI"
new_fri_pts@data$dataset <- "FRI"
Hearst_Plots@data$dataset <- "HEARST_PLOTS"
Calibration_Plots_2D@data$dataset <- "CAL_PLOTS"
ELC@data$dataset <- "ELC_FEC"

########################################################################
#####Merging all datasets together
df <- merge(GY_spatial@data, NFI_spatial@data, all=T)
df <- merge(df, new_fri_pts@data, all=T)
df <- merge(df, Hearst_Plots@data, all=T)
df <- merge(df, Calibration_Plots_2D@data, all=T)
df <- merge(df, ELC@data, all=T)

df <- df[c("X", "Y", "dataset", "MOISTURE_R", "TEXT")]

write.csv(df, "./Models/Soil_data/Response_Raw.csv", row.names=F)

end_time <- Sys.time()
print(paste("End of [4] Generating soil response data.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END