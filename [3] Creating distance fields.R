##############################################################################################################################
##############################################################################################################################
###[3] Creating distance fields.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca), with thanks to Daniel Saurette at the Ontario Ministry of Agriculture, Food and Rural Affairs for code outline
start_time <- Sys.time()

#Load in libraries
library(raster)
library(rgdal)

#####Specifying resolution
Raw_DEM_reso = 10

#####Load in DEM
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

if(!dir.exists("./temp")){dir.create("./temp")} #These lines are useful as they store potentially large raster temporary files in a recognizable location instead of default C drive
rasterOptions(tmpdir="./temp")

#####Create directories
if(!dir.exists("./Predictor_Variables")){dir.create("./Predictor_Variables")}
if(!dir.exists("./Predictor_Variables/DistanceFields")){dir.create("./Predictor_Variables/DistanceFields")}
if(!dir.exists(paste0("./Predictor_Variables/DistanceFields/",  Raw_DEM_reso, "DEM"))){dir.create(paste0("./Predictor_Variables/DistanceFields/", Raw_DEM_reso, "DEM"))}


######Create a euclidean distance field along the axes: x, y, Northeast, Southeast, and centre

#Convert the raster to a dataframe for calculating the XDIST and YDIST
df<- as(DEM,"SpatialPointsDataFrame")
df<- as.data.frame(df@coords)
# Calculate xdist and ydist, the euclidean distance along the x and y axis respectively
df$xdist<- DEM@extent@xmax - df$x
df$ydist<- DEM@extent@ymax - df$y

#Convert xdist to a raster
xgrid<- subset(df,select=c("x","y",'xdist'))
coordinates(xgrid)<- c("x","y")
gridded(xgrid)<-TRUE
xgrid <- raster(xgrid)
xgrid <- resample(xgrid,DEM,method="ngb")

projection(xgrid)<-crs(DEM)
xgrid<- mask(x = xgrid, mask = DEM)
writeRaster(xgrid, "./Predictor_Variables/DistanceFields/DIST_X.tif", overwrite = TRUE)
rm(xgrid)

#Convert ydist to a raster
ygrid<- subset(df,select=c("x","y",'ydist'))
coordinates(ygrid)<- c("x","y")
gridded(ygrid)<-TRUE
ygrid<-raster(ygrid)
ygrid<- resample(ygrid,DEM,method="ngb")

projection(ygrid)<-crs(DEM)
ygrid<- mask(x = ygrid, mask = DEM)
writeRaster(ygrid, "./Predictor_Variables/DistanceFields/DIST_Y.tif", overwrite = TRUE)
rm(ygrid)

#Generate points representing the 4 corners of study extent and the center of study extent
nw  <- c(DEM@extent@xmin,DEM@extent@ymax)
sw  <- c(DEM@extent@xmin,DEM@extent@ymin)
ne  <- c(DEM@extent@xmax,DEM@extent@ymax)
se  <- c(DEM@extent@xmax,DEM@extent@ymin)
mid <- c(round(DEM@extent@xmax - ((DEM@extent@xmax - DEM@extent@xmin)/2),0),round(DEM@extent@ymax - ((DEM@extent@ymax - DEM@extent@ymin)/2),0))

#Calculate distance from NW point and output raster
NW<- distanceFromPoints(DEM,nw)
NW<- mask(x = NW, mask = DEM)
projection(NW)<-crs(DEM)
writeRaster(NW, "./Predictor_Variables/DistanceFields/DIST_NW.tif", overwrite = TRUE)

#Calculate distance from NW point and output raster
SW<- distanceFromPoints(DEM,sw)
SW<- mask(x = SW, mask = DEM)
projection(SW)<-crs(DEM)
writeRaster(SW, "./Predictor_Variables/DistanceFields/DIST_SW.tif", overwrite = TRUE)

#Calculate distance from NW point and output raster
NE<- distanceFromPoints(DEM,ne)
NE<- mask(x = NE, mask = DEM)
projection(NE)<-crs(DEM)
writeRaster(NE, "./Predictor_Variables/DistanceFields/DIST_NE.tif", overwrite = TRUE)

#Calculate distance from SE point and output raster
SE<- distanceFromPoints(DEM,se)
SE<- mask(x = SE, mask = DEM)
projection(SE)<-crs(DEM)
writeRaster(SE, "./Predictor_Variables/DistanceFields/DIST_SE.tif", overwrite = TRUE)

#Calculate distance from Centre and output raster
ctr<- distanceFromPoints(DEM,mid)
ctr<- mask(x = ctr, mask = DEM)
projection(ctr)<-crs(DEM)
writeRaster(ctr, "./Predictor_Variables/DistanceFields/DIST_MID.tif", overwrite = TRUE)

end_time <- Sys.time()
print(paste("End of [3] Creating distance fields.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END