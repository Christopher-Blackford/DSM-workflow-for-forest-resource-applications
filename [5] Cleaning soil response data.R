##############################################################################################################################
##############################################################################################################################
###[5] Cleaning soil response data.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

######Loading libraries
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(ggplot2)

#####Specifying resolution - just using DEM to clip
Raw_DEM_reso = 10
DEM <- raster(paste0("./Raw/DEM_Raw/", Raw_DEM_reso, "DEM.tif"))

#####Create directories
if(!dir.exists("./Models/Soil_data/graphics")){dir.create("./Models/Soil_data/graphics")}
if(!dir.exists("./Models/Soil_data/spatial_file")){dir.create("./Models/Soil_data/spatial_file")}

#####Load data
df <- read.csv("./Models/Soil_data/Response_Raw.csv", stringsAsFactors = FALSE)
row.names(df) <- 1:nrow(df); str(df)

#####Make graph of soil classes of response data before cleaning
for (i in 3:ncol(df)){
  ggplot(data=as.data.frame(table(df[,colnames(df[i])])), aes(x=reorder(Var1,-Freq), y=Freq))+
    geom_bar(stat="identity")+
    labs(x = "Class", y = "Count")+
    theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.25))
  ggsave(paste0("./Models/Soil_data/graphics/Original_", colnames(df[i]), "_factors.png"), width = 10, height = 6)
  }

#####Changing NAs to blank values (since no NAs in coordinates this will just act on soil attributes)
df[is.na(df)] <- ""

########################################################################
#####Moisture
All_pdf_MoistureClasses <- c("-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "x", "h", "s") #All legitimate classes

df$MOISTURE_R[-which(df$MOISTURE_R %in% All_pdf_MoistureClasses)] = "" #If there's a non-legitimate record, remove it

#Output histogram of cleaning moisture regime values
ggplot(data=as.data.frame(table(df[,"MOISTURE_R"])), aes(x=reorder(Var1,-Freq), y=Freq)) +
  geom_bar(stat="identity")+
  labs(x = "Class", y = "Count")+
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.25))
ggsave(paste0("./Models/Soil_data/graphics/Removed_MOISTURE_R_factors.png"), width = 10, height = 6)


########################################################################
#####Texture
All_pdf_TextClasses <- c("vcS", "cS", "mS", "fS", "LvcS", "LcS", "LmS", "LfS",
                      "LvfS", "SivcS",
                      "SicS", "SimS", "SifS", "SivfS", "L", "vcSL", "cSL", "mSL", "fSL", "vfSL",
                      "SiC", "SC"," C",
                      "Si", "SiL", "SiCL", "SCL", "CL") #these come from page 28 in computer field guide and are all the legitimate values soil texture could possibly take
All_pdf_DecompClasses <- c("Of1", "Of2", "Of3", "Of4", "Om5", "Om6", "Oh7", "Oh8", "Oh9", "Oh10") #these come from page 26 in hard copy guide

All_pdf_TEXT <- c(All_pdf_TextClasses, All_pdf_DecompClasses); rm(All_pdf_TextClasses, All_pdf_DecompClasses) #All legitimate classes

#My cleaning attempt to correct captilization issues, Osbeing coded as zeros, non-legitimate classes etc.
df$TEXT <- gsub(pattern = "N/A|n/a", replacement = "", df$TEXT) 
df$TEXT <- gsub(pattern = "0|o", replacement = "O", df$TEXT)
df$TEXT <- gsub(pattern = "F", replacement = "f", df$TEXT)
df$TEXT <- gsub(pattern = "I", replacement = "i", df$TEXT)
df$TEXT <- gsub(pattern = " .", replacement = "", df$TEXT)

df$TEXT[-which(df$TEXT %in% All_pdf_TEXT)] = "" #Remove non-legitimate classes

#Changing texture codes to character explanations (e.g. mS becomes "Sand")
Texture_key <- read.csv("./Raw/Response_data/Soil texture almagamation - detailed.csv"); Texture_key <- Texture_key[,-3]
df <- merge(df, Texture_key, by.x = "TEXT", by.y = "Original", all.x = TRUE)

#Converting NAs to blanks while keeping as factor
levels(df$TEXT_Reclass) <- c(levels(df$TEXT_Reclass),"")  #Add the extra level to your factor
df$TEXT_Reclass[is.na(df$TEXT_Reclass)] <- ""  
df$TEXT = NULL
df <- dplyr::rename(df, TEXT = TEXT_Reclass)

ggplot(data=as.data.frame(table(df[,"TEXT"])), aes(x=reorder(Var1,-Freq), y=Freq)) +
  geom_bar(stat="identity")+
  labs(x = "Class", y = "Count")+
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.25))
ggsave(paste0("./Models/Soil_data/graphics/Removed_TEXT_factors.png"), width = 10, height = 6)

########################################################################
#####Clip spatial response data to raster
xy <- subset(df, select = c(X, Y))
Response_points <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = DEM@crs)
Response_points@data$ID <- 1:nrow(Response_points@data)

rasValue <- raster::extract(DEM, Response_points, method = "simple")
df <- as.data.frame(rasValue); df$ID <- 1:nrow(df)
df <- merge(Response_points@data, df, by = "ID") 
df <- df[!is.na(df$rasValue),]; df$rasValue <- NULL #Removing points that are in raster extent (i.e. raster square) but don't overlap with DEM

df[,1] = NULL; df <- unique(df) #Remove duplicated points

###Output final csv
write.csv(df, "./Models/Soil_data/Soil_response_data.csv", row.names=F)

###Output spatial file of points because it's useful
xy <- subset(df, select = c(X, Y))
Response_points <- SpatialPointsDataFrame(coords = xy, data = df, proj4string = DEM@crs)
writeOGR(Response_points, dsn = "./Models/Soil_data/spatial_file", layer = "All_points_clean",
         driver = "ESRI Shapefile", overwrite = TRUE, morphToESRI = TRUE)

end_time <- Sys.time()
print(paste("End of [5] Cleaning soil response data.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END