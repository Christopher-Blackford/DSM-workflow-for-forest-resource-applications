##############################################################################################################################
##############################################################################################################################
###[6] DSM - k-Nearest Neighbour.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#Loading libraries
library(raster); library(rgdal); library(rgeos)
library(caret); library(doParallel)
library(kknn) #- this one it doesn't need to be called once you install it because caret will pick it up but it doesn't get installed with caret

#####Specifying resolution and filter
Raw_DEM_reso = 10
Filter_type = 100


#Create directories for response dataset - these are likely already created
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data"))}
#Create directories for model output
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Predicted_layers"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Predicted_layers"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Results"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Results"))}

#This runs your code in parallel which is useful
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


#####
#####Load in soil dataset if it exists, create it if it doesn't
if(file.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data/", Raw_DEM_reso, "DEM", Filter_type,"Response_df.csv"))){print("Loading previous filter dataset"); df <- read.csv(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data/", Raw_DEM_reso, "DEM", Filter_type,"Response_df.csv"))
  }else{print("Creating filter dataset")
  #Loading response variables (moisture, texture, depth class, depth mottle, mottle class)
  Response_df <- read.csv("./Models/Soil_data/Soil_response_data.csv")
  Response_df$ID <- 1:nrow(Response_df)
  
  xy <- subset(Response_df, select = c(X, Y))
  Response_points <- SpatialPointsDataFrame(coords = xy, data = Response_df, proj4string = crs(Covariates)) #Covariates@crs may need to be replaced with CRS(Covariates)

  #Extract covariate points to response data
  rasValue <- raster::extract(Covariates, Response_points, method = "simple")
  df <- as.data.frame(rasValue); df$ID <- 1:nrow(df)
  df <- merge(Response_df, df) #Quite a couple of non-overlapping points actually, double check this?
  df <- df[complete.cases(df),]
  
  row.names(df) <- 1:nrow(df)
  write.csv(df, paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data/", Raw_DEM_reso, "DEM", Filter_type,"Response_df.csv"), row.names=FALSE)
  df <- read.csv(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data/", Raw_DEM_reso, "DEM", Filter_type,"Response_df.csv")) #Need to read csv so it treats geo and bio colums as integers
  
  rm(xy,Response_df,rasValue,Response_points)
  }

df$X <- as.numeric(df$X); df$Y <- as.numeric(df$Y)
Predictor_variable_names <- paste(names(Covariates), collapse="+")
#####
#####


###Setting up values your parameters could take for tuning
#Number of neighbors to inform classification
k <- seq(from = 1, to = 31, by = 2)
tune_grid <- expand.grid(k=k)
k = c(3,5,11)
#How many folds of training data to analyse
num_folds <- 5
#How many times you sample from each fold
num_repeats <- 5

#kNN settings
fitControl <- trainControl(
  method = "repeatedcv",    #repeated cross-validation      
  number = num_folds,       #number of folds  
  repeats = num_repeats,    #number of times you sample a fold (k-fold)                 
  returnResamp = "all",     #tells you which sample in train() function were used to train (bootstrapped) and which were left out (see: kNN_Train$control$index)       
  allowParallel = TRUE,
  savePredictions = "final"
  )

#Making legitimate Moisture regime class - R has issues ordering numbers and character inputs as factors so this is a safe way to code them as factors
df$MR <- as.character(df$MOISTURE_R)
for (i in 1:nrow(df)){
  if(df$MR[i] != ""){df$MR[i] <- paste0("C", df$MR[i])}
}

df$MR <- gsub(pattern = "C-1", replacement = "CN1", df$MR) 
df$MR <- as.factor(df$MR)

#Setting up dataframe that will show model performance later
k_Nearest_Neighbour_results <- data.frame(NULL)

####Looping over predictor dataset
Plain_Response_names <- c("M", "T")
df_Response_names <- c("MR", "TEXT")


##################################################################################
##################################################################################
#####Running k-Nearest Neighbour
print("Running k-Nearest Neighbour")
#We are looping so we can look at Moisture Regime and Textural class
for (i in 1:length(df_Response_names)){
  Train_df <- df
  loop_Name <- which(colnames(Train_df) %in% df_Response_names[i]) #Column position of MR and TEXT etc.
  
  #Universal cleaning (removing spaces and converting to factor)
  Train_df[,loop_Name] <- as.character(Train_df[,loop_Name])
  Train_df <- Train_df[(Train_df[,loop_Name] != "") & (Train_df[,loop_Name] != " "),]
  Train_df[,loop_Name] <- as.factor(Train_df[,loop_Name])
  
  Equation <- as.formula(paste(df_Response_names[i], " ~", paste(Predictor_variable_names)))
  
  kNN_Train <- train( 
    data = Train_df,
    Equation,                               #Equation needs to be blank term I think?
    method = "knn",                        #kknn - k-Nearest Neighbour
    preProcess = c("center", "scale"),      #Predictor variables need to be normalized
    #importance = T,                         #Ranks order of importance for predictor variables
    tuneGrid = tune_grid,                   #Parameters to tune/optimize
    trControl = fitControl)
  saveRDS(kNN_Train, file = paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Results/", Plain_Response_names[i], "_kNN.rds"))
  
  #Saving model performance output - if multiple parameter values give optimal accuracy, report this
  if(nrow(kNN_Train$results[kNN_Train$results$Accuracy == max(kNN_Train$results$Accuracy),]) > 1){print(paste0("Multiple solutions for kNN ", Plain_Response_names[i]))}
  
  Model_performance <- kNN_Train$results[rownames(kNN_Train$results) == rownames(kNN_Train$bestTune),]
  Model_performance$Variable <- Plain_Response_names[i]
  k_Nearest_Neighbour_results <- rbind(k_Nearest_Neighbour_results, Model_performance)
  
  print(paste0("Done ", Plain_Response_names[i], " for k-Nearest Neighbour"))
  }

#Write out csv
k_Nearest_Neighbour_results$Model <- "kNN"
write.csv(k_Nearest_Neighbour_results, paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/k_Nearest_Neighbour/Results/kNN_Model_performances.csv"), row.names = F)
end_time <- Sys.time()
print(paste("End of DSM - k-Nearest Neighbour.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END