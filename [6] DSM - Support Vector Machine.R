##############################################################################################################################
##############################################################################################################################
###[6] DSM - Support Vector Machine.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

#Loading libraries
library(raster); library(rgdal); library(rgeos)
library(caret); library(doParallel)

#####Specifying resolution and filter
Raw_DEM_reso = 10
Filter_type = 100

#Create directories for response dataset
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/data"))}
#Create directories for model output
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Predicted_layers"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Predicted_layers"))}
if(!dir.exists(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Results"))){dir.create(paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Results"))}

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
#Cost
C <- c(0.01, 0.1, 1, 10, 100)
#Sigma
sigma <- c(0.01, 0.1, 1, 10, 100)
tune_grid <- expand.grid(C = C, sigma = sigma)

#How many folds of training data to analyse
num_folds <- 10
#How many times you sample from each fold
num_repeats <- 10

#SVM settings
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

Support_Vector_Machine_R_results <- data.frame(NULL)

####Looping over predictor dataset
Plain_Response_names <- c("M", "T")
df_Response_names <- c("MR", "TEXT")

##################################################################################
##################################################################################
#####Running Support Vector Machine
print("Running Support Vector Machine - Radial")
for (i in 1:length(df_Response_names)){
  Train_df <- df
  loop_Name <- which(colnames(Train_df) %in% df_Response_names[i]) #Column position of MR and TEXT etc.
  
  #Universal cleaning (removing spaces and converting to factor)
  Train_df[,loop_Name] <- as.character(Train_df[,loop_Name])
  Train_df <- Train_df[(Train_df[,loop_Name] != "") & (Train_df[,loop_Name] != " "),]
  Train_df[,loop_Name] <- as.factor(Train_df[,loop_Name])
  
  Equation <- as.formula(paste(df_Response_names[i], " ~", paste(Predictor_variable_names)))
  
  SVM_Train <- train( 
    data = Train_df,
    Equation,                               #Equation needs to be blank term I think?
    method = "svmRadial",                   #svmLinear - SVM with Linear classifier
    preProcess = c("center", "scale"),      #Predictor variables need to be normalized
    importance = T,                         #I believe this ranks order of importance for predictor variables
    tuneGrid = tune_grid,                   #Parameters to tune/optimize
    trControl = fitControl)
  
  saveRDS(SVM_Train, file = paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Results/", Plain_Response_names[i], "_SVM.rds"))

  #Saving model performance output - if multiple parameter values give optimal accuracy, report this
  if(nrow(SVM_Train$results[SVM_Train$results$Accuracy == max(SVM_Train$results$Accuracy),]) > 1){print(paste0("Multiple solutions for SVM Radial", Plain_Response_names[i]))}
  
  Model_performance <- SVM_Train$results[rownames(SVM_Train$results) == rownames(SVM_Train$bestTune),]
  Model_performance$Variable <- Plain_Response_names[i]
  Support_Vector_Machine_R_results <- rbind(Support_Vector_Machine_R_results, Model_performance)
  
  print(paste0("Done ", Plain_Response_names[i], " for Support Vector Machine - Radial"))
  }

#Write out csv
Support_Vector_Machine_R_results$Model <- "SVM-R"
write.csv(Support_Vector_Machine_R_results, paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/Support_Vector_Machine_R/Results/SVM_R_Model_performances.csv"), row.names = F)
end_time <- Sys.time()
print(paste("End of DSM - Support Vector Machine.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
