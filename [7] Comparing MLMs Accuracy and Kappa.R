##############################################################################################################################
##############################################################################################################################
###[7] Comparing MLMs Accuracy and Kappa.R
#Code by: Christopher Blackford (christopher.blackford@canada.ca)
start_time <- Sys.time()

library(tidyverse)

#####Specifying resolution
Raw_DEM_reso = 10
Filter_type = 100

df <- list.files(path = paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type), pattern = "Model_performances.csv",
                   recursive = TRUE, full.names = TRUE)
df <- lapply(X = df, FUN = read.csv)
df <- Reduce(function(x,y) merge(x = x, y = y, all = T), df)
  
df <- df[,c("Model", "Variable", "Accuracy","Kappa")] 

write.csv(df, paste0("./Models/", Raw_DEM_reso, "DEM", Filter_type, "/MLMs_comparative_performance.csv"), row.names = F)

end_time <- Sys.time()
print(paste("End of [7] Comparing MLMs Accuracy and Kappa.R ", capture.output(end_time - start_time)))
#####
####
###
##
#END
