#creating separate data frame for derivatives based on gams of inividual years

library(mgcv) #load packages
library(ggplot2)
library(MASS)
library(lubridate)

Sys.setenv(GOOGLE_DRIVE = "~/Google Drive/Shared drives/Urban Ecological Drought")
google.drive <- Sys.getenv("GOOGLE_DRIVE")
path.google <- ("~/Google Drive/My Drive/")
pathShare <- file.path(path.google, "../Shared drives/Urban Ecological Drought/data/NDVI_drought_monitoring/")

source("~/Documents/GitHub/NDVI_drought_monitoring/0_Calculate_GAMM_Derivs_Copy.R")

######################
#loading in raw data from 01_raw_data.R
######################

#raw.data <- read.csv(file.path(google.drive, "data/NDVI_drought_monitoring/raw_data_k=12.csv"))
raw.data <- read.csv(file.path(google.drive, "data/NDVI_drought_monitoring/raw_data_k=12_with_wet-forest.csv"))

newDF <- data.frame(yday=seq(1:365)) #create new data frame to predict over

dat24 <- raw.data[raw.data$year==2024,] #subset for curent yr
nmonths <- length(unique(lubridate::month(dat24$date))) # Number of knots per month for 2024

df <- data.frame()

######################
#loop through LC types and years
######################

for (LC in unique(raw.data$type)){
  datLC <- raw.data[raw.data$type==LC,]
  
  for (yr in unique(datLC$year)){
    datyr <- datLC[datLC$year==yr,]
    
    if(yr==2024){
      gamyr <- gam(NDVIReprojected ~ s(yday, k=nmonths), data=datyr)
    }else{
      gamyr <- gam(NDVIReprojected ~ s(yday, k=12), data=datyr)
    }
    
    #gampred <- predict(gamyr, newdata=newDF)
    derivs <- calc.derivs(model.gam=gamyr, newdata=newDF, vars="yday")
    derivs$type <- LC
    derivs$year <- yr
    #post$NDVIpred <- gampred
    
    df <- rbind(df,derivs)
  }
}

#write.csv(df, file.path(pathShare, "k=12_individual_years_derivs_GAM.csv"), row.names=F) #save file

write.csv(df, file.path(pathShare, "k=12_individual_years_derivs_GAM_with_forest-wet.csv"), row.names=F) #save file

######################
