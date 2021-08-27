############################
# RF classification of UAS 
# of Siberia UAS imagery
# 
# MML 11/16/20
############################

# clear environment
rm (list = ls())

# load required packages
library(rgdal)
library(raster)
#library(sp)
library(caret)
library(randomForest)
#library(tidyverse)

# set working directory
setwd("L:/projects/siberia_uas_planet_comp")

#------------load data sets---------
## 
#rbg flight from Cherskiy North Transect 1
tr1 <- stack("data/uas/CYN_TR1_FL016.tif")

# set the NA value 
NAvalue(tr1) <- 255
# set band names for rgb image
names(tr1) <- c("B1", "B2", "B3")

# stereographic projection
st <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# set coordinate system
crs(tr1) <- st 

##
# read parrot ms data
tr1m <- stack("data/processed/RU_CYN_TR1_FL016M_allbands_rgb_mask.tif")

# set band names for ms image
names(tr1m) <- c("green", "red", "rededge", "nir", "ndvi", "ndwi")

##
# read planet data
pl <- stack("data/processed/CYN_TR1_rgb_mask_20190706_002918_101b_3B_AnalyticMS_SR.tif")

names(pl) <- c("blue", "green", "red", "nir", "ndvi", "ndwi")

# read in training/validation data
lc <- readOGR("data/ground_truth/cyn_landover_pts.shp")

# create simplified land cover classes
lc$lc2 <- "ground"
lc$lc2[lc$Landcover=="water"] <- "water"
lc$lc2[lc$Landcover=="lichen"] <- "ground"
lc$lc2[lc$Landcover=="larch"|lc$Landcover=="shrub"] <- "tall"

# write updated training data
writeOGR(lc, "data/processed/CNY_TR1_lc_updated.shp", driver = "ESRI Shapefile", layer = "lc2")

# create data frame for validation/training
landExtract <- data.frame(lcID = as.numeric(as.factor(lc$lc2)),
                          x = lc@coords[,1],
                          y = lc@coords[,2])
                          
landClass <- data.frame(lcID = 1:3,
                   landCover = c ("ground", "tall", "water"))

# specify training and validation data sets
set.seed(11213)

# randomly select half of the records
sampleSamp <- sample(seq(1,404),202)

landExtract$sampleType <- "train"

landExtract$sampleType[sampleSamp] <- "valid"

# extract raster values for training data points
rgbEx <- raster::extract(tr1,lc)
msEx <- raster::extract(tr1m,lc)
plEx <- raster::extract(pl,lc)

# join the data
dataAll <- cbind(landExtract,rgbEx)
dataAllms <- cbind(landExtract,msEx)
dataAllpl <- cbind(landExtract,plEx)

# create training and validation subsets
trainD <- dataAll[dataAll$sampleType=="train",]
validD<- dataAll[dataAll$sampleType=="valid",]

trainDms <- dataAllms[dataAllms$sampleType=="train",]
validDms <- dataAllms[dataAllms$sampleType=="valid",]

trainDpl <- dataAllpl[dataAllpl$sampleType=="train",]
validDpl <- dataAllpl[dataAllpl$sampleType=="valid",]

#------------Random Forest Classification of RGB data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:2) # number of variables available for splitting at each tree node

rf_model <- caret::train(x = trainD[,c(5:7)], #digital number data
                         y = as.factor(trainD$lcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model

# evaluate validation data
confusionMatrix(predict(rf_model,validD[,5:7]),as.factor(validD$lcID))

# apply RF model to rgb data
rf_prediction <- raster::predict(tr1, model=rf_model,
                                 filename = "data/processed/CYN_tr1_rgb_rf_lc.tif",
                                 overwrite = T, progress = T)

#plot the data
plot(rf_prediction)

#------------Random Forest Classification of MS data------------#
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node

rf_model_ms <- caret::train(x = trainDms[,c(5:10)], #digital number data
                         y = as.factor(trainDms$lcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model_ms

# apply RF model to rgb data
rf_prediction_ms <- raster::predict(tr1m, model=rf_model_ms,
                                 filename = "data/processed/CYN_tr1_ms_rf_lc.tif",
                                 overwrite = T, progress = T)

#plot the data
plot(rf_prediction_ms)

confusionMatrix(predict(rf_model_ms,validDms[,5:10]),as.factor(validDms$lcID))

#------------Random Forest Classification of Planet data------------
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node

rf_model_pl <- caret::train(x = trainDpl[,c(5:10)], #digital number data
                            y = as.factor(trainDpl$lcID), #land class we want to predict
                            method = "rf", #use random forest
                            metric="Accuracy", #assess by accuracy
                            trainControl = tc, #use parameter tuning method
                            tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model_pl

# apply RF model to Planet data
rf_prediction_pl <- raster::predict(pl, model=rf_model_pl,
                                    filename = "data/processed/CYN_tr1_planet_rf_lc.tif",
                                    overwrite = T, progress = T)

#plot the data
plot(rf_prediction_pl)

confusionMatrix(predict(rf_model_pl,validDpl[,5:10]),as.factor(validDpl$lcID))
#------------Random Forest Classification of Planet data with validation from MS land cover ------------

# aggregate multispectral lc map to 3m resolution using modal function
lcp <- aggregate(rf_prediction_ms,fact = 47, fun=modal)
lcp <- resample(lcp,pl,method = "ngb",
                filename = "data/processed/CYN_tr1_ms_rf_lc_3m.tif",
                overwrite = T, progress = T)

# aggregate rgb lc map to 3m resolution using modal function
lcp2 <- aggregate(rf_prediction,fact = 183, fun=modal)
lcp2 <- projectRaster(lcp2,pl,method = "ngb",
                      filename = "data/processed/CYN_tr1_rgb_rf_lc_3m.tif",
                      overwrite = T, progress = T)

datLCr <- dataAllpl
datLCm <- dataAllpl
datLCr$lcID <- raster::extract(lcp2,lc)
datLCm$lcID <- raster::extract(lcp,lc)

r <- which(duplicated(datLCr[,5:10])==F)

datLCm <- datLCm[r,]
datLCr <- datLCr[r,]

trainLCr <- datLCr[datLCr$sampleType=="train",]
validLCr <- datLCr[datLCr$sampleType=="valid",]

trainLCm <- datLCm[datLCm$sampleType=="train",]
validLCm <- datLCm[datLCm$sampleType=="valid",]

# model from ms image
rf_model_pl_ms <- caret::train(x = trainLCm[,c(5:10)], #digital number data
                            y = as.factor(trainLCm$lcID), #land class we want to predict
                            method = "rf", #use random forest
                            metric="Accuracy", #assess by accuracy
                            trainControl = tc, #use parameter tuning method
                            tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model_pl_ms

# apply RF model to Planet data
rf_prediction_pl_ms <- raster::predict(pl, model=rf_model_pl_ms,
                                    filename = "data/processed/CYN_tr1_planet_rf_lc_ms.tif",
                                    overwrite = T, progress = T)

#plot the data
plot(rf_prediction_pl_ms)

# model from rgb image
rf_model_pl_r <- caret::train(x = trainLCr[,c(5:10)], #digital number data
                               y = as.factor(trainLCr$lcID), #land class we want to predict
                               method = "rf", #use random forest
                               metric="Accuracy", #assess by accuracy
                               trainControl = tc, #use parameter tuning method
                               tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model_pl_r

# apply RF model to Planet data
rf_prediction_pl_r <- raster::predict(pl, model=rf_model_pl_r,
                                       filename = "data/processed/CYN_tr1_planet_rf_lc_rgb.tif",
                                       overwrite = T, progress = T)

#plot the data
plot(rf_prediction_pl_r)
