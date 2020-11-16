############################
# image segmentation of UAS 
# of Siberia UAS imagery
# 
# MML 11/16/20
############################

# clear environment
rm (list = ls())

# load required packages
library(rgdal)
library(raster)
library(sp)
library(rgdal)
library(caret)
library(randomForest)

# set working directory
setwd("L:/projects/siberia_uas_planet_comp")

#------------load data sets------------#

# load rbg flight from Cherskiy North Transect 1
tr1 <- stack("data/uas/CYN_TR1_FL016.tif")

# set band names for rgb image
names(tr1) <- c("B1", "B2", "B3")

# stereographic projection
st <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# set coordinate system
crs(tr1) <- st 

# read in training/validation data
lc <- readOGR("data/ground_truth/cyn_landover_pts.shp")

# create simplified land cover classes
lc$lc2 <- "ground"
lc$lc2[lc$Landcover=="water"] <- "water"
lc$lc2[lc$Landcover=="lichen"] <- "ground"
lc$lc2[lc$Landcover=="larch"|lc$Landcover=="shrub"] <- "tall"

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
rgbEx <- extract(tr1,lc)

# join the data
dataAll <- cbind(landExtract,rgbEx)

# create training and validation subsets
trainD <- dataAll[dataAll$sampleType=="train",]
validD<- dataAll[dataAll$sampleType=="valid",]

#------------Random Forest Classification of RGB data------------#
# run the Random Forest model
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:3) # number of variables available for splitting at each tree node

rf_model <- caret::train(x = trainD[,c(5:7)], #digital number data
                         y = as.factor(trainD$lcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model

# apply RF model to rgb data
rf_prediction <- raster::predict(tr1, model=rf_model,
                                 filename = "CYN_tr1_rgb_rf_lc.tif",
                                 progress = T)

#plot the data
plot(rf_prediction)

#------------Random Forest Classification of MS data------------#

