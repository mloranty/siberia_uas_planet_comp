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