############################
# image segmentation of UAS 
# of Siberia UAS imagery
# 
# MML 11/13/20
############################

rm (list = ls())

library(rgdal)
library(raster)
library(OpenImageR)
library(SuperpixelImageSegmentation)

setwd("L:/projects/siberia_uas_planet_comp")

# stereographic projection
st <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# load rbg flight from Cherskiy North Transect 1
tr1 <- stack("data/uas/CYN_TR1_FL016.tif")
tr1i <- readImage("data/uas/CYN_TR1_FL016.tif", convert = T)
# set coordinate system
crs(tr1) <- st 

Region.slic <- superpixels(input_image = tr1i, method = "slic", superpixel = 80,
                          compactness = 30, return_slic_data = TRUE,
                          return_labels = TRUE, write_slic = "data/CNY_try_segment.bin",
                          verbose = FALSE)