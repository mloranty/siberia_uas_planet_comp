############################
# code to compare UAS 
# and Planet imagery
# for Siberia research sites
# 
# MML 10/28/20
############################

rm (list = ls())

library(rgdal)
library(raster)

setwd("L:/projects/siberia_uas_planet_comp")

# load planet analytic image from 6 July 2019
pl <- stack("data/CYN_06_Jul_2019_PSScene4Band_Explorer/files/PSScene4Band/20190706_002918_101b/analytic_sr_udm2/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# load rbg flight from Cherskiy North Transect 1
tr1 <- stack("data/uas/CYN_TR1_FL016.tif")
# set coordinate system
crs(tr1) <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# load rbg flight from Cherskiy North Transect 1
tr2 <- stack("data/uas/CYN_TR2_FL017.tif")
# set coordinate system
crs(tr2) <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# create merged extent object to clip Planet image
x <- merge(extent(tr1),extent(tr2))
