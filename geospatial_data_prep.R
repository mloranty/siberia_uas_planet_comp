############################
# code to compare UAS 
# and Planet imagery
# for Siberia research sites
# 
# MML 10/28/20
############################

rm (list = ls())

# load required packages
library(rgdal)
library(raster)
library(sp)
library(rgdal)

# set working directory
setwd("L:/projects/siberia_uas_planet_comp")

#------------define projections------------#
# stereographic projection (rgb uas data)
st <- "+proj=stere +lat_0=90 +lat_ts=71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" 

# North Pole Lambert Azimuthal Equal Area
laea <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Kolyma albers equal area
aea <- "+proj=aea +lat_1=25 +lat_2=75 +lat_0=60 +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#------------load rgb uas data------------#
# Cherskiy North Transect 1
tr1 <- stack("data/uas/CYN_TR1_FL016.tif")
# set coordinate system

crs(tr1) <- st 
# Cherskiy North Transect 2
tr2 <- stack("data/uas/CYN_TR2_FL017.tif")
# set coordinate system
crs(tr2) <- st 

# create from merged extent object to clip Planet image
x1 <- projectExtent(tr1,crs(pl))
x2 <- projectExtent(tr2,crs(pl))

x <- merge(extent(projectExtent(tr1,crs(pl))),
           extent(projectExtent(tr2,crs(pl))))

#------------load planet satellite data------------#
# planet analytic image from 6 July 2019
pl <- stack("data/CYN_06_Jul_2019_PSScene4Band_Explorer/files/PSScene4Band/20190706_002918_101b/analytic_sr_udm2/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# crop planet to extent of drone flights
test <- crop(pl,x)

# reproject Planet image to ...
plr <- projectRaster(pl,crs = st, res = 3,
                     filename = "CYN_stereo_20190706_002918_101b_3B_AnalyticMS_SR.tif")


plr <- crop(plr,merge(extent(tr1),extent(tr2)),
            filename = "CYN_stereo_20190706_002918_101b_3B_AnalyticMS_SR.tif")

# reproject drone imagery
tr1a <- projectRaster(tr1,crs = aea, res = res(tr1),
                      ) 
tr1l <- projectRaster(tr1,crs = laea, res = res(tr1))

#------------load training/validation data------------#
# read in  data
lc <- readOGR("data/ground_truth/cyn_landover_pts.shp")

#------------load multispectral uas data------------#

# Kolym_Albers_Equal_Area_Conic_2
# Authority: Custom
# 
# Projection: Albers
# false_easting: 0.0
# false_northing: 0.0
# central_meridian: 150.0
# standard_parallel_1: 25.0
# standard_parallel_2: 75.0
# latitude_of_origin: 60.0
# Linear Unit: Meter (1.0)
crs(tr2) <- st 

