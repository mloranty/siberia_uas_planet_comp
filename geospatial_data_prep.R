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

# set the NA value 
NAvalue(tr1) <- 255

# set coordinate system
crs(tr1) <- st 

# # Cherskiy North Transect 2
# tr2 <- stack("data/uas/CYN_TR2_FL017.tif")
# # set coordinate system
# crs(tr2) <- st 

tr1a <- projectRaster(tr1,crs = aea, res = 0.02,method = "bilinear",
                      filename = "data/uas/CYN_TR1_FL016_aea.tif")

# create bounding mask
m <- reclassify(tr1[[1]],c(0,254,1),
                filename = "data/uas/CYN_TR1_mask.tif")

# reproject drone imagery (this takes a long time!)
tr1a <- projectRaster(tr1,crs = aea, res = res(tr1),
                      ) 
#tr1l <- projectRaster(tr1,crs = laea, res = res(tr1))

#------------load multispectral uas data------------#
##
# read multispectral data
tr1m <- stack(c("data/uas/updated/RU_CYN_TR1_FL016M_index_green.tif",
                "data/uas/updated/RU_CYN_TR1_FL016M_index_red.tif",
                "data/uas/updated/RU_CYN_TR1_FL016M_index_red_edge.tif",
                "data/uas/updated/RU_CYN_TR1_FL016M_index_nir.tif",
                "data/uas/updated/RU_CYN_TR1_FL016M_index_ndvi.tif"))


# calculate ndwi after McFeeters et al 
tr1m[[6]] <- (tr1m[[1]]-tr1m[[4]])/(tr1m[[1]]+tr1m[[4]])

# # set band names for ms image
names(tr1m) <- c("green", "red", "rededge", "nir", "ndvi", "ndwi")

# reproject to mask MS data
m2 <- projectRaster(m,tr1m[[1]], method = "ngb",
                    filname = "data/processed/CYN_TR1_UTM_mask.tif" )

tr1m <- mask(tr1m,m2, 
             filename = "data/processed/RU_CYN_TR1_FL016M_allbands_rgb_mask.tif",
             overwrite = T)


#------------load planet satellite data------------#
##
# planet analytic image from 6 July 2019
pl <- stack("data/CYN_06_Jul_2019_PSScene4Band_Explorer/files/PSScene4Band/20190706_002918_101b/analytic_sr_udm2/20190706_002918_101b_3B_AnalyticMS_SR.tif")

# create from extent object to clip Planet image
# x <- merge(extent(projectExtent(tr1,crs(pl))),
#            extent(projectExtent(tr2,crs(pl))))

# crop planet to extent of RGBdrone flights
tr1pl <- crop(pl,projectExtent(tr1,crs(pl)))

# resample to mask planet data
m3 <- resample(m2,tr1pl[[1]], method = "ngb",
                    filname = "data/processed/CYN_TR1_UTM_mask.tif" )

# mask planet data to match extent of UAS data
tr1pl <- mask(tr1pl, m3,
              filename = "data/processed/CYN_TR1_rgb_mask_20190706_002918_101b_3B_AnalyticMS_SR.tif")

#calculate ndvi
tr1pl$ndvi <- (tr1pl$nir-tr1pl$red)/(tr1pl$nir+tr1pl$red)

#calculate ndwi
tr1pl$ndwi <- (tr1pl$green-tr1pl$nir)/(tr1pl$green+tr1pl$nir)

#write output to file
writeRaster(tr1pl, filename = "data/processed/CYN_TR1_rgb_mask_20190706_002918_101b_3B_AnalyticMS_SR.tif",
            overwrite = T)

#------------load training/validation data------------#
# read in  data
lc <- readOGR("data/ground_truth/cyn_landover_pts.shp")




# Kolyma_Albers_Equal_Area_Conic_2
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

