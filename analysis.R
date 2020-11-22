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

##
# read parrot ms data
tr1m <- stack("data/processed/RU_CYN_TR1_FL016M_allbands_rgb_mask.tif")

# set band names for ms image
names(tr1m) <- c("green", "red", "rededge", "nir", "ndvi", "ndwi")

# read 3m uas NDVI
tr1ms <- raster(filename = "data/processed/CYN_TR1_ms_ndvi_3m.tif")
##
# read planet data
pl <- stack("data/processed/CYN_TR1_rgb_mask_20190706_002918_101b_3B_AnalyticMS_SR.tif")

names(pl) <- c("blue", "green", "red", "nir", "ndvi", "ndwi")

## read lc maps
lcr <- raster("data/processed/CYN_tr1_rgb_rf_lc.tif")
lcm <- raster("data/processed/CYN_tr1_ms_rf_lc.tif")
lcpl <- raster("data/processed/CYN_tr1_planet_rf_lc.tif")
lcplr <- raster("data/processed/CYN_tr1_planet_rf_lc_rgb.tif")
lcplm <- raster("data/processed/CYN_tr1_planet_rf_lc_ms.tif")

# aggregate multispectral lc map to 3m resolution using modal function
lcp <- aggregate(lcm,fact = 47, fun=modal)
lcp <- resample(lcp,pl,method = "ngb",
              filename = "data/processed/CYN_tr1_ms_rf_lc_3m.tif")

# aggregate rgb lc map to 3m resolution using modal function
lcp2 <- aggregate(lcr,fact = 183, fun=modal)
lcp2 <- projectRaster(lcp2,pl,method = "ngb",
                filename = "data/processed/CYN_tr1_rgb_rf_lc_3m.tif")

## calculate cover class area
a <- ifelse(length(res(lcr))==1,
            res(lcr)^2,
            res(lcr)[1]*res(lcr)[2])
ar <- c(length(which(getValues(lcr) == 1))*a,
        length(which(getValues(lcr) == 2))*a,
        length(which(getValues(lcr) == 3))*a)

a <- ifelse(length(res(lcm))==1,
            res(lcm)^2,
            res(lcm)[1]*res(lcm)[2])
ar <- cbind(ar,c(length(which(getValues(lcm) == 1))*a,
                  length(which(getValues(lcm) == 2))*a,
                  length(which(getValues(lcm) == 3))*a))

a <- ifelse(length(res(lcp))==1,
            res(lcp)^2,
            res(lcp)[1]*res(lcp)[2])
ar <- cbind(ar,c(length(which(getValues(lcp) == 1))*a,
                 length(which(getValues(lcp) == 2))*a,
                 length(which(getValues(lcp) == 3))*a))

a <- ifelse(length(res(lcp2))==1,
            res(lcp2)^2,
            res(lcp2)[1]*res(lcp2)[2])
ar <- cbind(ar,c(length(which(getValues(lcp2) == 1))*a,
                 length(which(getValues(lcp2) == 2))*a,
                 length(which(getValues(lcp2) == 3))*a))

a <- ifelse(length(res(lcpl))==1,
            res(lcpl)^2,
            res(lcpl)[1]*res(lcpl)[2])
ar <- cbind(ar,c(length(which(getValues(lcpl) == 1))*a,
                 length(which(getValues(lcpl) == 2))*a,
                 length(which(getValues(lcpl) == 3))*a))

a <- ifelse(length(res(lcplr))==1,
            res(lcplr)^2,
            res(lcplr)[1]*res(lcplr)[2])
ar <- cbind(ar,c(length(which(getValues(lcplr) == 1))*a,
                 length(which(getValues(lcplr) == 2))*a,
                 length(which(getValues(lcplr) == 3))*a))

a <- ifelse(length(res(lcplm))==1,
            res(lcplm)^2,
            res(lcplm)[1]*res(lcplm)[2])
ar <- cbind(ar,c(length(which(getValues(lcplm) == 1))*a,
                 length(which(getValues(lcplm) == 2))*a,
                 length(which(getValues(lcplm) == 3))*a))


colnames(ar) <- c("area.r", "area.ms", "area.ms3m", "area.rgb3m", "area.pl", "area.plrgb", "area.plms")
row.names(ar) <- c("ground", "tall", "water")

write.csv(ar, file = "lc_area_orig_res.csv",
          row.names = T)

## boxplot of NDVI for UAS and planet images
n.dat <- data.frame(ndvi = c(getValues(tr1m$ndvi),getValues(tr1ms),getValues(pl$ndvi)),
                     source = c(rep("UAS", ncell(tr1m$ndvi)),rep("UAS 3m", ncell(tr1ms)),rep("Planet", ncell(pl$ndvi))))


png(filename = "figures/ndvi_boxplot.png",
    width = 8, height = 6, units = "in", res = 300)
par(cex = 3,cex.axis = 2, cex.lab = 2, cex.sub = 2,
    mfcol = c(1,1),
    mar = c(4,5,4,4))
boxplot(ndvi~source, data = n.dat, 
        xlab="", ylab = "NDVI", 
        ylim = c(0.1, 0.9))
dev.off()


## calculate NDVI by class
zonal(tr1m[[5]],lcm)

clr <- c("#D7D79E", "#5C8944", "#73B2FF")
# ndvi boxplots
yl <- c(0,0.9)
ex <- 1.2

par(mfrow = c(2,1),mar = c(0,4,4,2),cex = ex)
boxplot(pl[[5]],lcp,
        ylim = yl, ann = F, yaxis = F,
        col=clr, horizontal= T, add = T)

par(mar = c(5,4,0,2),cex = ex)
boxplot(tr1m[[5]],lcm,
  #      ylim = yl,
        xlim = c(-0.5,3.5),
        at = c(0.25, 1.25, 2.25), 
      #  width = 0.5,
        col=clr, horizontal= T,
        ylab = "NDVI")


#### dumpster
## create ndvi histograms for planet and parrot
hist(getValues(tr1m[[5]]),freq = F,
     xlab = "NDVI", add = T)

hist(getValues(pl[[5]]),freq = F,
     xlab = "NDVI", xlim = c(0.1,0.9))

