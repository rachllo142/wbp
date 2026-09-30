

library(terra)
library(gbm)
library(maxnet)
library(randomForest)
library(earth)

setwd("C:/Users/acush/OneDrive/Desktop/GILASDM")

occ <- readRDS("occ_values_clean.rds")

aspect_raster <- rast("aligned_aspect.tif")
dem_raster    <- rast("aligned_dem.tif")
slope_raster  <- rast("aligned_slope.tif")
precip_raster <- rast("PRISM_precip_aligned.tif")
tmax_raster   <- rast("PRISM_tmax_aligned.tif")
tmean_raster  <- rast("PRISM_tmean_aligned.tif")
tmin_raster   <- rast("PRISM_tmin_aligned.tif")

predictor_stack <- c(aspect_raster, dem_raster, slope_raster, 
                     precip_raster, tmax_raster, tmean_raster, tmin_raster)

set.seed(1234)
bg_points <- spatSample(predictor_stack, size=872, method="random", na.rm=TRUE)

gbm_raster     <- rast("gbm_pred.tif")
rf_raster      <- rast("rf_pred.tif")
mars_raster    <- rast("mars_pred.tif")
maxnet_raster  <- rast("maxnet_pred.tif")

par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(gbm_raster, main="GBM Suitability")
plot(rf_raster, main="RF Suitability")
plot(mars_raster, main="MARS Suitability")
plot(maxnet_raster, main="MaxNet Suitability")

ensemble_raster <- (gbm_raster + rf_raster + mars_raster + maxnet_raster) / 4

par(mfrow=c(1,1), mar=c(4,4,3,1))
plot(ensemble_raster, main="Ensemble Suitability")
