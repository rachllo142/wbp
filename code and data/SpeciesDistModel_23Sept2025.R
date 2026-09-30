
# Whitebark Pine SDM with PRISM + Downscaling


##### 1. Load packages #####
library(prism)     # for downloading PRISM data
library(raster)    # raster processing
library(sp)        # spatial points
library(dismo)     # Maxent SDM
library(rgdal)     # for reading/writing spatial data


# 2. Set paths

options(prism.path = "PRISM_data")           # folder to store PRISM data
dem_path <- "DEM_30m.tif"                   # path to high-res DEM
occ_path <- "wbp_occurrences.csv"           # occurrence CSV


# 3. Download PRISM 30-year normals

# Example: precipitation and mean temperature (monthly)
get_prism_normals(type = "ppt", resolution = "800m", mon = 1:12)
get_prism_normals(type = "tmean", resolution = "800m", mon = 1:12)

# Stack PRISM layers
ppt_stack <- prism_stack("ppt")
tmean_stack <- prism_stack("tmean")

# Optional: compute annual mean / total
ppt_annual <- calc(ppt_stack, sum)      # total annual ppt
tmean_annual <- calc(tmean_stack, mean) # mean annual temperature


# 4. Load DEM and resample PRISM

dem <- raster(dem_path)

# Crop PRISM layers to DEM extent
ppt_crop <- crop(ppt_annual, dem)
tmean_crop <- crop(tmean_annual, dem)

# Resample PRISM layers to DEM resolution (bilinear)
ppt_highres <- resample(ppt_crop, dem, method="bilinear")
tmean_highres <- resample(tmean_crop, dem, method="bilinear")


# 5. Generate topographic layers

slope <- terrain(dem, opt="slope", unit="degrees")
aspect <- terrain(dem, opt="aspect", unit="degrees")
hill <- hillShade(slope, aspect, angle=45, direction=315)


# 6. Stack all environmental layers

env_stack <- stack(ppt_highres, tmean_highres, slope, aspect, hill)
names(env_stack) <- c("ppt", "tmean", "slope", "aspect", "hillshade")


# 7. Load occurrence points

wbp_occ <- read.csv(occ_path)    # columns: lon, lat
coordinates(wbp_occ) <- ~lon+lat
crs(wbp_occ) <- crs(dem)


# 8. Run Maxent model

# Make sure maxent.jar is installed and detected
maxent_model <- maxent(x=env_stack, p=wbp_occ)


# 9. Predict habitat suitability

pred_suit <- predict(maxent_model, env_stack)

# Plot suitability map
plot(pred_suit, main="Whitebark Pine Habitat Suitability")
points(wbp_occ, pch=20, col='red')


# 10. Model evaluation (optional)
# Split data: 70% training, 30% testing
set.seed(123)
train_idx <- sample(1:nrow(wbp_occ), 0.7*nrow(wbp_occ))
train <- wbp_occ[train_idx, ]
test <- wbp_occ[-train_idx, ]

model_eval <- maxent(env_stack, p=train)
eval_res <- evaluate(model_eval, p=test, a=randomPoints(env_stack, 1000))
print(paste("AUC =", eval_res@auc))
