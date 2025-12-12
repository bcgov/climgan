library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

pred_wrf <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/wrf/march_nonan_bc/GAN_tmax_bc_march_nonan_gen250.tif')
pred_wrfextra <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/wrf/march_nonan_extracov/GAN_tmax_extracov_march_gen250.tif')
pred_wrf_ns <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/wrf/north_south_train/GAN_tmax_march_nonan_wrf_gen250.tif')
pred_wrf_ak_bc <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/wrf/march_nonan_ak_bc_prism/GAN_tmax_march_ak_bc_prism_gen250.tif')
pred_wc <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/march_nonan_bc/GAN_tmax_march_nonan_worldclim_gen250.tif')
pred_wc_ns <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/north_south_train_replace_w_wrf/GAN_tmax_north_south_train_wc_gen250.tif')
# pred_wc_day <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/march_nonan_BC_worldclim_daymet/GAN_tmax_march_prism.tif')
pred_wc_full <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/full_domain/GAN_tmax_march_no_focal.tif')
pred_wc_full2 <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/full_domain/GAN_tmax_march_nonan.tif')
pred_wc_full3 <- rast('C:/Users/SBEALE/Desktop/Gan Predictions/tmax/worldclim/full_domain/GAN_tmax_march.tif')

# load the PRISM  data for the variable
prism <- rast('C:/Users/SBEALE/Desktop/full_domain/tmax_03_PRISM_Daymet.nc')
# 
wc <- rast("C:/Users/SBEALE/Desktop/full_domain/tmax_03_WorldClim_coarse.nc")
wc_foc <- rast("C:/Users/SBEALE/Desktop/full_domain/tmax_03_WorldClim_coarse_focal_max_w15.nc")
wc <- project(wc, prism)
wc_foc <- project(wc_foc, prism)

# 
# wrf <- rast('C:/Users/SBEALE/Desktop/Cropped_Coarsened_WRF_PRISM/no_overlap/tmin_03_WRF_coarse.nc')
# wrf <- project(wrf, prism)
# 
daymet <- rast('//objectstore2.nrs.bcgov/ffec/Climatologies/Daymet/daymet_1981_2010_tmax_03.tif')
daymet <- project(daymet, prism)

pred_wrf <- project(pred_wrf, prism)
pred_wrfextra <- project(pred_wrfextra, prism)
pred_wrf_ns <- project(pred_wrf_ns, prism)
pred_wrf_ak_bc <- project(pred_wrf_ak_bc, prism)
pred_wc <- project(pred_wc, prism)
pred_wc_ns <- project(pred_wc_ns, prism)
# pred_wc_day <- project(pred_wc_day, prism)
pred_wc_full <-project(pred_wc_full, prism)
pred_wc_full2 <-project(pred_wc_full2, prism)
pred_wc_full3 <-project(pred_wc_full3, prism)

# color scheme
combined <- c(values(prism), values(wc), values(pred_wrf), values(pred_wc))
# combined <- c(values(prism), values(wc), values(daymet))
combined <- combined[is.finite(combined)]
inc=diff(range(combined))/500
breaks=seq(quantile(combined, 0.005)-inc, quantile(combined, 0.995)+inc, inc)
ColScheme <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")


# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(prism, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "PRISM") %>%
  # addRasterImage(wrf, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024 , group = "WRF") %>%
  addRasterImage(wc, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "WorldClim") %>%
  addRasterImage(daymet, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "Daymet") %>%
  addRasterImage(pred_wrf, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WRF") %>%
  addRasterImage(pred_wrfextra, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WRF Extra Covariates") %>%
  addRasterImage(pred_wrf_ns, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WRF North + South Train") %>%
  addRasterImage(pred_wrf_ak_bc, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WRF Ak BC PRISM") %>%
  addRasterImage(pred_wc, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WorldClim") %>%
  addRasterImage(pred_wc_ns, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WorldClim North + South Train") %>%
  # addRasterImage(pred_wc_day, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WorldClim + Daymet") %>%
  addRasterImage(pred_wc_full, colors = ColPal.raster, opacity = 1, maxBytes = 10539505, group = "GAN WorldClim Full") %>%
  addLayersControl(  
    overlayGroups = c("PRISM", "WorldClim", "Daymet", "GAN WRF", "GAN WRF Extra Covariates", 
                      "GAN WRF North + South Train", "GAN WorldClim", 
                      "GAN WorldClim North + South Train", "GAN WorldClim Full"),
    options = layersControlOptions(collapsed = FALSE)
  )
map

map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(prism, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "PRISM") %>%
  addRasterImage(wc, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "WorldClim") %>%
  addRasterImage(wc_foc, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "WorldClim foc") %>%
  addRasterImage(pred_wc_full, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "GAN WorldClim no focal") %>%
  addRasterImage(pred_wc_full2, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "GAN WorldClim nan") %>%
  addRasterImage(pred_wc_full3, colors = ColPal.raster, opacity = 1, maxBytes = 10653329, group = "GAN WorldClim focal, nan=0") %>%
  addLayersControl(  
    overlayGroups = c("PRISM", "WorldClim", "WorldClim foc", "GAN WorldClim no focal", "GAN WorldClim nan", "GAN WorldClim focal, nan=0"),
    options = layersControlOptions(collapsed = FALSE)
  )
map