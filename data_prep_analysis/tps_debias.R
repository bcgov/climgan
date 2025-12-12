library(fields)
library(terra)
library(data.table)
library(reticulate)
library(RColorBrewer)
library(raster)
library(sf)
library(leaflet)

## read in GAN prediction
months <- tolower(month.abb)
gans <- list()

for (i in seq_along(months)) {
  res <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/spec1/gen50/", months[i], "_merged_masked.tif"))
  gans[[i]] <- res
}

par(mfrow = c(3, 4))

for (i in seq_along(months)) {
  plot(gans[[i]], main = paste(months[i], "GAN50"))
}

## read in and prep station data
var <- "ppt"
stns_data <- list()
dem <- rast(paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/test area prep/dem_stn_train.tif"))

par(mfrow = c(3, 4))

for (i in seq_along(months)) {
  # load the source STATION data for the BC prism
  dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/"
  stn.info <- fread(paste(dir, var,"/pr_uscdn_8110.csv", sep="")) #read in
  for (j in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[j])==c(-9999), (j):=NA, ] # replace -9999 with NA
  stn.info <- stn.info[-which(Elevation==-9999),]
  stn.info <- stn.info[-which(El_Flag=="@"),]
  stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]
  
  res <- gans[[i]]
  stn.info.train <- stn.info[Lat>ext(res)[3] & Lat<ext(res)[4] & Long > ext(res)[1] & Long < ext(res)[2]]
  
  if (var != "ppt") {
    stn.info.train[, (month.abb) := lapply(.SD, function(x) x/10), .SDcols = month.abb] # divide by 10
  }
  
  # remove any stations that have incorrect elevations outside threshold
  stns <- vect(as.data.frame(stn.info.train), geom = c("Long", "Lat"), crs = crs(dem))
  
  stn.info.train$dem_elev <- terra::extract(dem, stns)[,2]
  stn.info.train$elev_diff <- (stn.info.train$Elevation - stn.info.train$dem_elev)
  
  # remove any stations that are outside of 3 standard deviations from the centre of the dist
  mu  <- mean(stn.info.train$elev_diff, na.rm = TRUE)
  std_dev <- sd(stn.info.train$elev_diff,  na.rm = TRUE)
  lower <- mu - 3*std_dev
  upper <- mu + 3*std_dev
  stn.info.train <- stn.info.train[elev_diff >= lower & elev_diff <= upper]
  
  # extract GAN generated map values at station locations and calculate bias
  month <- stringr::str_to_title(months[i])
  pts <- vect(as.data.frame(stn.info.train), geom = c("Long", "Lat"), crs = crs(res))
  stn.info.train$raster_vals <- terra::extract(res, pts)[,2]
  stn.info.train$bias <- stn.info.train[[month]] - stn.info.train$raster_vals
  
  # add pseudostations
  # create box that has the same extent as the GAN generated map, change CRS to Albers Equal Area, and make the box into a grid with cells that are 100km wide (the centers of these cells will be the pseudostations), change parameters grid_width & buffer
  grid_width <- 100000 # 100km in m
  buffer <- 50000 # 50km in m
  bbox <- as.polygons(ext(res), crs=crs(res))
  bbox_sf <- st_as_sf(bbox)
  bbox_aea_sf <- st_transform(bbox_sf, "ESRI:102008")
  grid <- st_make_grid(bbox_aea_sf, cellsize = grid_width, what = "centers")
  
  # project the real stations onto the same grid as pseudostations and remove any pseudostations that are within 100km from a real station, then project the pseudostations back onto lat/lon crs
  grid_vect <- vect(grid)
  pts_aea <- project(pts, grid_vect)
  d <- distance(grid_vect, pts_aea)
  keep_idx <- apply(d, 1, function(x) all(x > buffer))
  grid_vect_keep <- grid_vect[keep_idx, ]
  grid_keep_ll <- project(grid_vect_keep, bbox)
  
  # mask out stations in the ocean
  vals <- extract(res, grid_keep_ll)
  ps <- grid_keep_ll[!is.na(vals[,2]), ]
  
  # build dataframe to bind with stn.info.train 
  ps_df <- data.frame(
    St_ID = NA,
    St_Flag = NA,
    Name = NA,
    Elevation = NA,
    El_Flag = NA,
    Long = crds(ps)[,1],
    Lat = crds(ps)[,2],
    Jan = 0, Feb = 0, Mar = 0, Apr = 0, May = 0, Jun = 0,
    Jul = 0, Aug = 0, Sep = 0, Oct = 0, Nov = 0, Dec = 0,
    Annual = 0,
    dem_elev = 0,
    elev_diff = 0,
    raster_vals = 0,
    bias = 0
  )
  
  stn.info.train <- rbind(stn.info.train, ps_df)
  
  # make all stations in training area = 0
  prism <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/", months[i], "/prism_train_coarse.nc"))
  all_stns <- vect(as.data.frame(stn.info.train), geom = c("Long", "Lat"), crs = crs(res))
  training <- extract(prism, all_stns)
  inside_training <- !is.na(training[,2])
  stn.info.train$bias[inside_training] <- 0
  
  # sanity check
  plot(is.na(res),  main = paste(months[i], "Stations"))
  all <- vect(as.data.frame(stn.info.train), geom = c("Long", "Lat"), crs = crs(res))
  zero_bias_pts <- all[all$bias == 0, ]
  nonzero_bias_pts <- all[all$bias != 0, ]
  plot(zero_bias_pts, col="red", pch=20, cex = 0.7, add=TRUE)
  plot(nonzero_bias_pts, col="green", pch=20, cex = 0.7, add=TRUE)
  
  stns_data[[i]] <- stn.info.train
}

## fit Thin Plate Spline model and predict
for (i in seq_along(months)) {
  res <- gans[[i]]
  stn.info.train <- stns_data[[i]]
  
  coords <- as.matrix(stn.info.train[, c("Long", "Lat")])
  bias   <- stn.info.train$bias
  
  tps_model <- Tps(coords, bias, lambda = NULL)
  
  grid_coords <- crds(res, df = TRUE, na.rm=FALSE)
  pred_bias <- predict(tps_model, grid_coords)
  
  bias_raster <- rast(res)
  values(bias_raster) <- pred_bias
  
  corrected <- res + bias_raster
  
  # save spline surface and debiased map
  writeCDF(bias_raster, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/spec1/gen50/spline.nc"), varname='prec', overwrite = T)
  
  writeCDF(corrected, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/spec1/gen50/", months[i], "_debias.nc"), varname='prec', overwrite = T)
}

## plot surfaces
prism_list <- list()
fm_list <- list()
spec_list <- list()
debias_list <- list()

for (i in seq_along(months)) {
  prism <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/", months[i], "/prism_train_coarse.nc"))
  prism.log <- log1p(prism)
  fm <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/" , months[i], "_fullregion_masked.nc"))
  fm.log <- log1p(fm)
  spec <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/spec1/gen50/" , months[i], "_merged_masked.tif"))
  spec.log <- log1p(spec)
  debias <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/", months[i], "/spec1/gen50/" , months[i], "_debias.nc"))
  debias.log <- log1p(debias)
  
  prism.log <- project(prism.log, debias.log)
  fm.log <- project(fm.log, debias.log)
  spec.log <- project(spec.log, debias.log)
  
  prism_list[[i]] <- prism.log
  fm_list[[i]] <- fm.log
  spec_list[[i]] <- spec.log
  debias_list[[i]] <- debias.log
}

dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/"
stn.info <- fread(paste(dir, var,"/pr_uscdn_8110.csv", sep="")) #read in
for (j in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[j])==c(-9999), (j):=NA, ] # replace -9999 with NA
stn.info <- stn.info[-which(Elevation==-9999),]
stn.info <- stn.info[-which(El_Flag=="@"),]
stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]
stn.info.train <- stn.info[Lat>ext(debias_list[[1]])[3] & Lat<ext(debias_list[[1]])[4] & Long > ext(debias_list[[1]])[1] & Long < ext(debias_list[[1]])[2]]

var <- "ppt"
if (var != "ppt") {
  stn.info.train[, (month.abb) := lapply(.SD, function(x) x/10), .SDcols = month.abb] # divide by 10
} else {
  stn.info.train[, (month.abb) := lapply(.SD, function(x) log1p(x)), .SDcols = month.abb]
}

stn.data <- stn.info.train[,get("Jul")]
stn.data <- stn.data[is.finite(stn.data)]

all_values <- c(values(prism_list[[7]]),
                values(fm_list[[7]]),
                values(spec_list[[7]]),
                values(debias_list[[7]])
)

all_values <- all_values[is.finite(all_values)]
inc=diff(range(all_values))/500
breaks=seq(quantile(all_values, 0.00025)-inc, quantile(all_values, 0.99975)+inc, inc)
ColScheme <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")


# leaflet map
labels <- paste(stn.info.train$Name, "(El. ", stn.info.train$Elevation, "m)", sep="")
map <- leaflet(stn.info.train) %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(prism_list[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "Jul PRISM") %>%
  addRasterImage(fm_list[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "Jul FM4") %>%
  addRasterImage(spec_list[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "Jul GAN50") %>%
  addRasterImage(debias_list[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "Jul GAN50 debias") %>%
  addCircleMarkers(lng = ~Long, lat = ~Lat, color="black", fillColor = ~ ColPal(stn.data), opacity = 1, fillOpacity = 1, popup = labels, radius=6, weight=2, group = "Stations") %>%
  addLayersControl(
    overlayGroups = c("Jul PRISM",
                      "Jul FM4",
                      "Jul GAN50",
                      "Jul GAN50 debias",
                      "Stations"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )
map
