# Random Forest Model for comparison

# creation of a composite climatology (mosaic) for north america, from PRISM and daymet
# use ML to create a synthetic "prism-like" blend between PRISM and Daymet in the Yukon 
# Colin Mahony, edited by Tirion Grice
# Nov 2025

library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

studyarea <- ext(c(-150, -105, 48, 72))

# load in dem
dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/", sep="")
dem.full <- rast(paste(dir, "dem/dem_NWNA_coarse.nc", sep=""))
mask <- dem.full > 0
dem <- mask(dem.full, mask, maskvalues = FALSE)
plot(dem)
dem <- focal(dem, w=13, fun="min", na.policy="only") # add a buffer of minimum values around coastline to allow for a buffer of climate prediction
plot(dem)

# load in ocean proximity
coastal.full <- rast(paste(dir, "dem/op/ocean_proximity_train.nc", sep=""))
plot(coastal.full)
coastal.full <- project(coastal.full, dem)
coastal <- mask(coastal.full, dem)
plot(coastal)

monthcodes <- sprintf("%02d", 1:12)
elements <- c("tmax", "tmin", "prec")

# load in era5 data
era5_full <- list()
era5_vars <- c("tasmax", "tasmin", "prec")
dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/era5_clim/", sep="")
for (i in seq_along(era5_vars)) {
  file <- list.files(path = paste0(dir, era5_vars[i], "/27x27/", sep=""), pattern = ".*_cropped\\.nc$")
  era5 <- rast(file.path(dir, era5_vars[i], "27x27", file))
  names(era5) <- paste0(elements[i], "_", monthcodes)
  
  # mask out ocean in era5
  era5_aligned <- project(era5, dem)
  era5_masked <- mask(era5_aligned, dem)
  values(era5_masked)[!is.finite(values(era5_masked))] <- NA
  if (era5_vars[i]=="prec") values(era5_masked) <- log2(values(era5_masked))
  
  era5_full[[i]] <- era5_masked
}
names(era5_full) <- elements
era5_rast <- rast(era5_full)


# e <- 3
# m <- 2

for (e in 1:length(elements)){
  for (m in 1:length(monthcodes)){
    #browser()
    months <- tolower(month.abb)
    # load the PRISM mosaic training data
    file <- file.path("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM", elements[e], months[m], "prism_train_coarse.nc")
    prism <- rast(file)
    values(prism)[!is.finite(values(prism))] <- NA
    if (elements[e]=="prec") values(prism) <- log2(values(prism))
    
    # extract the training data
    points <- crds(prism, na.rm = T)
    predictand.points <- extract(prism, points)
    elev.points <- extract(dem, points)
    coastal.points <- extract(coastal, points)
    select <- c(1,4,7,10,13,16,19,22,25,28,31,34) # 1 month representing each season for the 3 variables
    if (!m %in% select) select <- c(select, c(1,13,25)+(m-1)) # add the current month to the variable set
    era5.points <- extract(era5_rast[[select]], points)
    data.train <- cbind(predictand.points, elev.points, coastal.points, era5.points)
    colnames(data.train)[1:3] <- c("predictand","elev", "coastal")
    data.train <- data.train[complete.cases(data.train),]
    
    # train the model
    model <- ranger(y=data.train[,1], x=data.train[,-1], num.trees=300, replace = F, sample.fraction = 0.25, oob.error=F)
    # model <- ranger(y=data.train[,1], x=data.train[,-1], oob.error=F)
    
    # predict across entire study area
    points.pred <- crds(dem, na.rm = F)
    elev.pred <- extract(dem, points.pred)
    coastal.pred <- extract(coastal, points.pred)
    era5.pred <- extract(era5_rast[[select]], points.pred)
    data.pred <- cbind(points.pred, elev.pred, coastal.pred, era5.pred)
    colnames(data.pred)[1:4] <- c("x","y","elev", "coastal")
    data.pred$id <- seq_along(data.pred$x)
    data.pred <- data.pred[complete.cases(data.pred),]
    
    # predict to the unmapped area
    pred.fillarea <- predict(model, data = data.pred)
    rm(model)
    
    # compile prediction
    clim.pred <- dem
    values(clim.pred) <- NA
    clim.pred[data.pred$id] <- pred.fillarea$predictions
    
    # write the raster
    writeRaster(clim.pred, paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/Random Forest/", elements[e], monthcodes[m], ".tif", sep=""), overwrite=T)
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

#---------------------------------
# View results

# rasters for the variable of interest

us <- list()
for (i in seq_along(months)) {
  prism <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/tmax/", months[i], "/prism_train_coarse.nc"))
  us[[i]] <- prism
}
cp <- list()
for (i in 1:12) {
  pred <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/Random Forest/training area/tmax", sprintf("%02d", i), ".tif"))
  cp[[i]] <- pred
}

# Color Scheme
combined <- c(unlist(lapply(us, values)), unlist(lapply(cp, values)))
# if(elements[e]=="Pr") combined <- log2(combined)
combined <- combined[is.finite(combined)]
inc=diff(range(combined))/500
breaks=seq(quantile(combined, 0.005)-inc, quantile(combined, 0.995)+inc, inc)
# ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColScheme <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")

# if(elements[e]=="Pr"){
#   values(us) <- log2(values(us))
#   values(cp) <- log2(values(cp))
#   values(wc) <- log2(values(wc))
#   values(dm) <- log2(values(dm))
# } 

# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
  addRasterImage(us[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jan PRISM") %>%
  addRasterImage(us[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Feb PRISM") %>%
  addRasterImage(us[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Mar PRISM") %>%
  addRasterImage(us[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Apr PRISM") %>%
  addRasterImage(us[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "May PRISM") %>%
  addRasterImage(us[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun PRISM") %>%
  addRasterImage(us[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jul PRISM") %>%
  addRasterImage(us[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Aug PRISM") %>%
  addRasterImage(us[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Sep PRISM") %>%
  addRasterImage(us[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct PRISM") %>%
  addRasterImage(us[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov PRISM") %>%
  addRasterImage(us[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec PRISM") %>%
  addRasterImage(cp[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jan Predicted") %>%
  addRasterImage(cp[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Feb Predicted") %>%
  addRasterImage(cp[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Mar Predicted") %>%
  addRasterImage(cp[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Apr Predicted") %>%
  addRasterImage(cp[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "May Predicted") %>%
  addRasterImage(cp[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun Predicted") %>%
  addRasterImage(cp[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jul Predicted") %>%
  addRasterImage(cp[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Aug Predicted") %>%
  addRasterImage(cp[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Sep Predicted") %>%
  addRasterImage(cp[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct Predicted") %>%
  addRasterImage(cp[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov Predicted") %>%
  addRasterImage(cp[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec Predicted") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("Jan PRISM", "Feb PRISM", "Mar PRISM", "Apr PRISM", "May PRISM", "Jun PRISM", "Jul PRISM", "Aug PRISM", "Sep PRISM", "Oct PRISM", "Nov PRISM", "Dec PRISM",
                      "Jan Predicted", "Feb Predicted", "Mar Predicted", "Apr Predicted", "May Predicted", "Jun Predicted", "Jul Predicted", "Aug Predicted", "Sep Predicted", "Oct Predicted", "Nov Predicted", "Dec Predicted"),
    options = layersControlOptions(collapsed = FALSE)
  )
map