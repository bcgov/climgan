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
library(htmlwidgets)

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
  #if (era5_vars[i]=="prec") values(era5_masked) <- log2(values(era5_masked))
  
  era5_full[[i]] <- era5_masked
}
names(era5_full) <- elements
era5_rast <- rast(era5_full)


# e <- 3
# m <- 2

# load in data for predicting area
dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/", sep="")
dem.full <- rast(paste(dir, "dem/dem_full.nc", sep=""))
mask <- dem.full > 0
dem.pred <- mask(dem.full, mask, maskvalues = FALSE)
plot(dem.pred)
dem.pred <- focal(dem.pred, w=13, fun="min", na.policy="only") # add a buffer of minimum values around coastline to allow for a buffer of climate prediction
plot(dem.pred)

coastal.full <- rast(paste(dir, "dem/op/ocean_proximity_full.nc", sep=""))
plot(coastal.full)
coastal.full <- project(coastal.full, dem.pred)
coastal.pred <- mask(coastal.full, dem.pred)
plot(coastal.pred)

era5_pred <- list()
era5_vars <- c("tasmax", "tasmin", "prec")
dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/era5_clim/", sep="")
for (i in seq_along(era5_vars)) {
  file <- list.files(path = paste0(dir, era5_vars[i], "/27x27/", sep=""), pattern = ".*_full\\.nc$")
  era5 <- rast(file.path(dir, era5_vars[i], "27x27", file))
  names(era5) <- paste0(elements[i], "_", monthcodes)
  
  # mask out ocean in era5
  era5_aligned <- project(era5, dem)
  era5_masked <- mask(era5_aligned, dem)
  values(era5_masked)[!is.finite(values(era5_masked))] <- NA
  if (era5_vars[i]=="prec") values(era5_masked) <- log2(values(era5_masked))
  
  era5_pred[[i]] <- era5_masked
}
names(era5_pred) <- elements
era5_rast <- rast(era5_pred)

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
    writeRaster(clim.pred, paste("C:/Users/tgrice/OneDrive - Government of BC/Documents/GANs/Tirion/Results/Random Forest/", elements[e], monthcodes[m], ".tif", sep=""), overwrite=T)
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

#---------------------------------
# View results

# rasters for the variable of interest
months <- tolower(month.abb)
us <- list()
for (i in seq_along(months)) {
  prism <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/tmin/", months[i], "/prism_train_coarse.nc"))
  #prism.log <- log1p(prism)
  us[[i]] <- prism
}
# cp <- list()
# for (i in 1:12) {
#   pred <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/Random Forest/prec", sprintf("%02d", i), ".tif"))
#   cp[[i]] <- pred
# }
gan <- list()
gens <- c(10,20,30,40,50)
for (i in seq_along(gens)) {
  pred <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/tmin/Model4/dec/spec1/", "gen", gens[i], "/dec_fullregion_masked.nc"))
  gan[[i]] <- pred
}
gan2 <- list()
for (i in seq_along(months)) {
  pred <- rast(paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/tmin/Model4/", months[i], "/", months[i], "_fullregion_masked.nc"))
  gan2[[i]] <- pred
}

for (i in 1:12) {
  us[[i]] <- project(us[[i]], gan[[1]])
  gan2[[i]] <- project(gan2[[i]], gan[[1]])
}

# Color Scheme
combined <- c(unlist(lapply(us[[12]], values)), unlist(lapply(gan2[[12]], values)), unlist(lapply(gan, values)))
# if(elements[e]=="Pr") combined <- log2(combined)
combined <- combined[is.finite(combined)]
inc=diff(range(combined))/500
breaks=seq(quantile(combined, 0.00025)-inc, quantile(combined, 0.99975)+inc, inc)
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
# 
# stn.info <- fread("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/ppt/pr_uscdn_8110.csv")
# stn.info <- stn.info[-which(El_Flag=="@"),]
# stn.data <- stn.info[,get(month.abb[12])]
# stn.data <- log1p(stn.data)
# stn.info <- stn.info[is.finite(stn.data),]
# stn.data <- stn.data[is.finite(stn.data)]
# 
# labels <- paste(stn.info$Name, "(El. ", stn.info$Elevation, "m)", sep="")
# 
# e <- ext(-150, -104.8958, 42.0625, 48)
# bbox_poly <- as.polygons(e)

# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  # addPolygons(
  #   data = bbox_poly,
  #   color = "black",
  #   weight = 2,
  #   fill = FALSE,
  #   group = "US Test area"
  # ) %>%
  # addRasterImage(us[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jan PRISM") %>%
  # addRasterImage(us[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Feb PRISM") %>%
  # addRasterImage(us[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Mar PRISM") %>%
  # addRasterImage(us[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Apr PRISM") %>%
  # addRasterImage(us[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "May PRISM") %>%
  # addRasterImage(us[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun PRISM") %>%
  # addRasterImage(us[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jul PRISM") %>%
  # addRasterImage(us[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Aug PRISM") %>%
  # addRasterImage(us[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Sep PRISM") %>%
  # addRasterImage(us[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct PRISM") %>%
  # addRasterImage(us[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov PRISM") %>%
  addRasterImage(us[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec PRISM") %>%
  # addRasterImage(cp[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jan RF") %>%
  # addRasterImage(cp[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Feb RF") %>%
  # addRasterImage(cp[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Mar RF") %>%
  # addRasterImage(cp[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Apr RF") %>%
  # addRasterImage(cp[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "May RF") %>%
  # addRasterImage(cp[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun RF") %>%
  # addRasterImage(cp[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jul RF") %>%
  # addRasterImage(cp[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Aug RF") %>%
  # addRasterImage(cp[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Sep RF") %>%
  # addRasterImage(cp[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct RF") %>%
  # addRasterImage(cp[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov RF") %>%
  # addRasterImage(cp[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec RF") %>%
  # addRasterImage(gan2[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jan GAN FM4") %>%
  # addRasterImage(gan2[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Feb GAN FM4") %>%
  # addRasterImage(gan2[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Mar GAN FM4") %>%
  # addRasterImage(gan2[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Apr GAN FM4") %>%
  # addRasterImage(gan2[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "May GAN FM4") %>%
  # addRasterImage(gan2[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun GAN FM4") %>%
  # addRasterImage(gan2[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jul GAN FM4") %>%
  # addRasterImage(gan2[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Aug GAN FM4") %>%
  # addRasterImage(gan2[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Sep GAN FM4") %>%
  # addRasterImage(gan2[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct GAN FM4") %>%
  # addRasterImage(gan2[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov GAN FM4") %>%
  addRasterImage(gan2[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN FM4") %>%
  addRasterImage(gan[[1]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN10") %>%
  addRasterImage(gan[[2]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN20") %>%
  addRasterImage(gan[[3]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN30") %>%
  addRasterImage(gan[[4]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN40") %>%
  addRasterImage(gan[[5]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN50") %>%
  # addRasterImage(gan[[6]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun GAN100") %>%
  # addRasterImage(gan[[7]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun GAN150") %>%
  # addRasterImage(gan[[8]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun GAN200") %>%
  # addRasterImage(gan[[9]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Jun GAN250") %>%
  # addRasterImage(gan[[10]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Oct GAN FM4") %>%
  # addRasterImage(gan[[11]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Nov GAN FM4") %>%
  # addRasterImage(gan[[12]], colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Dec GAN FM4") %>%
  # addCircleMarkers(lng = ~Long, lat = ~Lat, color="black", fillColor = ~ ColPal(stn.data), opacity = 1, fillOpacity = 1, popup = labels, radius=6, weight=2, group = "Stations") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("Dec PRISM", 
                      # "Feb PRISM", "Mar PRISM", "Apr PRISM", "May PRISM", "Jun PRISM","Jul PRISM", "Aug PRISM", "Sep PRISM", "Oct PRISM", "Nov PRISM", "Dec PRISM",
                      # "Jan RF", "Feb RF", "Mar RF", "Apr RF", "May RF", "Jun RF", "Jul RF", "Aug RF", "Sep RF", "Oct RF", "Nov RF", "Dec RF",
                      # "Jan GAN FM4", "Feb GAN FM4", "Mar GAN FM4", "Apr GAN FM4", "May GAN FM4", "Jun GAN FM4", "Jul GAN FM4", "Aug GAN FM4", "Sep GAN FM4", "Oct GAN FM4", "Nov GAN FM4", "Dec GAN FM4"),
                      "Dec GAN FM4", "Dec GAN10", "Dec GAN20", "Dec GAN30", "Dec GAN40", "Dec GAN50"), 
                      # "US Test area", "Stations"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  htmlwidgets::onRender("
    function(el, x) {
      var map = this;

      // Create the button
      var button = L.control({position: 'topright'});
      button.onAdd = function(map) {
        var div = L.DomUtil.create('div', 'leaflet-bar');
        var btn = L.DomUtil.create('a', '', div);
        btn.innerHTML = 'Clear';
        btn.style.backgroundColor = 'white';
        btn.style.cursor = 'pointer';
        btn.style.padding = '5px';
        btn.onclick = function(){
          map.eachLayer(function(layer){
            if(layer.options && layer.options.pane === 'overlayPane'){
              map.removeLayer(layer);
            }
          });
        };
        return div;
      };
      button.addTo(map);
    }
  ")
map

# ---- map for comparing different GANs within a month

# # load Susan's maps
# sbeale_jun <- rast("O:/Mosaic_Yukon/operational/WorldClim/prec/jun/Predictions/GAN_gen150.nc")
# sbeale_jun <- log1p(sbeale_jun)
# # sbeale_feb <- sbeale_feb + 273.15 #convert to Kelvin
# sbeale_jun <- aggregate(sbeale_jun, fact = 3) # coarsen to match my pred resolution

# load the PRISM  data for the variable and unstandardize
prism_jun <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/jun/prism_train_coarse.nc")
prism_jun <- log1p(prism_jun)

# load predicted data
pred_jun_fm4 <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/jun/jun_fullregion_masked.nc")
pred_jun_fm4_spec <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/jun/spec1/gen50/jun_fullregion_masked.nc")
pred_jun_fm4_debias <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/prec/Model4/jun/spec1/gen50/jun_gen50_debiased.tif")

prism_jun <- project(prism_jun, pred_jun_fm4_spec)
pred_jun_fm4 <- project(pred_jun_fm4, pred_jun_fm4_spec)
pred_jun_fm4_debias <- project(pred_jun_fm4_debias, pred_jun_fm4_spec)

stn.info <- fread("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/ppt/pr_uscdn_8110.csv")
stn.info <- stn.info[-which(El_Flag=="@"),]
stn.data <- stn.info[,get(month.abb[6])]
stn.data <- log1p(stn.data)
stn.info <- stn.info[is.finite(stn.data),]
stn.data <- stn.data[is.finite(stn.data)]


all_values <- c(values(prism_jun),
                values(pred_jun_fm4),
                values(pred_jun_fm4_spec),
                values(pred_jun_fm4_debias)
)

all_values <- all_values[is.finite(all_values)]
inc=diff(range(all_values))/500
breaks=seq(quantile(all_values, 0.00025)-inc, quantile(all_values, 0.99975)+inc, inc)
ColScheme <- colorRampPalette(brewer.pal(9, "YlGnBu"))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")

# vals <- values(sd)
# vals <- vals[!is.na(vals)]
# ColScheme_sd <- c("white", "black")
# ColPal_sd <- colorNumeric(ColScheme_sd, domain = vals, na.color = "white")
# ColPalsd.raster <- colorNumeric(ColScheme_sd, domain = vals, na.color = "transparent")

# leaflet map
labels <- paste(stn.info$Name, "(El. ", stn.info$Elevation, "m)", sep="")
map <- leaflet(stn.info) %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(prism_jun, colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "JUN PRISM") %>%
  addRasterImage(pred_jun_fm4, colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "JUN GAN FM4") %>%
  addRasterImage(pred_jun_fm4_spec, colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "JUN GAN50") %>%
  addRasterImage(pred_jun_fm4_debias, colors = ColPal.raster, opacity = 1, maxBytes = 12890669, group = "JUN GAN50 DEBIAS") %>%
  addCircleMarkers(lng = ~Long, lat = ~Lat, color="black", fillColor = ~ ColPal(stn.data), opacity = 1, fillOpacity = 1, popup = labels, radius=6, weight=2, group = "Stations") %>%
  addLayersControl(
    overlayGroups = c("JUN PRISM",
                      "JUN GAN FM4",
                      "JUN GAN50",
                      "JUN GAN50 DEBIAS",
                      "Stations"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )
map