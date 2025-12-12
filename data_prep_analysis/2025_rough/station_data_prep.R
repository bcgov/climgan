# putting station data into a map to be tiled

library(terra)
library(data.table)
library(dplyr)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

var <- "ppt"

# load the source STATION data for the BC prism
dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/"
stn.info <- fread(paste(dir, var,"/pr_uscdn_8110.csv", sep="")) #read in
for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
stn.info <- stn.info[-which(El_Flag=="@"),]
stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]

## ---- CANADA ----
stn.info.train <- stn.info[Lat>47.57 & Lat<72.04 & Long > -150.16 & Long < -104.87] # training area

# standardize
stand_dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec"
stand_file <- file.path(stand_dir, "/standardization.csv")
stand <- fread(stand_file)

if (var == "ppt") {
  stn.info.train[, (month.abb) := lapply(.SD, function(x) log1p(x)), .SDcols = month.abb]
  
  for (m in month.abb) {
    mean <- stand[[3]][1]
    std <- stand[[4]][1]
    stn.info.train[, (m) := (get(m) - mean) / std]
  }
} else {
  stn.info.train[, (month.abb) := lapply(.SD, function(x) x/10), .SDcols = month.abb] # divide by 10
  
  for (m in month.abb) {
    mean <- stand[[1]][1]
    std <- stand[[2]][1]
    stn.info.train[, (m) := (get(m) - mean) / std]
  }
}

# load reference map
ref_CA <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/dem_NWNA_coarse.nc")
blank <- rast(ref_CA)
values(blank) <- NA

template_CA <- rep(blank, 12)
names(template_CA) <- month.abb

pts <- vect(as.data.frame(stn.info.train), geom = c("Long", "Lat"), crs = crs(ref_CA))

months <- month.abb

for (i in seq_along(months)) {
  m <- months[i]
  template_CA[[i]] <- rasterize(pts, blank, field = m)   # replace with monthly station data
}
names(template_CA) <- month.abb

# writeCDF(template_CA, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/", "ppt_CA_station_data.nc"), varname='prec', overwrite = T)

## ---- US ----
# us stations
stn.info.us <- stn.info[Lat<47.5 & Lat>42 & Long > -125 & Long < -104] 

# standardize
stand_dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec"
stand_file <- file.path(stand_dir, "/standardization.csv")
stand <- fread(stand_file)

if (var == "ppt") {
  stn.info.us[, (month.abb) := lapply(.SD, function(x) log1p(x)), .SDcols = month.abb]
  
  for (m in month.abb) {
    mean <- stand[[3]][1]
    std <- stand[[4]][1]
    stn.info.us[, (m) := (get(m) - mean) / std]
  }
} else {
  stn.info.us[, (month.abb) := lapply(.SD, function(x) x/10), .SDcols = month.abb] # divide by 10
  
  for (m in month.abb) {
    mean <- stand[[1]][1]
    std <- stand[[2]][1]
    stn.info.us[, (m) := (get(m) - mean) / std]
  }
}

# load reference map
ref_US <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/test area prep/dem_US.nc")
blank <- rast(ref_US)
values(blank) <- NA

template_US <- rep(blank, 12)
names(template_US) <- month.abb

# randomly sample 10% of the stations for testing, leave 90% for later eval
set.seed(42)
test_stations <- stn.info.us %>% slice_sample(n = ceiling(0.10 * nrow(stn.info.us)))
eval_stations <- stn.info.us %>% anti_join(test_stations)

pts <- vect(test_stations, geom = c("Long", "Lat"), crs = crs(ref_US))

months <- month.abb

for (i in seq_along(months)) {
  m <- months[i]
  template_US[[i]] <- rasterize(pts, blank, field = m)  # replace with monthly station data
}
names(template_US) <- month.abb

usca_merged <- merge(template_CA, template_US)

dem <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/test area prep/dem_stn_train.tif")
usca_final <- project(usca_merged, dem)

writeRaster(usca_final, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/", "ppt_station_data.tif"))
write.csv(test_stations, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/", "ppt_test_stations.csv"))
write.csv(eval_stations, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/", "ppt_eval_stations.csv"))

## ---- TEST AREA ----
# load reference map
ref_test <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/test area prep/dem_stn_test.nc")
blank <- rast(ref_test)
values(blank) <- NA

template_test <- rep(blank, 12)
names(template_test) <- month.abb

pts <- vect(eval_stations, geom = c("Long", "Lat"), crs = crs(ref_test))

months <- month.abb
for (i in seq_along(months)) {
  m <- months[i]
  template_test[[i]] <- rasterize(pts, blank, field = m)  # replace with monthly station data
}
names(template_test) <- month.abb
writeCDF(template_test, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Stations/", "ppt_station_test_data.nc"), varname='prec', overwrite = T)

## ---- PREP PRISM FOR TRAINING AREA (add band of NAs along the bottom to match dem ext)
prism_dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/")
dem_dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/test area prep/")

dem <- rast(paste0(dem_dir, "dem_stn_train.tif"))
months <- month.abb

for (i in seq_along(months)) {
  prism <- rast(paste0(prism_dir, months[i], "/prism_train_coarse.nc"))
  prism_stn <- project(prism, dem)
  writeRaster(prism_stn, paste0(prism_dir, months[i], "/prism_stn_train.tif"))
}


