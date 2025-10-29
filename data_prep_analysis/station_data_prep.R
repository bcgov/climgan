# putting station data into a map to be tiled

library(terra)
library(data.table)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

# load the source STATION data for the BC prism
dir <- "O:/Climatologies/PRISM_BC/"
stn.info <- fread(paste(dir, "Stations/","tmin_uscdn_8110.csv", sep="")) #read in
for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
stn.info <- stn.info[-which(El_Flag=="@"),]
stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]
stn.info <- stn.info[Lat>47.57 & Lat<72.04 & Long > -150.16 & Long < -104.87] # training area
stn.info[, (month.abb) := lapply(.SD, function(x) x/10), .SDcols = month.abb] # divide by 10

# standardize
stand_dir <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/tmin"

for (m in month.abb) {
  stand_file <- file.path(stand_dir, paste0(tolower(m), "/standardization.csv"))
  stand <- fread(stand_file)
  mean <- stand[[3]][1]
  std <- stand[[4]][1]
  stn.info[, (m) := (get(m) - mean) / std]
}

# load reference map
ref <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/dem_NWNA_coarse.nc")
blank <- rast(ref)
values(blank) <- NA

template <- rep(blank, 12)
names(template) <- month.abb

pts <- vect(stn.info, geom = c("Long", "Lat"), crs = crs(ref))

months <- month.abb

for (i in seq_along(months)) {
  m <- months[i]
  template[[i]] <- rasterize(pts, blank, field = m)  # replace with monthly station data
}

writeCDF(template, paste0("O:/Mosaic_Yukon/Tirion/Stations/", "tmin_station_data.nc"), varname='tmin', overwrite = T)