# Evaluation of GAN climatological model in generating a credible annual cycle

library(terra)
library(data.table)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("tmin", "tmax", "prec")
element.names <- c("mean\ndaily minimum temperature (\u00b0C)", "mean\ndaily maximum temperature (\u00b0C)", "precipitation (mm)")

# studyarea <- ext(c(-137, -120, 59, 64.5))
studyarea <- ext(c(-125, -113, 49, 60))

# dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
# dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- rast(paste0("O:/Climatologies/climr_mosaic/climr_mosaic_dem_800m.tif"))
dem <- crop(dem, studyarea)

dem.bc <- rast(paste0("O:/Climatologies/PRISM_BC/PRISM_dem/PRISM_dem.asc"))
bdy.bc <- vect(bc_bound())
bdy.bc <- project(bdy.bc, dem.bc)

regions <- c("YT", "AB_N", "AB_S", "US", "BC")
lon1 <- -141; lat1 = 60.1
region1 <- as.polygons(ext(c(lon1, lon1+21, lat1, lat1+5)))

region2 <- as.polygons(ext(c(-119.9, -113, 54.1, 60)))

region3 <- ext(c(-120, -113, 49.01, 54))
region3 <- as.polygons(region3)
region3 <- erase(region3, bdy.bc)

region4 <- as.polygons(ext(c(-125, -110, 45, 48.9)))
region4 <- erase(region4, bdy.bc)
region4 <- crop(region4, land)

region5 <- bdy.bc

region6 <- region2+region3

plot(studyarea)
plot(bdy.bc, add=T)
plot(region1, add=T, border="red")
plot(region2, add=T, border="red")
plot(region3, add=T, border="red")
plot(region4, add=T, border="red")
# -----------------------------------------
# Assemble the monthly files into a 12-month raster stack

epoch <- 50

for(e in 1:1){
  
  for(m in 1:12){
    
    dir <- paste0("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/operational/WorldClim/", elements[e], "/", tolower(month.abb[m]), "/Predictions/")
    temp <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
    assign(paste(elements[e], "v2024", sep="."), if(m==1) temp else c(get(paste(elements[e], "v2024", sep=".")), temp))
    
    # temp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    temp <- rast(paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_merged_masked.tif", sep=""))
    assign(paste(elements[e], "fm4", sep="."), if(m==1) temp else c(get(paste(elements[e], "fm4", sep=".")), temp))
    
    # temp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/spec1/gen",epoch,"/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    temp <- rast(paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/spec1/gen",epoch,"/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    assign(paste(elements[e], "fm4s", sep="."), if(m==1) temp else c(get(paste(elements[e], "fm4s", sep=".")), temp))
    
    # temp <- rast(paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/spec1/gen",epoch,"/", tolower(month.abb[m]), "_debias.nc", sep=""))
    temp <- rast(paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_debias.nc", sep=""))
    assign(paste(elements[e], "fm4db", sep="."), if(m==1) temp else c(get(paste(elements[e], "fm4db", sep=".")), temp))
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

for(e in 1:1){
  
  # -----------------------------------------
  # load the source STATION data for the BC prism
  # dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/"
  # stn.info <- fread(paste(dir, "Stations/",c("Tmin", "Tmax", "Pr")[e],"_uscdn_8110.csv", sep="")) #read in
  dir <- paste("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/Tirion/", sep="")
  stn.info <- fread(paste(dir, "Stations/", c("tmin/", "tmax/", "ppt/")[e], c("tmin", "tmax", "pr")[e],"_uscdn_8110.csv", sep="")) #read in
  for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
  stn.info <- stn.info[-which(El_Flag=="@"),]
  stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]
  # stn.info <- stn.info[Lat > 60 & Lat < studyarea[4] & Long > -140 & Long < studyarea[2] ]
  pts <- vect(stn.info, geom = c("Long", "Lat"), crs = "EPSG:4326")
  inside <- apply(relate(pts, region6, "within"), 1, any)
  stn.info.bc <- stn.info[inside]
  
  
  # -----------------------------------------
  # plot comparison of station data
  # stns <- stn.info$St_ID
  stns <- stn.info.bc$St_ID
  print(length(stns))
  for(stn in stns){
    par(mfrow=c(2,1), mar=c(3,4,2,1), mgp=c(2, 0.25, 0))
    
    plot(dem)
    pt <- stn.info[St_ID == stn, c("Long", "Lat")]
    points(pt, cex=1, pch=16)
    text(pt, stn, cex=0.95, pos=4)
    
    stn.data <- as.vector(unlist(stn.info[St_ID == stn, ..month.abb]))
    if(e !=3) stn.data <- stn.data/10
    v2024 <- as.vector(unlist(extract(get(paste(elements[e], "v2024", sep=".")), stn.info[St_ID == stn, c("Long", "Lat"), with = FALSE])[-1]))
    fm4 <- as.vector(unlist(extract(get(paste(elements[e], "fm4", sep=".")), stn.info[St_ID == stn, c("Long", "Lat"), with = FALSE])[-1]))
    fm4s <- as.vector(unlist(extract(get(paste(elements[e], "fm4s", sep=".")), stn.info[St_ID == stn, c("Long", "Lat"), with = FALSE])[-1]))
    fm4db <- as.vector(unlist(extract(get(paste(elements[e], "fm4db", sep=".")), stn.info[St_ID == stn, c("Long", "Lat"), with = FALSE])[-1]))
    
    plot(stn.data, type="l", ylim=range(c(stn.data, v2024, fm4, fm4s, fm4db)), lwd=2, xlab="", xaxt="n", ylab=element.names[e], main=paste("Station:", stn))
    axis(1, at=1:12, labels = month.abb, las=2, tck=0)
    lines(fm4db, col="purple")
    lines(fm4s, col="orange")
    lines(fm4, col="dodgerblue")
    lines(v2024, col="gray", lty=2)
    legend("topleft", legend=c("Station data", "fm4db", "fm4s", "fm4", "v2024"), lwd=c(2,1,1,1,1), lty=c(1,1,1,1,2), col=c("black", "purple", "orange", "dodgerblue", "gray"), bty="n")
    
    print(stn)
  }
  print(elements[e])
}

par(mfrow=c(1,1))
r <- get(paste(elements[e], "v2025", sep="."))
plot(r[[1]])
points(stn.info$Long, stn.info$Lat, cex=0.9)


# dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
# dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- rast(paste0("O:/Climatologies/climr_mosaic/climr_mosaic_dem_800m.tif"))
dem <- crop(dem, r)
plot(dem)
points(stn.info$Long, stn.info$Lat, cex=0.9)

# evalarea <- ext(c(-141, -120, 60, 65))
plot(evalarea, add=T)
