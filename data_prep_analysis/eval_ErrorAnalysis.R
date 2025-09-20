# Evaluation of GAN climatological models relative to the 1981-2010 PRISM station climatologies

library(terra)
library(data.table)
library(rworldmap)
library(bcmaps)
library(scales)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("tmin", "tmax", "prec")
element.names <- c("mean\ndaily minimum temperature (\u00b0C)", "mean\ndaily maximum temperature (\u00b0C)", "precipitation (mm)")

studyarea <- ext(c(-143.5, -112, 47.5, 65))



# -----------------------------------------
# Assemble the monthly files into a 12-month raster stack
for(e in 2:length(elements)){
  
  for(m in 1:12){
    
    dir <- paste0("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/operational/WorldClim/", elements[e], "/", tolower(month.abb[m]), "/Predictions/")
    temp <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
    assign(paste(elements[e], "v2024", sep="."), if(m==1) temp else c(get(paste(elements[e], "v2024", sep=".")), temp))
    
    
    temp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    assign(paste(elements[e], "v2025", sep="."), if(m==1) temp else c(get(paste(elements[e], "v2025", sep=".")), temp))
    
    print(monthcodes[m])  
  }
  print(elements[e])
}


# -----------------------------------------
# DEM 

dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- crop(dem, studyarea)

bdy <- rworldmap::countriesLow
bdy.bc <- project(vect(bcmaps::bc_bound()), bdy)

# -----------------------------------------
# load the source STATION data for the BC prism

dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/"
stn.info <- fread(paste(dir, "Stations/",c("Tmin", "Tmax", "Pr")[e],"_uscdn_8110.csv", sep="")) #read in
for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
stn.info <- stn.info[-which(El_Flag=="@"),]
stn.info <- stn.info[complete.cases(stn.info[, ..month.abb])]
stn.info <- stn.info[Long > studyarea[1] & Long < studyarea[2] & Lat>studyarea[3] & Lat<studyarea[4]]


plot(dem)
plot(studyarea, add=T)
plot(bdy, add=T, border="dodgerblue")
plot(bdy.bc, add=T, border="dodgerblue")

points(stn.info[, c("Long", "Lat")], cex=0.5, pch=16)

# -----------------------------------------
# map prediction error

for(e in 2:length(elements)){
  
  
  for(m in 1:12){
    
    # -------------------------------
    # process station data
    
    dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/"
    stn.info <- fread(paste(dir, "Stations/",c("Tmin", "Tmax", "Pr")[e],"_uscdn_8110.csv", sep="")) #read in
    for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
    stn.info <- stn.info[-which(El_Flag=="@"),]
    stn.info <- stn.info[complete.cases(stn.info[, get(month.abb[m])])]
    stn.info <- stn.info[Long > studyarea[1] & Long < studyarea[2] & Lat>studyarea[3] & Lat<studyarea[4]]
    stn <- stn.info[,get(month.abb[m])]
    stn <- if(e==3) log2(stn) else stn/10
    
    # calculate distances between stations, for point size during plotting
    library(FNN)
    ## reproject coordinates to km using a cosine(latitude) transformation of longitude in order to approximate the convergence of longitudes. NB this only works over short distances. 
    kmPerDegLat <- 111
    CentralLon <- -120 #arbitrary central longitude
    stn.info$lat.km <- stn.info$Lat*kmPerDegLat
    stn.info$lon.km <- (stn.info$Long-CentralLon)*kmPerDegLat*cos(stn.info$Lat*pi/180)
    
    #find distances between stations. 
    k=4 #4 nearest neighbours
    nndist <- as.data.frame(get.knnx(stn.info[,c("lon.km", "lat.km")], stn.info[,c("lon.km", "lat.km")], k=k, algorithm="brute")[2])
    
    # -------------------------------
    # extract both gan predictions to build a common colour scheme
    gan.v2024 <- as.vector(unlist(extract(get(paste(elements[e], "v2024", sep="."))[[m]], stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    gan.v2025 <- as.vector(unlist(extract(get(paste(elements[e], "v2025", sep="."))[[m]], stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    gan.v2024 <- if(e==3) log2(gan.v2024) else gan.v2024
    gan.v2025 <- if(e==3) log2(gan.v2025) else gan.v2025
    
    error.v2024 <- gan.v2024-stn
    error.v2025 <- gan.v2025-stn
    # hist(error)
    
    lim.lower <- quantile(c(error.v2024, error.v2025), 0.01, na.rm=T)
    lim.upper <- quantile(c(error.v2024, error.v2025), 0.99, na.rm=T)
    lim <- round(max(abs(c(lim.lower, lim.upper))),1)
    breaks <- seq(0-lim,lim,lim/50)
    ColScheme <- colorRampPalette(rev(c(brewer.pal(11, "RdBu")[1:5], rep("white", 3), brewer.pal(11, "RdBu")[7:11])))(length(breaks)-1)
    if(e==3) ColScheme <- rev(ColScheme)
    
    # plot GAN error relative to station normals
    png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/maps/Error_", elements[e], "_", monthcodes[m], ".png", sep=""), type="cairo", units="in", width=8, height=4, pointsize=8, res=600)
    par(mar=c(0.25,0.25, 0.25,0.25), mfrow=c(1,2))
    
    v <- "v2024"
    for(v in c("v2024", "v2025")){ # maps for both generations of the emulator(Susan=2024; tirion=2025)
      
      error <- get(paste("error", v, sep="."))
      error[which(error>lim)] <- lim
      error[which(error < 0-lim)] <- 0-lim
      plot(stn.info[, c("Long", "Lat")], yaxs="i", xaxt="n", yaxt="n", xaxs="i", 
           pch=21, lwd=0.4, cex=(log(nndist[,4])/2.5)^1, 
           bg=ColScheme[as.numeric(cut(error,breaks = breaks))], 
           col=alpha("grey", 0.5))
      plot(bdy.bc, add=T, lwd=0.1)
      plot(bdy, add=T, lwd=0.1)
      
      legend_ramp(comp, title = paste("Error in", month.name[m], element.names[e]), 
                  ColScheme = ColScheme, breaks = breaks, pos=c(0.2, 0.23, 0.05, 0.4), 
                  log = if(e==3) 2 else NULL, 
                  log.relative = TRUE, 
                  horizontal = FALSE, 
                  title.height = if(e==3) 1 else 2)
      mtext(v, line = -1.5, side=1, adj=0.35, cex=2)
      
      print(v)
    }
    dev.off()
    
    print(monthcodes[m])  
  }
  print(elements[e])
}
