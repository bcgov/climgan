# Evaluation of GAN climatological models relative to the 1981-2010 PRISM station climatologies

library(terra)
library(climr) #for legend_ramp()
library(data.table)
library(rworldmap)
library(bcmaps)
library(scales) # for alpha transparency
library(RColorBrewer)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("tmin", "tmax", "prec")
element.names <- c("mean\ndaily minimum temperature (\u00b0C)", "mean\ndaily maximum temperature (\u00b0C)", "precipitation (mm)")

studyarea <- ext(c(-143.5, -112, 47.5, 65))


# -----------------------------------------
# DEM 

dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- crop(dem, studyarea)

bdy <- rworldmap::countriesLow
bdy.bc <- project(vect(bcmaps::bc_bound()), bdy)

# -----------------------------------------
# Compare 2 maps
# -----------------------------------------

e=2
for(e in 2:length(elements)){
  
  # -----------------------------------------
  # map prediction error
  
  m=4
  for(m in 1:12){
    
    dir <- paste0("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/operational/WorldClim/", elements[e], "/", tolower(month.abb[m]), "/Predictions/")
    clim.v1 <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    name.v1 <- "v2025f1"
    
    clim.v2 <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/", tolower(month.abb[m]), "/specialization1/gen250/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    name.v2 <- "v2025f1s1"
    
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
    stn.v1 <- as.vector(unlist(extract(clim.v1, stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    stn.v2 <- as.vector(unlist(extract(clim.v2, stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    stn.v1 <- if(e==3) log2(stn.v1) else stn.v1
    stn.v2 <- if(e==3) log2(stn.v2) else stn.v2
    
    error.v1 <- stn.v1-stn
    error.v2 <- stn.v2-stn
    # hist(error)
    
    lim.lower <- quantile(c(error.v1, error.v2), 0.01, na.rm=T)
    lim.upper <- quantile(c(error.v1, error.v2), 0.99, na.rm=T)
    lim <- round(max(abs(c(lim.lower, lim.upper))),1)
    breaks <- seq(0-lim,lim,lim/50)
    ColScheme <- colorRampPalette(rev(c(brewer.pal(11, "RdBu")[1:5], rep("white", 3), brewer.pal(11, "RdBu")[7:11])))(length(breaks)-1)
    if(e==3) ColScheme <- rev(ColScheme)
    
    # plot GAN error relative to station normals
    png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/maps/Error_", elements[e], "_", monthcodes[m], "_", name.v2, ".png", sep=""), type="cairo", units="in", width=8, height=4, pointsize=8, res=600)
    par(mar=c(0.25,0.25, 0.25,0.25), mfrow=c(1,2))
    
    v <- "v1"
    for(v in c("v1", "v2")){ # maps for both generations of the emulator
      
      error <- get(paste("error", v, sep="."))
      error[which(error>lim)] <- lim
      error[which(error < 0-lim)] <- 0-lim
      plot(stn.info[, c("Long", "Lat")], yaxs="i", xaxt="n", yaxt="n", xaxs="i", 
           pch=21, lwd=0.4, cex=(log(nndist[,4])/2.5)^1, 
           bg=ColScheme[as.numeric(cut(error,breaks = breaks))], 
           col=alpha("grey", 0.5))
      plot(bdy.bc, add=T, lwd=0.1)
      plot(bdy, add=T, lwd=0.1)
      
      legend_ramp(dem, title = paste("Error in", month.name[m], element.names[e]), 
                  ColScheme = ColScheme, breaks = breaks, pos=c(0.2, 0.23, 0.05, 0.4), 
                  log = if(e==3) 2 else NULL, 
                  log.relative = TRUE, 
                  horizontal = FALSE, 
                  title.height = if(e==3) 1 else 2)
      mtext(get(paste("name", v, sep=".")), line = -1.5, side=1, adj=0.35, cex=2)
      
      print(v)
    }
    dev.off()
    
    print(monthcodes[m])  
  }
  
  print(elements[e])
}



# -----------------------------------------
# Compare 3 maps
# -----------------------------------------

e=2
for(e in 2:length(elements)){
  
  # -----------------------------------------
  # map prediction error
  
  m=4
  for(m in 1:12){
    
    dir <- paste0("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/operational/WorldClim/", elements[e], "/", tolower(month.abb[m]), "/Predictions/")
    clim.v1 <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
    name.v1 <- "v2024"
    
    clim.v2 <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    name.v2 <- "v2025f1"
    
    clim.v3 <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/", tolower(month.abb[m]), "/specialization1/gen250/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    name.v3 <- "v2025f1s1"
    
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
    stn.v1 <- as.vector(unlist(extract(clim.v1, stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    stn.v2 <- as.vector(unlist(extract(clim.v2, stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    stn.v3 <- as.vector(unlist(extract(clim.v3, stn.info[, c("Long", "Lat"), with = FALSE])[2]))
    stn.v1 <- if(e==3) log2(stn.v1) else stn.v1
    stn.v2 <- if(e==3) log2(stn.v2) else stn.v2
    stn.v3 <- if(e==3) log2(stn.v3) else stn.v3
    
    error.v1 <- stn.v1-stn
    error.v2 <- stn.v2-stn
    error.v3 <- stn.v3-stn
    # hist(error)
    
    lim.lower <- quantile(c(error.v1, error.v2, error.v3), 0.01, na.rm=T)
    lim.upper <- quantile(c(error.v1, error.v2, error.v3), 0.99, na.rm=T)
    lim <- round(max(abs(c(lim.lower, lim.upper))),1)
    breaks <- seq(0-lim,lim,lim/50)
    ColScheme <- colorRampPalette(rev(c(brewer.pal(11, "RdBu")[1:5], rep("white", 3), brewer.pal(11, "RdBu")[7:11])))(length(breaks)-1)
    if(e==3) ColScheme <- rev(ColScheme)
    
    # plot GAN error relative to station normals
    png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model1/maps/Error_", elements[e], "_", monthcodes[m], "_", name.v3, ".png", sep=""), type="cairo", units="in", width=12, height=4, pointsize=8, res=600)
    par(mar=c(0.25,0.25, 0.25,0.25), mfrow=c(1,3))
    
    v <- "v1"
    for(v in c("v1", "v2", "v3")){ # maps for both generations of the emulator
      
      error <- get(paste("error", v, sep="."))
      error[which(error>lim)] <- lim
      error[which(error < 0-lim)] <- 0-lim
      plot(stn.info[, c("Long", "Lat")], yaxs="i", xaxt="n", yaxt="n", xaxs="i", 
           pch=21, lwd=0.6, cex=(log(nndist[,4])/2.5)^1, 
           bg=ColScheme[as.numeric(cut(error,breaks = breaks))], 
           col=alpha("grey", 0.5))
      plot(bdy.bc, add=T, lwd=0.1)
      plot(bdy, add=T, lwd=0.1)
      
      legend_ramp(dem, title = paste("Error in", month.name[m], element.names[e]), 
                  ColScheme = ColScheme, breaks = breaks, pos=c(0.2, 0.23, 0.05, 0.4), 
                  log = if(e==3) 2 else NULL, 
                  log.relative = TRUE, 
                  horizontal = FALSE, 
                  title.height = if(e==3) 1 else 2)
      mtext(get(paste("name", v, sep=".")), line = -1.5, side=1, adj=0.35, cex=2)
      
      print(v)
    }
    dev.off()
    
    print(monthcodes[m])  
  }
  
  print(elements[e])
}
