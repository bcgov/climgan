
# quantitative evaluation of GAN climatologies against PRISM stations
# Colin Mahony colin.mahony@gov.bc.ca


library(rnaturalearth)
library(terra)
library(data.table)
library(sf)
library(scales)
library(RColorBrewer)
library(bcmaps)
library(plotrix) # for Taylor plots

source("data_prep_analysis/util.R")
elements <- c("tmin", "tmax", "prec")
element.names <- c("Tmin", "Tmax", "Precipitation")

#######################
## Common data
#######################

studyarea <- ext(c(-143.5, -105, 45, 65))

dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- crop(dem, studyarea)
X <- dem

#PRISM DEM
dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/PRISM_BC/PRISM_dem/", sep="")
dem.bc <- rast(paste(dir, "PRISM_dem.asc", sep=""))

slope <- terrain(dem, v = "slope", unit = "radians")   # or unit = "degrees" if preferred
aspect <- terrain(dem, v = "aspect", unit = "radians")
hill <- shade(slope, aspect, angle = 40, direction = 270)

bdy.bc <- vect(bc_bound())
bdy.bc <- project(bc, dem.bc)

# Create an ocean mask
maskarea <- studyarea+c(4,4,4,4)
maskpoly <- as.polygons(maskarea) |> st_as_sf()
st_crs(maskpoly) <- crs(dem)
land <- vect(ne_download(scale = 10, type = "land", category = "physical", returnclass = "sf"))
land <- crop(land, maskarea)
land_union <- st_union(st_as_sf(land))
oceanmask_geom <- st_difference(st_geometry(maskpoly), st_geometry(land_union))
oceanmask <- st_sf(geometry = oceanmask_geom, crs = st_crs(maskpoly))
oceanmask <- vect(oceanmask)

# -----------------------------------------
# define the evaluation regions

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

plot(studyarea)
plot(bdy.bc, add=T)
plot(region1, add=T, border="red")
plot(region2, add=T, border="red")
plot(region3, add=T, border="red")
plot(region4, add=T, border="red")


#######################
## 
#######################

datasets <- c("climr", "fm", "sp")
datasets.names <- c("climr", "Foundational", "Specialized")

monthpair <- c(4,10)
monthpair <- c(1,7)
monthpair <- c(6,12)

png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Figures/GanEval.TaylorPlots", paste(month.abb[monthpair], collapse = ""), "png",sep="."), type="cairo", units="in", width=9, height=6.25, pointsize=10, res=600)
par(mfrow=c(2,3), mar=c(0,0,0,0), mgp=c(2,0.25, 0), tck=-0.01)
m=1
for(m in monthpair){
  monthcode = monthcodes[m]
  
  e=3
  for(e in 1:3){
    element = elements[e]
    
    # load the source STATION data for the BC prism
    dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/PRISM_BC/", sep="")
    stn.info <- fread(paste(dir, "Stations/",c("Tmin", "Tmax", "Pr")[e],"_uscdn_8110.csv", sep="")) #read in
    for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
    stn.info <- stn.info[-which(El_Flag=="@"),]
    stn <- vect(stn.info, geom = c("Long", "Lat"), crs = "EPSG:4326")
    stn.data <- stn.info[,get(month.abb[m])]
    stn.data <- if(e==3) log2(stn.data) else stn.data/10
    stn.info <- stn.info[is.finite(stn.data),]
    stn.data <- stn.data[is.finite(stn.data)]
    
    ###########################################################
    ## plot a key map
    #######################
    
    if(m==1){
      png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Figures/GanEval.KeyMap", elements[e],"png",sep="."), type="cairo", units="in", width=6.5, height=5.8, pointsize=10, res=600)
      par(mar=c(0,0,0,0))
      legend.args=list(text='Elevation (m)', side=2, font=2, line=0.5, cex=0.8)
      lim <- quantile(values(X), 0.99)
      values(X)[which(values(X)>lim)] <- lim
      # plot(hill, col=alpha(grey(0:100/100), 1), maxpixels=ncell(hill), legend=F)
      plot(X, col=terrain.colors(99), xaxt="n", yaxt="n")
      plot(crop(hill,X), add=T, col=alpha(grey(0:100/100), 0.5), legend=F, legend.mar=0)
      plot(oceanmask, add=T, col="white", border=F)
      # plot(bc, add=T)
      plot(stn, add=T, pch=16, col="gray50", cex=0.8, lwd=0.5)
      # mtext(paste("(a)", sep=""), side=1, line=-1.5, adj=0.005, font=2, cex=0.8)
      
      for(i in 1:length(regions)){
        region <- get(paste("region", i, sep=""))
        plot(region, add=T, lty=2, lwd=2)
        text(if(i==5) -120 else ext(region)[2], ext(region)[4]-0.5, regions[i], font=2, pos=2, offset=0.1)
      }
      l <- ext(X)
      rect(l[1], l[3], l[2], l[4], border = "black", lwd = 1)
      
      dev.off()
    }
    
    #################################
    ## Taylor plots
    #################################
    
    # load the climr mosaic
    climr <- rast(paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_1981_2010_", c("Tmin", "Tmax", "Pr")[e], monthcodes[m], ".tif", sep=""))
    
    # load the foundational model
    fm <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
    if(e==3) fm <- expm1(fm) # TEMPORARY UNTIL TIRION PRODUCES RESULTS IN MM/MONTH.
    
    # optional loop for comparing generators from different epochs
    epochs <- seq(50,250,50)
    for(epoch in epochs){
      
      # load the specialized model
      sp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model4/", tolower(month.abb[m]), "/spec1/gen",epoch,"/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
      if(e==3) sp <- expm1(sp) # TEMPORARY UNTIL TIRION PRODUCES RESULTS IN MM/MONTH.
      
      
      # ALTERNATIVE: plot a single taylor plot. 
      png(filename=paste("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/Figures/GanEval.TaylorPlot", elements[e], month.abb[m], "gen250", "png",sep="."), type="cairo", units="in", width=5, height=5, pointsize=10, res=600)
      par(mfrow=c(1,1), mar=c(2,2,1,1), mgp=c(2,0.25, 0), tck=-0.01)
      
      taylor.diagram(ref = 0:5, model = 0:5, main = paste(month.name[m], element.names[e]),
                     col = "black", pch = 1, cex = 1, normalize = TRUE, xlab="",
                     sd.arcs = TRUE, grad.corr.lines = TRUE, pos.cor = TRUE)
      
      colScheme <- c("black", "dodgerblue", "yellow", "red", "pink")
      pointScheme <- c(21, 22, 24)
      
      # Add a legend
      legend("topright",
             legend = regions,
             title="Regions",
             fill = colScheme,      # colored boxes
             border = NULL,
             bty = "n",
             inset = c(0,-0.07))
      
      # second column: black shapes
      legend("topright",
             legend = datasets.names,
             title="Datasets",
             bg = "white",
             pch = pointScheme,  # shapes
             pt.cex = 1.5,
             bty = "n",
             inset = c(0.3,-0.07))   # shift to the left so they appear in second column
      
      for(r in 1:length(regions)){
        region <- get(paste("region", r, sep="")) 
        crs(region) <- "EPSG:4326"
        
        stn.vect <- vect(stn.info, geom = c("Long", "Lat"), crs = "EPSG:4326")
        stn.crop <- crop(stn.vect, region)
        stn.values <- as.data.frame(stn.crop)[,which(names(stn.crop)==month.abb[m])]
        stn.values <- if(e==3) log2(stn.values) else stn.values/10
        
        for(dataset in datasets){
          d=which(datasets==dataset)
          temp <- get(dataset)
          temp <- mask(temp, region)
          # plot(temp)
          # plot(stn.crop, add=T)
          stn.temp <- as.vector(unlist(extract(temp, stn.crop)[2]))
          stn.temp <- if(e==3) log2(stn.temp) else stn.temp
          assign(paste("stn", dataset, elements[e], monthcodes[m], sep="."), stn.temp)
          assign(paste("error", dataset, elements[e], monthcodes[m], sep="."), stn.temp - stn.values)
          # Add points on the Taylor diagram
          # taylor.diagram(ref = stn.values, model = stn.temp, add = TRUE,
          #                col = colScheme[d], pch = c(16,15,17)[r], cex = 1.2, normalize = TRUE, pcex=1.5)
          taylor.diagram.filled(ref = stn.values, model = stn.temp, add = TRUE,
                                bg = colScheme[r], pch = c(21,22,24)[d], cex = 1.5, normalize = TRUE)
        }
        
        # print(paste("region", i))
      }
      
      dev.off() # ALTERNATIVE: for plotting a single month. 
      
      print(epoch)
    }
    print(element)
  }
  print(month.abb[m])
}
dev.off() #STANDARD: for plotting multiple months
