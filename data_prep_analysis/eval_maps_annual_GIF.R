library(terra)
library(climr)
library(data.table)
library(RColorBrewer)
library(magick)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("tmax")
element.names <- c("mean\ndaily maximum temperature (\u00b0C)")

v <- 2025
for(v in c(2024, 2025)){ # maps for both generations of the emulator(Susan=2024; tirion=2025)
  
  #-----------------------
  #determine a common colour scale for all months of each element
  e=1
  for(e in 1:length(elements)){
    assign(paste("lim", elements[e], sep="."), vector())
    bbox <- ext(-141, -120, 57, 68)
    
    for(m in 1:12){
      
      if(v == 2024){
        dir <- paste0("O:/Mosaic_Yukon/operational/WorldClim/", elements[e], "/", tolower(month.abb[m]), "/Predictions/")
        comp <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
        comp_crop <- crop(comp, bbox)
      } else {
        comp <- rast(paste("O:/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model2/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
        comp_crop <- crop(comp, bbox)
      }
      if(elements[e]=="prec") values(comp_crop) <- log2(values(comp_crop))
      
      temp <- values(comp_crop)
      temp <- temp[is.finite(temp)]
      lim=c(quantile(temp, 0.005), quantile(temp, 0.995))
      assign(paste("lim", elements[e], sep="."), c(get(paste("lim", elements[e], sep=".")), lim))
      
      print(monthcodes[m])  
    }
    print(elements[e])
  }
  
  #-----------------------
  #export map pngs
  
  e=1
  m=3
  for(e in 1:length(elements)){
    bbox <- ext(-141, -120, 57, 68)
    
    common.colours <- TRUE
    # common.colours <- if(elements[e]=="prec") TRUE else FALSE
    
    for(m in 1:12){
      
      if(v == 2024){
        dir <- paste0("O:/Mosaic_Yukon/operational/WorldClim/", c("tmin", "tmax", "prec")[e], "/", tolower(month.abb[m]), "/Predictions/")
        comp <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
        comp_crop <- crop(comp, bbox)
      } else {
        comp <- rast(paste("O:/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model2/", tolower(month.abb[m]), "/", tolower(month.abb[m]), "_fullregion_masked.nc", sep=""))
        comp_crop <- crop(comp, bbox)
      }
      
      if(elements[e]=="prec") values(comp_crop) <- log2(values(comp_crop))
      # comp <- mask(comp, land)
      
      if(common.colours){
        temp <- get(paste("lim", elements[e], sep="."))
        inc=diff(range(temp))/500
        breaks=seq(min(temp, na.rm=T)-inc, max(temp, na.rm=T)+inc, inc)
      } else {
        temp <- values(comp_crop)
        temp <- temp[is.finite(temp)]
        inc=diff(range(temp))/500
        breaks=seq(quantile(temp, 0.005)-inc, quantile(temp, 0.995)+inc, inc)
      }
      ColScheme <- colorRampPalette(if(elements[e]=="prec") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
      
      values(comp_crop)[values(comp_crop)>max(breaks)] <- max(breaks)
      values(comp_crop)[values(comp_crop)<min(breaks)] <- min(breaks)
      
      png(filename=paste("O:/Mosaic_Yukon/Tirion/Results/foundational_model/", elements[e], "/Model2/maps/GIF/v", v, "_", elements[e], "_", monthcodes[m], ".png", sep=""), type="cairo", units="in", width=8, height=8, pointsize=12, res=600)
      par(mfrow=c(1,1), mar=c(0,0,0,0))
      plot(comp_crop, col=ColScheme, breaks=breaks, legend=F, main="", axes=F, maxcell=ncell(comp_crop), mar=NA)
      #legend_ramp(comp, title = paste("1981-2010", month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(0.2, 0.23, 0.05, 0.45), log = if(e==3) 2 else NULL, horizontal = FALSE, title.height = if(e==3) 1 else 2)
      dev.off()
      
      print(monthcodes[m])  
    }
    print(elements[e])
  }
  print(v)
}

# make the GIF
files <- sprintf("O:/Mosaic_Yukon/Tirion/Results/foundational_model/tmax/Model1/maps/GIF/v2025_tmax_%02d.png", 1:12)

imgs <- image_read(files)

gif <- image_animate(imgs, fps = 1)

image_write(gif, "O:/Mosaic_Yukon/Tirion/Results/foundational_model/tmax/Model1/maps/GIF/tmax_2025.gif")
