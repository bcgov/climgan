library(terra)

# create PRISM maps

dir_ak <- "O:/Climatologies/PRISM_AK/"
dir_bc <- "O:/Climatologies/PRISM_BC/"
dir_daymet <- "O:/Climatologies/Daymet/"

# var <- "tmin"

ak_ext <- ext(c(-150, -140.425, 58.625, 71.375))
bc_ext <- ext(c(-140, -114, 48, 60))
daymet_ext <- ext(c(-113, -105, 48, 72))

for (month in 1:12) {
  ak <- rast(paste0(dir_ak, "ak_ppt_1981_2010.", sprintf("%02d", month), ".asc", sep=""))
  bc <- rast(paste0(dir_bc, "pr_monClim_PRISM_historical_198101-201012_", month, ".tif", sep=""))
  daymet <- rast(paste0(dir_daymet, "daymet_1981_2010_pr_", sprintf("%02d", month), ".tif", sep=""))
  crs(ak) = crs(bc)
  ak_proj <- project(ak, daymet)
  
  # crop to area wanted
  ak_cropped <- crop(ak_proj, ak_ext)
  daymet_cropped <- crop(daymet, daymet_ext)
  
  # merge 
  full <- merge(ak_cropped, bc, daymet_cropped)
  writeRaster(full, paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/", "prism_train_", month, ".nc", sep=""))
}

# use CDO commands with the gridfile.txt in the PRISM folder to coarsen and line up with ERA5

# find yearly mean and st dev
months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthly_rasters <- list()

for (month in months) {
  prism_folder <- paste0("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/", month, "/")
  prism <- rast(paste(prism_folder, "prism_train_coarse.nc", sep=""))
  test <- rast(paste0(prism_folder, "prism_test.nc", sep=""))
  
  combined <- merge(prism, test)
  monthly_rasters[[month]] <- combined
}

all_months <- rast(monthly_rasters)

mean_val <- mean(values(all_months), na.rm = TRUE)
sd_val <- sd(values(all_months), na.rm = TRUE)

log_mean_val <- mean(log1p(values(all_months)), na.rm = TRUE)
log_sd_val <- sd(log1p(values(all_months)), na.rm = TRUE)

df <- data.frame(
  mean = mean_val,
  sd = sd_val,
  log_mean = log_mean_val,
  log_sd = log_sd_val
)

write.csv(df, "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/prec/standardization.csv", row.names = FALSE)