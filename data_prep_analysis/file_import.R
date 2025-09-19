library(terra)

prism_folder <- "O:/Climatologies/PRISM_akbcus/Tmin/"

write_folder <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/PRISM/tmin/"

for (i in 1:12) {
  month <- sprintf("%02d", i)
  input_file <- paste0("AK_BC_US_PRISM_merged_Tmin_8110_", month, ".asc")
  
  r <- rast(paste0(prism_folder, input_file))
  
  output_file <- paste0(write_folder, "ak_tmin_1981_2010.", month, ".nc")
  
  writeCDF(r, filename = output_file, varname = "tmin", overwrite = TRUE)
}


r <- rast("C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/climr_mosaic_dem.tif")

output_file <- "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/dem_ak.nc"

writeCDF(r, filename = output_file, varname = "dem", overwrite = TRUE)

