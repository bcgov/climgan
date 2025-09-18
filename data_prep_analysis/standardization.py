import pandas as pd
import xarray as xr
import torch

## ERA5
era5_folder = "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/era5_clim/prec/"

era5_fields = xr.open_dataset(era5_folder + "ppt_full.nc")
era5 = torch.from_numpy(era5_fields.to_array().to_numpy())[0,...]

stats = []

mask = ~torch.isnan(era5)
era5_mean = era5[mask].mean()
era5_std = era5[mask].std()
stats.append({"mean": float(era5_mean), "std": float(era5_std)})
  
df = pd.DataFrame(stats)
df.to_csv(era5_folder + "standardization.csv", index=False)

## dem
dem_folder = "C:/Users/TGRICE/OneDrive - Government of BC/Documents/GANs/dem/"

dem_fields = xr.open_dataset(dem_folder + "dem_full.nc")
dem = torch.from_numpy(dem_fields.to_array().to_numpy())[0,...]

stats = []

# standardize
mask = ~torch.isnan(dem)
dem_mean = dem[mask].mean()
dem_std = dem[mask].std()
stats.append({"mean": float(dem_mean), "std": float(dem_std)})

df = pd.DataFrame(stats)
df.to_csv(dem_folder + "standardization.csv", index=False)
