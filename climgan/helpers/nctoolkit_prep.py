# -*- coding: utf-8 -*-
"""
Created on Mon Jan 16 11:22:36 2023

@author: kirid
"""

import nctoolkit as nc
import numpy as np

##era5
fine = True

coarse_covars = {
    "u10": "/home/kiridaust/Masters/Data/Era5/BuMo/U10_LR.nc",
    "v10": "/home/kiridaust/Masters/Data/Era5/BuMo/V10_LR.nc",
    #"precip": "/home/kiridaust/Masters/Data/wind_vars_full/PREC_LR.nc",
    #"precip": "/home/kiridaust/Masters/Data/wind_vars_full/PREC_HR.nc", ##coarsened WRF
    "temp": "/home/kiridaust/Masters/Data/Era5/BuMo/TAS_LR.nc",
    #"q": "/home/kiridaust/Masters/Data/wind_vars_full/ERA5_Q2_LR.nc",
    "cape": "/home/kiridaust/Masters/Data/Era5/BuMo/CAPE_LR.nc",
    #"rh": "/home/kiridaust/Masters/Data/Era5/HumidLR/RHLR.nc",
    "pressure": "/home/kiridaust/Masters/Data/Era5/BuMo/PS_LR.nc",
    #"delta_ps": "/home/kiridaust/Masters/Data/wind_vars_full/PS_Diff_LR.nc",
    #"evap": "/home/kiridaust/Masters/Data/Era5/evap_raw.nc",
    }

coarse_out = {
    "train": "/home/kiridaust/Masters/Data/processed_data/BuMo/coarse_train.nc",
    "test": "/home/kiridaust/Masters/Data/processed_data/BuMo/coarse_test.nc",
    "val": "/home/kiridaust/Masters/Data/processed_data/BuMo/coarse_validation.nc"
    }

fine_covars = {
    #"precip": "/home/kiridaust/Masters/Data/wind_vars_full/PREC_HR.nc",
    #"rsds": "/home/kiridaust/Masters/Data/rsds/rsds_crop.nc",
    "u10": "/home/kiridaust/Masters/Data/wind_vars_full/U10_HR.nc",
    "v10": "/home/kiridaust/Masters/Data/wind_vars_full/V10_HR.nc",
    #"temp": "/home/kiridaust/Masters/Data/wind_vars_full/T2_HR.nc",
    #"q": "/home/kiridaust/Masters/Data/wind_vars_full/Q2_HR.nc",
    #"ps": "/home/kiridaust/Masters/Data/WRF/PSHR.nc",
    }


fine_out = {
    "train": "/home/kiridaust/Masters/Data/processed_data/BuMo/fine_train.nc",
    "test": "/home/kiridaust/Masters/Data/processed_data/BuMo/fine_test.nc",
    "val": "/home/kiridaust/Masters/Data/processed_data/BuMo/fine_validation.nc"
    }

train_region = [-128.5,-124.5,52.5,56.525]

if(fine):
    spatial_res = 1/(5*8)
    file_dict = fine_covars
    out_dict = fine_out
else:
    spatial_res = 1/5
    file_dict = coarse_covars
    out_dict = coarse_out
    
bounds = [train_region[0]+spatial_res/2,train_region[1]-spatial_res/2,train_region[2]+spatial_res/2,train_region[3]-spatial_res/2]
print("Bounds: ", bounds)
for i,covar in enumerate(file_dict.keys()):
    print("Processing ",covar)
    temp = nc.open_data(file_dict[covar])
    ##standardise names
    oldnm = temp.contents.variable[0]
    temp.rename({oldnm: covar})
    # if(covar == 'precip'):
    #     temp.top()
    #     temp.run()
    temp.subset(years = range(2001,2007)) #Just so we're not dealing with massive datasets
    #temp.cdo_command("setmissval,0")
    temp.to_latlon(lon = [bounds[0],bounds[1]], lat = [bounds[2],bounds[3]], res = [spatial_res,spatial_res])
    temp.run()
    #temp.cdo_command("setmissval,nan")
    temp.cdo_command("setmisstoc,0")
    temp.run()
    
    num_slices = int(temp.contents.ntimes)
    num_points = int(temp.contents.npoints)
    
    ##calculate mean and variance
    temp_mean = temp.copy()
    #calculate mean
    temp_mean.spatial_mean()
    temp_mean.tmean()
    temp_mean.run()

    #calculate variance - cdo will only summarise either spatially or temporally at once, so using formula to get overall variance
    temp_var = temp.copy()
    t3 = temp.copy()
    temp_var.spatial_var() #spatial variance
    temp_var.tsum() ##temporal sum of variance
    t3.spatial_mean() ##sparial mean
    t3.tvar() ##temporal variance of mean
    t3.multiply((num_points*(num_slices-1))/(num_points-1))
    temp_var.add(t3) ##combine them
    temp_var.multiply((num_points-1)/(num_points*num_slices-1)) ##this is now the variance
    temp_var.run()

    var_mean = float(temp_mean.to_xarray()[covar]) ##extract values
    var_var = np.sqrt(float(temp_var.to_xarray()[covar])) ##sqrt to get stdev
    ##standardise
    temp.subtract(var_mean)
    temp.divide(var_var)
    temp.run()

    if(i == 0):
        ds_out = temp.copy()
    else:
        ds_out.append(temp)

print("saving datasets")
ds_out.merge()
train = ds_out.copy()
##separate train and test
train.subset(years = [2002,2004,2006])
train.to_nc(out_dict["train"])
test = ds_out.copy()
test.subset(years = [2001,2003,2005])
test.to_nc(out_dict["test"])
ds_out.subset(years = 2001)
ds_out.to_nc(out_dict["val"])


#t2.assign(avg = lambda x: spatial_mean(x.tas), drop = True)


# cdo sub $IN -enlarge,$IN -timmean -fldmean $IN centred.nc
# cdo div centred.nc -enlarge,centred.nc -timmean -fldstd centred.nc $OUT
# gridtype  = lonlat
# gridsize  = 256
# xsize     = 16
# ysize     = 16
# xname     = longitude
# xlongname = "longitude"
# xunits    = "degrees_east"
# yname     = latitude
# ylongname = "latitude"
# yunits    = "degrees_north"
# xfirst    = 236.125
# xinc      = 0.25
# yfirst    = 52.875
# yinc      = -0.25
