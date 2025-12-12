How to use training and predicting code:

TRAINING:
1. (Optional) Open "input_data_prep.qmd" and run this to create PRISM input data, ocean proximity raster and extended rasters for predicting. Optional as all input data is on FFEC object storage (Mosaic_Yukon > Tirion > PRISM / ERA5 / dem). Filenames will match, but filepaths will not.

2. Open training_code_2025 folder, and select the variable/model file you want to run (all input data is standardized/transformed and tiled   in this file). These scripts were all run on thufir, so include the predicting code. Filenames will match, but filepaths will not.
- Foundational model scripts (<var>_fm4_train_pred.ipynb) are trained on all 12 months at once, but predict one month at a time.
- Specialization model scrips (<var_fm4_spec_train_pred.ipynb) are trained and predicted on one month.
- Note: For predicting, input files ending in "_pred.nc" are used to generate the training area, input files ending in "_US.nc" are used to generate the US portion. There is code to blend these two outputs in "output_data_prep.qmd".

3. Open "output_data_prep.qmd" and run this to create rasters from GAN outputs (.pt files). You will need to change the variable names throughout the files and make sure the filepaths are correct.

4. Open "tps_debias.R" and run this to debias the GAN output according to weather stations. You will need to change the variable names throughout the files and make sure the filepaths are correct.

5. which file is for leaflet maps?