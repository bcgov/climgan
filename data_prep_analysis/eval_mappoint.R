# plot seasonal cycle of specific points

library(terra)
library(tidyr)
library(dplyr)
library(ggplot2)

#------ 2025 Files -------

files1 <- file.path("O:/Mosaic_Yukon/Tirion/Results/foundational_model/tmax/Model1", tolower(month.abb), paste0(tolower(month.abb), "_fullregion_masked.nc"))

r1 <- rast(files1)

#------ 2024 Files -------

files2 <- sapply(tolower(month.abb), function(m) {
  list.files(
    file.path("O:/Mosaic_Yukon/operational/WorldClim/tmax", m, "Predictions"),
    pattern = "\\.nc$",
    full.names = TRUE
  )
})

r2 <- rast(files2)

#------ extract points -------

# specify point(s)
pts <- data.frame(
  lon = -124.33,
  lat = 63.61
)

# extract values at the given point
vals1 <- terra::extract(r1, pts[, c("lon", "lat")])[,-1]
vals2 <- terra::extract(r2, pts[, c("lon", "lat")])[,-1]

names(vals1) <- month.abb
names(vals2) <- month.abb

#------ plot -------

vals1 <- cbind(pts, vals1) %>%
  pivot_longer(cols = month.abb,
               names_to = "month", values_to = "value") %>%
  mutate(month = factor(month,
                        levels = month.abb),
         dataset = "GAN 2025")

vals2 <- cbind(pts, vals2) %>%
  pivot_longer(cols = month.abb,
               names_to = "month", values_to = "value") %>%
  mutate(month = factor(month,
                        levels = month.abb),
         dataset = "GAN 2024")

vals_long <- bind_rows(vals1, vals2)

ggplot(vals_long, aes(x = month, y = value, color = dataset, group = dataset)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Monthly values at selected point",
       x = "Month", y = "Value") +
  theme_minimal()


