install.packages("terra")
install.packages("tidyterra")
install.packages("ggplot2")
library(ggplot2)   
library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", full.names = T)

f

yangambi_center <- c(24.45, 0.81)

extent_yangambi <- ext(
  yangambi_center[1] - 0.25,  # xmin
  yangambi_center[1] + 0.25,  # xmax
  yangambi_center[2] - 0.25,  # ymin
  yangambi_center[2] + 0.25   # ymax
)

#forest loss raster
lossyear <- rast(f[grep("lossyear", f)])
#cut raster to yangambi area
lossyear_y <- crop(lossyear, extent_yangambi)

#pixel area (in km^2)
loss_area_km2 <- cellSize(lossyear_y, unit = "km")

#total forest loss per loss code (0–24) (year)
zonal_loss <- zonal(loss_area_km2, lossyear_y, fun = "sum", na.rm = TRUE)

#df
loss_df <- as.data.frame(zonal_loss)
names(loss_df) <- c("loss_code", "area_km2")

#remove no loss pixels
loss_df <- subset(loss_df, loss_code > 0)

#convert to years
loss_df$year <- 2000 + loss_df$loss_code

#sort by year column
loss_df <- loss_df[order(loss_df$year), ]

#annual forest loss graph
ggplot(loss_df[loss_df$year>=2001 & loss_df$year<=2019,], aes(x = year, y = area_km2)) +
  geom_col(fill = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Annual Forest Loss in Yangambi Region (2001–2019)",
    x = "Year",
    y = "Forest Area Lost (Km^2)"
  )

#forest loss by year
cols <- c("gray", hcl.colors(24, "Inferno", rev = TRUE))
plot(
  lossyear_y,
  col = cols,
  breaks = 0:24,
  main = "Forest Loss Year (2001–2024) - Yangambi",
  legend = TRUE,
  axes = TRUE
)

loss_df
