install.packages("terra")
install.packages("tidyterra")
install.packages("ggplot2")
library(ggplot2)   
library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", full.names = T)

f

yangambi_center <-c(24.45, 0.81)

yangambi_region <- ext(
  yangambi_center[1] - 0.25,  # xmin
  yangambi_center[1] + 0.25,  # xmax
  yangambi_center[2] - 0.25,  # ymin
  yangambi_center[2] + 0.25   # ymax
)

#forest loss raster
lossyear <- rast(f[grep("lossyear", f)])

lossyear_crop <- crop(lossyear, yangambi_region)


#Changing from lat/long to meters system
crs_utm <- "EPSG:32635"
lossyear_utm <- project(lossyear_crop, crs_utm, method = "near")
lossyear_int <- round(lossyear_utm)

#pixel area (in km^2)
loss_area_km2 <- cellSize(lossyear_utm, unit = "km")

#total forest loss per loss code (0–24) (year)
zonal_loss <- zonal(loss_area_km2, lossyear_int, fun = "sum", na.rm = TRUE)

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
ggplot(loss_df[loss_df$year >= 2001 & loss_df$year <= 2019,],
       aes(x = year, y = area_km2)) +
  geom_col(fill = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Annual Forest Loss in Yangambi Region (2001–2019)",
    x = "Year",
    y = "Forest Area Lost (km²)"
  )

#raster of deforestation occurring in any year rather than by year
# forest loss raster for 2001-2019 only
loss_binary_2001_2019 <- classify(lossyear_utm, rbind(
  c(0, 0, 0),    # no loss
  c(1, 19, 1),   # loss from 2001 to 2019
  c(20, 24, 0)   # ignore losses after 2019
))

#forest loss
cols <- c("darkgreen", "white")
plot(
  loss_binary,
  col = cols,
  legend = FALSE,
  axes = TRUE,
  main = "Deforestation 2001-2019 in Yangambi",
)
legend("right",
       legend = c("No Loss", "Forest Loss"),
       fill = c("darkgreen", "white"),
       border = "black",
       bty = "n")
