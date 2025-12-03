install.packages("terra")
install.packages("tidyterra")
install.packages("ggplot2")
library(ggplot2)   
library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", full.names = T)

f

#Crop the region
yangambi_center <-c(24.45, 0.81)

yangambi_region <- ext(
  yangambi_center[1] - 0.25,
  yangambi_center[1] + 0.25,
  yangambi_center[2] - 0.25,
  yangambi_center[2] + 0.25
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
  c(0, 0, 0),    #no loss
  c(1, 19, 1),   #loss from 2001 to 2019
  c(20, 24, 0)   #ignore losses after 2019
))

rivers <- vect(f[grep("rivers", f)])
crs(rivers) <- "EPSG:4326"
rivers_utm <- project(rivers, crs_utm)

loss_binary_factor <- as.factor(loss_binary_2001_2019)

ggplot() +
  geom_spatraster(data = loss_binary_factor) +
  scale_fill_manual(
    values = c("0" = "darkgreen", "1" = "white"),
    labels = c("No Loss", "Forest Loss"),
    name = "Forest Loss",
    guide = guide_legend(
      override.aes = list(color = "black")  # add black box around legend keys
    )
  ) +
  geom_spatvector(data = rivers_crop, color = "blue", size = 1, show.legend = FALSE) +  # no legend
  theme_minimal() +
  labs(
    title = "Deforestation 2001-2019 in Yangambi",
    x = "Easting (km)",
    y = "Northing (km)"
  ) +
  coord_sf(
    xlim = c(xmin(loss_binary_2001_2019), xmax(loss_binary_2001_2019)),
    ylim = c(ymin(loss_binary_2001_2019), ymax(loss_binary_2001_2019))
  ) +
  scale_x_continuous(labels = function(x) x / 1000) +
  scale_y_continuous(labels = function(y) y / 1000) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

#Regression between forest loss and year
loss_reg <- loss_df %>%
  filter(year >= 2001 & year <= 2019)

#linear regression
fit_loss <- lm(area_km2 ~ year, data = loss_reg)
summary(fit_loss)

#scatterplot
plot(loss_reg$year, loss_reg$area_km2,
     pch = 19,
     xlab = "Year",
     ylab = "Forest Loss (km²)",
     main = "Forest Loss Trend (2001–2019)")
abline(fit_loss, col = "blue", lwd = 2)

#residuals plot
plot(loss_reg$year, residuals(fit_loss),
     pch = 19,
     xlab = "Year",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0)

#residual histogram
hist(residuals(fit_loss), col = "green",
     main = "Residual Distribution",
     xlab = "Residuals")

#QQ plot
qqnorm(residuals(fit_loss), pch = 19)
qqline(residuals(fit_loss))


#Shapiro wilk test
shapiro.test(residuals(fit_loss))