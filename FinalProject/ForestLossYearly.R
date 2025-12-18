#install.packages("terra")
#install.packages("ggplot2")
library(ggplot2)   
library(terra)
library(dplyr)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", full.names = T)

#yangambi coordinates
yangambi_center <-c(24.45, 0.81)

#Region around coordinates
yangambi_region <- ext(
  yangambi_center[1] - 0.25,
  yangambi_center[1] + 0.25,
  yangambi_center[2] - 0.25,
  yangambi_center[2] + 0.25
)

#forest loss raster
lossyear <- rast(f[grep("lossyear", f)])
lossyear_crop <- crop(lossyear, yangambi_region)


#changing from lat/long
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
loss_binary_2001_2019 <- classify(lossyear_crop, rbind(
  c(0, 0, 0),    #no loss
  c(1, 19, 1),   #loss from 2001 to 2019
  c(20, 24, 0)   #ignore losses after 2019
))

rivers <- vect("C:/users/lilye/OneDrive/Documents/GEOG331/FinalProject/data/rwdb_rivers/rwdb_riv.shp")
crs(rivers) <- "EPSG:4326"
rivers_crop_ll <- crop(rivers, yangambi_region)
rivers_crop <- project(rivers_crop_ll, "EPSG:32635")

loss_binary_utm <- project(loss_binary_2001_2019, "EPSG:32635", method = "near")
loss_binary_factor <- as.factor(loss_binary_utm)

par(mar = c(5, 4, 4, 5)) #right margin

#plot raster
plot(loss_binary_utm, col = c("darkgreen", "white"), legend = FALSE)

#add rivers on top
lines(rivers_crop, col = "blue", lwd = 2)

#legend
legend(
  "right",
  inset = c(-0.10, 0),
  legend = c("No Loss", "Forest Loss", "Rivers"),
  fill   = c("darkgreen", "white", NA),
  border = c("black", "black", NA),
  lwd    = c(NA, NA, 2),
  col    = c(NA, NA, "blue"),
  bg = "white",
  cex = 1.2,
  xpd = TRUE
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
