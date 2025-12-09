#install.packages("terra")
#install.packages("tidyterra")
#install.packages("ggplot2")
library(ggplot2)   
library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", full.names = T)

f

#Crop the region
center <- vect("POINT(24.45 0.81)", crs="EPSG:4326")
center_utm <- project(center, "EPSG:32635")

#Extract numeric coordinates (meters)
cx <- crds(center_utm)[1]
cy <- crds(center_utm)[2]

#30 km radius
radius <- 30000

yangambi_region <- ext(
  cx - radius,
  cx + radius,
  cy - radius,
  cy + radius
)


#forest loss raster
lossyear <- rast(f[grep("lossyear", f)])


#project raster
crs_utm <- "EPSG:32635"
lossyear_utm <- project(lossyear, crs_utm, method="near")

#crop using the UTM extent
lossyear_crop <- crop(lossyear_utm, yangambi_region)

lossyear_int <- round(lossyear_crop)

#pixel area (in km^2)
loss_area_km2 <- cellSize(lossyear_crop, unit = "km")

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
rivers_utm <- project(rivers, "EPSG:32635")

# crop in UTM (this was missing before!)
rivers_crop <- crop(rivers_utm, yangambi_region)

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
    xlim = c(xmin(yangambi_region), xmax(yangambi_region)),
    ylim = c(ymin(yangambi_region), ymax(yangambi_region)),
    expand = FALSE
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

