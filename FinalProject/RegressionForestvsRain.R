library(dplyr)
library(ggplot2)
library(readxl)

library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data",
                full.names = TRUE)

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
zonal_loss <- zonal(loss_area_km2, lossyear_int, fun = "sum", na.rm = TRUE)

#df
loss_df <- as.data.frame(zonal_loss)
names(loss_df) <- c("loss_code", "area_km2")

loss_df <- loss_df %>% 
  filter(loss_code > 0) %>% 
  mutate(year = 2000 + loss_code) %>% 
  arrange(year)

loss_df_annual <- loss_df %>%
  filter(year >= 2001 & year <= 2019) %>%
  select(year, area_km2)

#climate data
datH <- read_excel("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\Yangambi_daily_climate.xlsx")

datH$Date <- as.Date(with(datH, paste(Year, Month, day, sep = "-")), "%Y-%m-%d")

# Filter for years used in forest-loss analysis
datYearFiltered <- datH %>%
  filter(Year >= 2000 & Year <= 2019)

# Assign seasons
dat_season <- datYearFiltered %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2) ~ "Dec–Feb",
    Month %in% 3:11 ~ "Mar–Nov"
  ))

# Make December count toward next year's season
dat_season <- dat_season %>%
  mutate(Season_Year = ifelse(Month == 12, Year + 1, Year))

# Summarize seasonal rainfall
seasonal_precip <- dat_season %>%
  group_by(Season_Year, Season) %>%
  summarise(total_precip = sum(Pm, na.rm = TRUE), .groups = "drop") %>%
  filter(Season_Year <= 2019)

# Extract dry-season rainfall only
dry_precip <- seasonal_precip %>%
  filter(Season == "Dec–Feb") %>%
  rename(year = Season_Year) %>%
  select(year, total_precip)

#merge datasets
regression_df <- merge(loss_df_annual, dry_precip, by = "year")

#regression analysis
fit <- lm(total_precip ~ area_km2, data = regression_df)
summary(fit)

#scatterplot of rain and forest loss
plot(regression_df$area_km2, regression_df$total_precip, pch = 19,
     xlab = "Forest Loss (km²)",
     ylab = "Dry Season Precipitation (mm)",
     main = "Dry-Season Rainfall vs. Forest Loss")
abline(fit, col = "red", lwd = 2)


#residual plot
plot(regression_df$area_km2, summary(fit)$residuals, pch = 19,
     xlab = "Forest Loss (km²)",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0)

#residual histogram
hist(summary(fit)$residuals, col = "purple",
     main = "Residual Distribution",
     xlab = "Residuals")

#QQ plot
qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals)

#Shapiro-Wilk test
shapiro.test(summary(fit)$residuals)

summary(fit)
