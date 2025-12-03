library(dplyr)
library(readxl)
library(terra)
library(tidyterra)

f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data",
                full.names = TRUE)

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


#changing from lat/long to meters system
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

#Filter for years used in forest-loss analysis
datYearFiltered <- datH %>%
  filter(Year >= 2000 & Year <= 2020)

#assign seasons
dat_season <- datYearFiltered %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2) ~ "Dec–Feb",
    Month %in% 3:11 ~ "Mar–Nov"
  ))

#make December count toward next year's season
dat_season <- dat_season %>%
  mutate(Season_Year = ifelse(Month == 12, Year + 1, Year))

#Summarize seasonal rainfall
seasonal_precip <- dat_season %>%
  group_by(Season_Year, Season) %>%
  summarise(total_precip = sum(Pm, na.rm = TRUE), .groups = "drop") %>%
  filter(Season_Year <= 2020)

#extract dry-season rainfall only
dry_precip <- seasonal_precip %>%
  filter(Season == "Dec–Feb") %>%
  rename(year = Season_Year) %>%
  select(year, total_precip)

#merge datasets
regression_df <- merge(loss_df_annual, dry_precip, by = "year")

regression_df <- regression_df %>%
  arrange(year) %>%
  mutate(loss_lag2 = dplyr::lag(area_km2, n = 2))

regression_df <- regression_df %>% 
  filter(!is.na(loss_lag2))

#regression analysis
fit_lag2 <- lm(total_precip ~ loss_lag2, data = regression_df)
summary(fit_lag2)

#scatterplot of rain and forest loss
plot(regression_df$loss_lag2, regression_df$total_precip, pch = 19,
     xlab = "Forest Loss (km²), 2 Year Lag",
     ylab = "Dry Season Precipitation (mm)",
     main = "Dry-Season Rainfall vs. 2 year Forest Loss Lag")
abline(fit_lag2, col = "red", lwd = 2)


#residual plot
plot(regression_df$loss_lag2, residuals(fit_lag2), pch = 19,
     xlab = "Forest Loss (km²), 2 Year Lag",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0)

#residual histogram
hist(summary(fit_lag2)$residuals, col = "purple",
     main = "Residual Distribution",
     xlab = "Residuals")

#QQ plot
qqnorm(summary(fit_lag2)$residuals, pch = 19)
qqline(summary(fit_lag2)$residuals)

#Shapiro-Wilk test
shapiro.test(summary(fit_lag2)$residuals)
