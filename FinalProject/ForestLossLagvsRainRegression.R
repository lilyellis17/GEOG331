#Load libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(terra)
library(tidyterra)

#Load forest loss data
f <- list.files("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\data", 
                full.names = TRUE)

#define Yangambi region
yangambi_center <- c(24.45, 0.81)
yangambi_region <- ext(
  yangambi_center[1] - 0.25, yangambi_center[1] + 0.25, 
  yangambi_center[2] - 0.25, yangambi_center[2] + 0.25
)

#load and crop forest loss raster
lossyear <- rast(f[grep("lossyear", f)])
lossyear_crop <- crop(lossyear, yangambi_region)

#Change projection to UTM
crs_utm <- "EPSG:32635"
lossyear_utm <- project(lossyear_crop, crs_utm, method = "near")
lossyear_int <- round(lossyear_utm)

#Calculate pixel area in km^2
loss_area_km2 <- cellSize(lossyear_utm, unit = "km")

#Zonal sum of forest loss
zonal_loss <- zonal(loss_area_km2, lossyear_int, fun = "sum", na.rm = TRUE)

#convert to dataframe
loss_df <- as.data.frame(zonal_loss)
names(loss_df) <- c("loss_code", "area_km2")

#filter and prepare yearly forest loss data
loss_df <- loss_df %>%
  filter(loss_code > 0) %>%
  mutate(year = 2000 + loss_code) %>%
  arrange(year)

loss_df_annual <- loss_df %>%
  filter(year >= 2001 & year <= 2019) %>%
  select(year, area_km2)

#load climate data
datH <- read_excel("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\Yangambi_daily_climate.xlsx")
datH$Date <- as.Date(with(datH, paste(Year, Month, day, sep = "-")), "%Y-%m-%d")

#filter for years 2000-2019
datYearFiltered <- datH %>% filter(Year >= 2000 & Year <= 2019)

# distinguish seasons
dat_season <- datYearFiltered %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2) ~ "Dec–Feb",
    Month %in% 3:11 ~ "Mar–Nov"
  ))

# Make December count toward next year's season
dat_season <- dat_season %>%
  mutate(Season_Year = ifelse(Month == 12, Year + 1, Year))

#seasonal rainfall
seasonal_precip <- dat_season %>%
  group_by(Season_Year, Season) %>%
  summarise(total_precip = sum(Pm, na.rm = TRUE), .groups = "drop") %>%
  filter(Season_Year <= 2019)

#extract dry-season rainfall
dry_precip <- seasonal_precip %>%
  filter(Season == "Dec–Feb") %>%
  rename(year = Season_Year) %>%
  select(year, total_precip)

#Merge forest loss and dry-season rainfall
regression_df <- merge(loss_df_annual, dry_precip, by = "year")

#create 1-year lag of rainfall
regression_df <- regression_df %>%
  mutate(precip_lag1 = dplyr::lag(total_precip, n = 1)) %>%
  filter(!is.na(precip_lag1))

# Linear regression of forest loss ~ 1-year lagged rainfall
fit_reverse <- lm(area_km2 ~ precip_lag1, data = regression_df)
summary(fit_reverse)

#forest loss vs. lagged dry-season rainfall graph
plot(regression_df$precip_lag1, regression_df$area_km2, 
     pch = 19, 
     xlab = "Dry Season Precipitation (mm), 1 Year Lag", 
     ylab = "Forest Loss (km²)", 
     main = "Forest Loss vs. 1-Year Lagged Dry-Season Rainfall")
abline(fit_reverse, col = "blue", lwd = 2)

#residual plot
plot(regression_df$precip_lag1, residuals(fit_reverse), 
     pch = 19, 
     xlab = "Dry Season Precipitation (mm), 1 Year Lag", 
     ylab = "Residuals", 
     main = "Residual Plot")
abline(h = 0)

#Residual histogram
hist(residuals(fit_reverse), col = "green", 
     main = "Residual Distribution", 
     xlab = "Residuals")

#QQ plot
qqnorm(residuals(fit_reverse), pch = 19)
qqline(residuals(fit_reverse))

#Shapiro-Wilk test for normality
shapiro.test(residuals(fit_reverse))
