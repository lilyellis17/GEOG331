#install packages
#install.packages("readxl")

#load packages
library(readxl)
library(dplyr)
library(ggplot2)

#read in climate  data
datH <- read_excel("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\Yangambi_daily_climate.xlsx")
head(datH)

datH$Date <- as.Date(with(datH, paste(Year, Month, day, sep = "-")), "%Y-%m-%d")

#filter for the data between 2000 and 2018
datYearFiltered <- datH %>%
  filter(Year >= 2000 & Year <= 2019)

#calculate the total precipitation by year
annual_precip <- datYearFiltered %>%
  group_by(Year) %>%
  summarise(total_precip = sum(Pm, na.rm = TRUE))

#create columns to break down dry and wet seasons
dat_season <- datYearFiltered %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2) ~ "Dec–Feb",
    Month %in% 3:11 ~ "Mar–Nov"
  ))

#change the year for december so it's for the next year's precipitation measure
dat_season <- dat_season %>%
  mutate(Season_Year = ifelse(Month == 12, Year + 1, Year))

#calculate the total precipitation by season
seasonal_precip <- dat_season %>%
  group_by(Season_Year, Season) %>%
  summarise(total_precip = sum(Pm, na.rm = TRUE)) %>%
  ungroup() %>%
  #exclude 2020 data since there is not full 2020 data
  filter(Season_Year <= 2019)

#Dry season analysis
ggplot(subset(seasonal_precip, Season == "Dec–Feb"),
       aes(x = Season_Year, y = total_precip)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Dec–Feb (Dry Season) Precipitation by Year (2001–2019) in Yangambi",
    x = "Year",
    y = "Total Precipitation (mm)"
  ) +
  #set the x axis to 2001 through 2019
  scale_x_continuous(limits = c(2001, 2019))

dry_precip <- seasonal_precip %>%
  filter(Season == "Dec–Feb")

fit_precip <- lm(total_precip ~ Season_Year, data = dry_precip)
summary(fit_precip)  # slope, p-value, R²

#scatter plot with regression line
plot(dry_precip$Season_Year, dry_precip$total_precip,
     pch = 19,
     xlab = "Year",
     ylab = "Dry Season Precipitation (mm)",
     main = "Dry-Season Precipitation Trend")
abline(fit_precip, col = "blue", lwd = 2)

#Residual plot
plot(dry_precip$Season_Year, residuals(fit_precip),
     pch = 19,
     xlab = "Year",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0)

#Residual histogram
hist(residuals(fit_precip), col = "lightblue",
     main = "Residual Distribution",
     xlab = "Residuals")

#QQ plot
qqnorm(residuals(fit_precip), pch = 19)
qqline(residuals(fit_precip))

#Shapiro-Wilk test for normality
shapiro.test(residuals(fit_precip))


#Wet season analysis

wet_precip <- seasonal_precip %>%
  filter(Season == "Mar–Nov")

#Wet season precipitation plot
ggplot(wet_precip,
       aes(x = Season_Year, y = total_precip)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "gray40", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Mar–Nov (Wet Season) Precipitation by Year (2000–2019) in Yangambi",
    x = "Year",
    y = "Total Precipitation (mm)"
  ) +
  scale_x_continuous(limits = c(2000, 2019))

#Linear regression for wet season
fit_wet <- lm(total_precip ~ Season_Year, data = wet_precip)
summary(fit_wet)

#Scatter plot with regression line
plot(wet_precip$Season_Year, wet_precip$total_precip,
     pch = 19,
     xlab = "Year",
     ylab = "Wet Season Precipitation (mm)",
     main = "Wet-Season Precipitation Trend")
abline(fit_wet, col = "darkgreen", lwd = 2)

#residual plot
plot(wet_precip$Season_Year, residuals(fit_wet),
     pch = 19,
     xlab = "Year",
     ylab = "Residuals",
     main = "Residual Plot (Wet Season)")
abline(h = 0)

#residual histogram
hist(residuals(fit_wet), col = "lightgreen",
     main = "Residual Distribution (Wet Season)",
     xlab = "Residuals")

#QQ plot
qqnorm(residuals(fit_wet), pch = 19)
qqline(residuals(fit_wet))

#Shapiro-Wilk test
shapiro.test(residuals(fit_wet))