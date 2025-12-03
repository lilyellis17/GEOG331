library(dplyr)
library(ggplot2)
library(readxl)

#climate data
datH <- read_excel("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\Yangambi_daily_climate.xlsx")

datH$Date <- as.Date(with(datH, paste(Year, Month, day, sep = "-")), "%Y-%m-%d")

#Filter for years used in forest-loss analysis
datYearFiltered <- datH %>%
  filter(Year >= 2000 & Year <= 2019)

#get the variation of rain using standard deviation by year
daily_var <- datYearFiltered %>%
  group_by(Year) %>%
  summarise(
    sd_daily_rain = sd(Pm, na.rm = TRUE)
  )

#Plot the intra annual standard deviation by year
ggplot(daily_var, aes(x = Year, y = sd_daily_rain)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  labs(
    title = "Intra-Annual Rainfall Variability",
    y = "Standard Deviation of Daily Rainfall (mm)",
    x = "Year"
  )