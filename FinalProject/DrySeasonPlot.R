#install packages
install.packages("readxl")

#load packages
library(readxl)
library(dplyr)
library(ggplot2)

#read in climate  data
datH <- read_excel("C:\\users\\lilye\\OneDrive\\Documents\\GEOG331\\FinalProject\\Yangambi_daily_climate.xlsx")
head(datH)

datH$Date <- as.Date(with(datH, paste(Year, Month, day, sep = "-")), "%Y-%m-%d")

#filter for teh data between 2000 and 2018
datYearFiltered <- datH %>%
  filter(Year >= 2000 & Year <= 2018)

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
  #exclude 2019 to 2020 data since there is not full 2020 data
  filter(Season_Year <= 2019)

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
