#PACKAGES
#use install.packages to install lubridate
#install.packages(c("lubridate"))
#it is helpful to comment this line after you run this line of code on the computer
#and the package installs. You really don't want to do this over and over again.

#Load package
library(lubridate)

assert <- function(statement,err.message){
  #if evaluates if a statement is true or false for a single item
  if(statement == FALSE){
    print(err.message)
  }
  
}

#check how the statement works
#evaluate a false statement
assert(1 == 2, "error: unequal values")

#evaluate a true statement
assert(2 == 2, "error: unequal values")
#set up assert to check if two vectors are the same length
a <- c(1,2,3,4)
b <- c(8,4,5)
assert(length(a) == length(b), "error: unequal length")

#QUESTION 3

#read in the data file
#skip the first 3 rows since there is additional column info
#specify the the NA is designated differently
#datW <- read.csv("Z:\\lmellis\\Data\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
datW <- read.csv("C:\\Users\\lilye\\OneDrive\\Desktop\\GEOG_331\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), skip=3, header=FALSE)
#preview data
print(datW[1,])

# get sensor info from file
# this data table will contain all relevant units
#sensorInfo <-   read.csv("Z:\\lmellis\\Data\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
sensorInfo <-   read.csv("C:\\Users\\lilye\\OneDrive\\Desktop\\GEOG_331\\bewkes\\bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

#get column names from sensorInfo table
# and set weather station colnames  to be the same
colnames(datW) <-   colnames(sensorInfo)
#preview data
print(datW[1,])

#WORKING WITH DATES
#convert to standardized format
#date format is m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calculations
datW[1,]

#CHECK MISSING DATA
#see how many values have missing data for each sensor observation
#air temperature
length(which(is.na(datW$air.temperature)))
#wind speed
length(which(is.na(datW$wind.speed)))
#precipitation
length(which(is.na(datW$precipitation)))
#soil temperature
length(which(is.na(datW$soil.moisture)))
#soil moisture
length(which(is.na(datW$soil.temp)))

#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#SETTING UP TESTS FOR QA/QC
#make a plot with filled in points (using pch)
#line lines
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")
#creating column for below freezing
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check the values at the extreme range of the data and throughout the percentiles
quantile(datW$air.tempQ1)

#look at days with really low air temperature
datW[datW$air.tempQ1 < 8,]  

#look at days with really high air temperature
datW[datW$air.tempQ1 > 33,]  

#HEAVY RAIN AND STRONG WIND
#plot precipitation and lightning strikes on the same plot
#normalize lighting strikes to match precipitation
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy
#make the plot with precipitation and lightning activity marked
#make it empty to start and add in features
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")
#plot precipitation points only when there is precipitation 
#make the points semi-transparent
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15)        

#plot lightning points only when there is lightning     
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#Question 5: test if the length of datW is the same at lightscale vector
assert(nrow(datW) == length(lightscale), "error: unequal length")

#filter out storms in wind and air temperature measurements
# filter all values with lightning that coincides with rainfall greater than 2mm or only rainfall over 5 mm.    
#create a new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))

# Question 6
datW$wind.speedQ1 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                            ifelse(datW$precipitation > 5, NA, datW$wind.speed))

assert(all(is.na(datW$wind.speedQ1[datW$precipitation >=2 & datW$lightning.acvitivy >0 ])), "error: precipitation at or above 2 with lightning activity not filtered out")
assert(all(is.na(datW$wind.speedQ1[datW$precipitation > 5])), "error: precipitation above 5 not filtered out")

plot(datW$DD, datW$wind.speedQ1, pch=19, type = "b", xlab = "Day of Year", ylab = "Wind Speed (Filtered)")

# Question 7
#adding a month column
datW$month<- month(dates)
july_data <- subset(datW, month ==7)

par(mfrow=c(2,2))
plot(july_data$DD, july_data$soil.moisture, pch=19, type="b", xlab="Day of Year", ylab="Soil Moisture")
plot(july_data$DD, july_data$soil.temp, pch=19, type="b", xlab="Day of Year", ylab="Soil Temperature")
plot(july_data$DD, july_data$precipitation, pch=19, type="b", xlab="Day of Year", ylab="Precipitation (Filtered)")
plot(july_data$DD, july_data$air.tempQ2, pch=19, type="b", xlab="Day of Year", ylab="Air Temp (Filtered)")

#reset the plotting layout
par(mfrow = c(1, 1))

# Question 8
#Getting all averages
avg_air_temp <- mean(datW$air.tempQ2, na.rm = TRUE)
avg_wind_speed <- mean(datW$wind.speedQ1, na.rm = TRUE)
avg_soil_moisture <- mean(datW$soil.moisture, na.rm = TRUE)
avg_soil_temp <- mean(datW$soil.temp, na.rm = TRUE)

#Getting sum of precipitation
sum_precip <- sum(datW$precipitation, na.rm = TRUE)

#Getting all of the observations used
count_air_temp <- sum(!is.na(datW$air.tempQ2))
count_wind_speed <- sum(!is.na(datW$wind.speedQ1))
count_soil_moisture <- sum(!is.na(datW$soil.moisture))
count_soil_temp <- sum(!is.na(datW$soil.temp))
count_precip <- sum(!is.na(datW$precipitation))

#Time period of study
start_date <- min(dates, na.rm = TRUE)
end_date   <- max(dates, na.rm = TRUE)

#TODO double check the sensor round (estimated right now)
research_table <- data.frame(
  Variable = c("Average Air Temp", "Average Wind Speed", "Average Soil Moisture", "Average Soil Temp", "Total Precipitation"),
  Measurement = c(round(avg_air_temp, 1), round(avg_wind_speed, 1), round(avg_soil_moisture, 3), round(avg_soil_temp, 1), round(sum_precip, 1)),
  Number_Of_Observations = c(count_air_temp, count_wind_speed, count_soil_moisture, count_soil_temp, count_precip)
)
print(research_table)
cat("\nTime Period of Measurements:", format(start_date, "%Y-%m-%d %H:%M"),
    "to", format(end_date, "%Y-%m-%d %H:%M"), "\n")

# Question 9
par(mfrow=c(2,2))
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab="Day of Year", ylab="Soil Moisture")
plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab="Day of Year", ylab="Soil Temperature")
plot(datW$DD, datW$precipitation, pch=19, type="b", xlab="Day of Year", ylab="Precipitation (Filtered)")
plot(datW$DD, datW$air.tempQ2, pch=19, type="b", xlab="Day of Year", ylab="Air Temp (Filtered)")

