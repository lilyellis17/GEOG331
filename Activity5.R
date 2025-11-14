#load in lubridate
#library(lubridate)

#read in streamflow data
datH <- read.csv("Z:\\lmellis\\Data\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)        

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\lmellis\\Data\\hw5_data\\2049867.csv")                            
print(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))  

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#QUESTION 3
#Getting info for streamflow data
str(datH)
colnames(datH)

#Getting info for precipitation data
str(datD)
colnames(datD)

#QUESTION 4
help(expression)

#QUESTIONS 5 & 6

aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#2017 data vector
dat2017 <- subset(datD, year == 2017)
ave2017 <- aggregate(dat2017$discharge, by=list(dat2017$doy), FUN="mean")
colnames(ave2017) <- c("doy","dailyAve")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
) 
lines(ave2017$doy, ave2017$dailyAve, col="green", lwd=2)
axis(1, seq(15, 345, by = 30), #tick intervals
     lab=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation", "2017 data"), #legend items
       lwd=c(2,NA, 2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "green"),#colors
       pch=c(NA,15, NA),#symbols
       bty="n")#no legend border

#QUESTION 7
#figure out how many observations there are for each date
counts <- aggregate(datP$HPCP, by=list(as.Date(dateP)), FUN=length)
colnames(counts) <- c("date","n_obs")
#boolean value, true if 24 observations on that day, false if not
counts$full24 <- counts$n_obs == 24

#Get the average discharge per day to graph
aveDischargePerDay <- aggregate(datD$discharge, by=list(datesD), FUN="mean")

#make plot
ggplot()+
  geom_line(data = aveDischargePerDay, mapping = aes(x = Group.1, y = x))+
  geom_point(
    data = aveDischargePerDay[counts$full24 == TRUE, ],
    aes(x = Group.1, y = x, color = "Data for full 24 hours")
  ) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  scale_color_manual(
    name = "Legend",
    values = c("Data for full 24 hours" = "red")
  ) +
  labs(x = "Average Daily Discharge", y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), title = "Daily Discharge")


#QUESTION 8

#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#Set days of year to look at to 32 through 34
hydroD <- datD[datD$doy >= 32 & datD$doy < 34 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 32 & datP$doy < 34 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}

#QUESTION 9
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#Define the seasons
datD$season <- ifelse(datD$doy >= 354 | datD$doy < 80, "Winter",
                      ifelse(datD$doy >= 80 & datD$doy < 172, "Spring",
                      ifelse(datD$doy >= 172 & datD$doy < 265, "Summer",
                             "Fall")))
#make a factor
datD$season <- factor(datD$season,
                      levels = c("Winter", "Spring", "Summer", "Fall"))

#Create violin plot for 2017
datD %>% 
  filter(year == 2017) %>%
  ggplot(aes(season, discharge)) +
  geom_violin() +
  labs(x = ("Season"), y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), title = ("Discharge by Season for 2017"))

#create violin plot for 2016
datD %>% 
  filter(year == 2016) %>%
  ggplot(aes(season, discharge)) +
  geom_violin() +
  labs(x = ("Season"), y = expression(paste("Discharge ft"^"3 ","sec"^"-1")), title = ("Discharge by Season for 2016"))