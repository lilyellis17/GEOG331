#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
#look at the first tree height
heights[1]
#look at the 2nd and 3rd tree heights
heights[2:3]

#get more info on the matrix function
help(matrix)

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]

#look at all values in row 1
Mat.bycol[1,]

#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder
#TODO Change the following line
#datW <- read.csv("Z:\\lmellis\\Data\\noaa_weather\\2011124.csv",
#                 stringsAsFactors = T)
datW <- read.csv("C:\\Users\\lilye\\Desktop\\GEOG 331\\noaa_weather\\2011124.csv", stringsAsFactors = T )

#get more information about the dataframe
str(datW)

#specify a column with a proper date format
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#Question 2
#vector of characters
ch <- c("apple", "banana", "orange", "grape", "pear")

#vector of numerics
num <- c(12, 12.4, 0, 0.1, 100)

#vector of integers
ints <- c(1, 2, 3, 4, 5)

#vector of factor data
clothes_vector <- c("pants", "shirt", "dress", "skirt", "socks")
clothes_factor <- factor(clothes_vector)

#find out all unique site names
unique(datW$NAME)

#look at the mean maximum temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

#look at the mean maximum temperature for Aberdeen
#with na.rm argument set to true to ingnore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

#convert level to number for factor data type
#you will have to reference the level output or look at the row of data to see the character designation.
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Figure out what the arguments in hist mean
help(hist)
help(paste)

#make a new histogram for the first site in our levels, Aberdeen. Add in SD and mean.
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


#Question 4
#Adding histograms to same window
par(mfrow=c(2,2))
#histogram site 2
h2 <- hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram for site 3
h3 <- hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#histogram for site 4
h4 <- hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#Histogram site 5
h5 <- hist(datW$TAVE[datW$siteN == 5],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
#reset the plotting layout
par(mfrow = c(1, 1))
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
par(mfrow=c(2,2))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Question 5 (regraphing)
# Plot 1 with a distribution curve
h1 <- hist(datW$TAVE[datW$siteN == 2],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[2]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x1.plot <- seq(-10,30, length.out = 100)

#the dnorm function will produce the probability density based on a mean and standard deviation.
y1.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y1.scaled <- (max(h2$density)/max(y1.plot)) * y1.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.
points(x1.plot,
       y1.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Plot 2 with a distribution curve
h2 <- hist(datW$TAVE[datW$siteN == 3],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[3]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x2.plot <- seq(-10,30, length.out = 100)

#the dnorm function will produce the probability density based on a mean and standard deviation.
y2.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE))

#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y2.scaled <- (max(h2$density)/max(y2.plot)) * y2.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.
points(x2.plot,
       y2.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Plot 3 with a distribution curve
h3 <- hist(datW$TAVE[datW$siteN == 4],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[4]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x3.plot <- seq(-10,30, length.out = 100)

#the dnorm function will produce the probability density based on a mean and standard deviation.
y3.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE))

#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y3.scaled <- (max(h3$density)/max(y3.plot)) * y3.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.
points(x3.plot,
       y3.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

# Plot 4 with a distribution curve
h4 <- hist(datW$TAVE[datW$siteN == 5],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[5]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x4.plot <- seq(-10,30, length.out = 100)

#the dnorm function will produce the probability density based on a mean and standard deviation.
y4.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 5],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 5],na.rm=TRUE))

#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
y4.scaled <- (max(h4$density)/max(y4.plot)) * y4.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.
points(x4.plot,
       y4.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)

#For documentation on the normal distribution
help(dnorm)

#Calculating probability of below 0 temperatures for site 1
#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Probablility of temperatures below 5 for site 1
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#probability of temperatures between 0 and 5
#pnrom with 5 gives me all probability (area of the curve) below 5 
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#probability of temperatures above 20 at site 1
#pnorm of 20 gives me all probability (area of the curve) below 20 
#subtracting from one leaves me with the area above 20
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm value of 0.95 at site 1
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
qnorm(0.05,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#question 6; if mean temp goes up by 4 but SD stays the same, how often will we see extreme high temps
#temps above 18.51026
1 - pnorm(18.51026,
          (mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE))+4,
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#Question 7
#Histogram of daily precipitation for site 1
par(mfrow = c(1, 1))
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily Precipitation (Inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 8 
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
datW$year <- format(datW$dateF, "%Y")
sumPrecip <- aggregate(
  x = datW$PRCP,
  by = list(Site = datW$NAME, Year = datW$year),
  FUN = sum,
  na.rm = TRUE
)
print(sumPrecip, row.names = FALSE)
#Histogram for site 5
site5_annual <- sumPrecip$'x'[sumPrecip$Site == (datW$NAME[5])]
hist(site5_annual,
     freq=FALSE, 
     main = paste(levels(datW$NAME)[5]),
     xlab = "Annual Precipitation (Inches)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#Question 9: annual mean precipitation
averagePRCP <- aggregate(datW$PRCP, by=list(datW$NAME), FUN = "mean", na.rm=TRUE)
averagePRCP
averageTemp