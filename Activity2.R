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
datW <- read.csv("Z:\\lmellis\\Data\\noaa_weather\\2011124.csv",
                 stringsAsFactors = T)
#get more information about the dataframe
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#google date formatting in r to find more options and learn more

#vector of characters
ch <- c("apple", "banana", "orange", "grape", "pear")

#vector of numerics
num <- c(12, 12.4, 0, 0.1, 100)

#vector of integers
ints <- c(1, 2, 3, 4, 5)

#vector of factor data
fac <- "clothes"(pants, shirt, dress, skirt, socks)