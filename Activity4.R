#use built in iris dataset
#take a look at it 
head(iris)
#install.packages("dplyr")
#install.packages("ggplot2")
#load in some tidyverse packages
library(tidyverse)
library(ggplot2)
library(dplyr)
rm(list = ls())
#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables
#Creating vector with just the versicolor species
flower <- iris[iris$Species == "versicolor",]

#Creating a list of all regressions to be done
regression_list <- list(flower$Sepal.Length ~ flower$Sepal.Width, 
                        flower$Petal.Length ~ flower$Petal.Width, 
                        flower$Sepal.Length ~ flower$Petal.Length)

#List object to store the regression fits
fit <- list()
#List object to store the summaries of each regression
summaries <- list()
#Count object for indexing
count <- 0
for(item in regression_list){
  count <- count + 1
  fit[[count]] <- lm(item)
  summaries[[count]] <- summary(fit[[count]])
}
print(summaries)

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))
#New data frame with inner join between iris and height dataframes on the Species value
iris2 <- inner_join(iris, height, by="Species", copy = FALSE, suffix = c(".x", ".y"),)
print(iris2)

#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(shape = 1, fill = "white")

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(shape = 1, fill = "white") + theme_classic()

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
min_length <- min(iris$Petal.Length)
max_length <- max(iris$Petal.Length)
ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, size = Petal.Length, color = Species)) + geom_point(shape = 19) + scale_size(range = c(min_length, max_length)) + theme_classic()+ labs(x = "Sepal Length", y = "Sepal Width", title = "Sepal Length vs Width")


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		
#The arguments for plot are first the x and y coordinates. You don't have to do anything else to get a base scatterplot.
#With ggplot, you put in the source of the data, then the x and y coordinates as your parameters.
#However, with ggplot, you wont get any sort of graph until you put further specifications in following the ggplot arguments.
#You need to specify the type of graph following the ggplot() arguments (for example, geom_point())