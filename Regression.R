# October 7th, 2025 in class activity regression

rm(list = ls())

# subeset the virgibia species
flower <- iris[iris$Species == "virginica",]

# see top 5 rows of iris dataset
head(iris)

plot(flower$Sepal.Length, flower$Petal.Length, pch =19,
     xlab = "Sepal Length", ylab = "Petal Length",
     main = "Iris Virginica")

fit <- lm(flower$Petal.Length ~ flower$Sepal.Length)

# plot residuals
plot(flower$Sepal.Length, summary(fit)$residuals, pch = 19,
     xlab= "Sepal Length", ylab = "Residuals")
abline(h = 0)

hist(summary(fit)$residuals, col = "purple",
     main = "Residual Distribution", xlab = "Residuals")

qqnorm(summary(fit)$residuals, pch = 19)
qqline(summary(fit)$residuals, pch = 19)

shapiro.test(summary(fit)$residuals)
