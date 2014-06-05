library(datasets)
data(iris)
# what is the mean of 'Sepal.Length' for the species virginica?
apply(subset(iris, Species == "virginica", select = c(Sepal.Length)), 2, mean )
# what R code returns a vector of the means of the variables 'Sepal.Length',
# 'Sepal.Width', 'Petal.Length', and 'Petal.Width'?
apply(iris[, 1:4], 2, mean)
data(mtcars)
# How can one calculate the average miles per gallon (mpg) by number of 
# cylinders in the car (cyl)?
with(mtcars, tapply(mpg, cyl, mean))
# what is the absolute difference between the average horsepower of 4-cylinder 
# cars and the average horsepower of 8-cylinder cars?
abs(mean(mtcars$hp[mtcars$cyl == 4]) - mean(mtcars$hp[mtcars$cyl == 8]))