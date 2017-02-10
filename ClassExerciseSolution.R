# In Class Excercise
# Smoothing Spline

rm(list=ls())
library(splines)
# Set seed to 1
set.seed(1)

# Create the vector y by random 200 numbers from 1 to 20 
# and create the vector x as sequence generation from 1 to the lenght 
# of y for 201 numbers.
y <- c(runif(200,1,20)) 
x <- seq(1, length(y), len = 201)

# Plot y
plot(y, col.main = 2)

# Fit smoothing spline with cross-validation and line plot it with red color
(s <- smooth.spline(y, cv = TRUE))
lines(s,col="red",lwd=2)

# Fit smoothing spline with degree of freedom = 5 and line plot it with red color
(s1 <- smooth.spline(y, df = 5))
lines(s1,col="blue",lwd=2)
legend("bottomright",legend=c("5 DF","2 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

