
# Smoothing Spline and Local Regression Lab

# Smoothing Spline

##-- artificial example (outside textbook)
rm(list = ls())
# To make better understand about the lambda, We will go over the argument named "spar" with
# scaled the lambda from 0 to 1. We use the splines library for smoothing spline in R.
library(splines)
y18 <- c(1:3, 5, 4, 7:3, 2*(2:5), rep(10, 4)) # Create the data set
xx  <- seq(1, length(y18), len = 201) # Create values for y18 at which we want predictions
# we fit the smoothing spline with y18 itself and using the cross-validation
(s3   <- smooth.spline(y18, cv = TRUE))
plot(y18, col.main = 2, ann = FALSE)
title("Smoothing Splines: Lambda Values")
lines(s3, col = "red", lwd = 2.5)
# Using the spar to simplify different scaled lambda
(s01  <- smooth.spline(y18, spar = 0.1))
(s02  <- smooth.spline(y18, spar = 1))
lines(predict(s01, xx), col = "darkgreen", lwd = 2.5)
lines(predict(s02, xx), col = 4, lwd = 2.5)
#legend(12,4,c("lambda = fitted","lambda = 0", "lambda approaches infinity"),col=c("red","green","blue"))
legend("bottomright", c("lambda = fitted","lambda = 0", "lambda approaches infinity"), lty=c(1,1), lwd=c(2.5,2.5),col=c("red", "darkgreen", "blue"))
# If spar approach to 0, the graph is perfectly fit
# but if spar approach to 1, the graph is straight line

# Lab from the text book
# Smoothing Splines
rm(list=ls())
# In order to fit smoothing splines in R, we use the splines library. Then we attach wage data
# and create a grid of values for age at which we want predictions
library(splines)
library(ISLR)
attach(Wage)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
# In order to fit a smoothing spline, we use the smooth.spline() function.
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
# Try different df
fit3=smooth.spline(age,wage,df=2)
lines(fit3,col="green",lwd=2)
fit4=smooth.spline(age,wage,df=60)
lines(fit4,col="black",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF","2 DF","60 DF"),col=c("red","blue","green","black"),lty=1,lwd=2,cex=.8)
# Notice that in the first call to smooth.spline(), we specified df=16. The
# function then determines which value of λ leads to 16 degrees of freedom. In
# the second call to smooth.spline(), we select the smoothness level by crossvalidation;
# this results in a value of λ that yields 6.8 degrees of freedom.


# Local Regression

#http://research.stowers-institute.org/efg/R/Statistics/loess.htm
#Stowers Institute for Medical Research
y <- rnorm(1000)
x <- 1:1000
out.lowess <- lowess(x,y,f=0.3,iter=3,delta=0)
out.lowess$y[1:5]
out.loess <- loess(y~x,span=0.3,degree=1,family="symmetric",iterations=4,surface="direct")
fitted(out.loess)[1:5]

?lowess
?loess 

#1. Crete a sine curve and add some noise:
period <- 120
x <- 1:120
y <- sin(2*pi*x/period) + runif(length(x),-1,1)
#2. Plot the points on this noisy sine curve:
plot(x,y, main="Sine Curve + 'Uniform' Noise")
mtext("showing loess smoothing (local regression smoothing)")

#3. Apply loess smoothing using the default span value of 0.75:
y.loess <- loess(y ~ x, span=0.75, data.frame(x=x, y=y))

#4. Compute loess smoothed values for all points along the curve:
y.predict <- predict(y.loess, data.frame(x=x))
length(y.predict)
#5. Plot the loess smoothed curve along with the points that were already plotted:
lines(x,y.predict)

#6. Let's use the R "optimize" function to find the peak of the loess smoothed curve and plot that point:
peak <- optimize(function(x, model) predict(model, data.frame(x=x)),c(min(x),max(x)),maximum=TRUE,model=y.loess) 
points(peak$maximum,peak$objective,pch=FILLED.CIRCLE<-19)
#line to be red

trough <- optimize(function(x, model) predict(model, data.frame(x=x)),c(min(x),max(x)),maximum=FALSE,model=y.loess) 
points(trough$minimum ,trough$objective,pch=FILLED.CIRCLE<-19)

######Same thing but with more span values
set.seed(19)
period <- 120
x <- 1:120
y <- sin(2*pi*x/period) + runif(length(x),-1,1)
plot(x,y, main="Sine Curve + 'Uniform' Noise")
mtext("showing loess smoothing (local regression smoothing)")
spanlist <- c(0.10, 2.00) 
for (i in 1:length(spanlist))
{
  y.loess <- loess(y ~ x, span=spanlist[i], data.frame(x=x, y=y))
  y.predict <- predict(y.loess, data.frame(x=x))
  lines(x,y.predict,col=i)
  peak <- optimize(function(x, model)
    predict(model, data.frame(x=x)), c(min(x),max(x)),maximum=TRUE,model=y.loess)
  points(peak$maximum,peak$objective, pch=FILLED.CIRCLE<-19, col=i)
  trough <- optimize(function(x, model) predict(model, data.frame(x=x)),c(min(x),max(x)),maximum=FALSE,model=y.loess) 
  points(trough$minimum ,trough$objective,pch=FILLED.CIRCLE<-19)
}
legend (0,-0.8, c(paste("span=", formatC(spanlist, digits=2, format="f"))), lty=SOLID<-1, col=1:length(spanlist), bty="n")


##### Practice from the book using wage data 
library(ISLR)
attach(Wage)
agelims =range(age)
age.grid=seq(from=agelims [1],to=agelims [2])
plot(age ,wage ,xlim=agelims ,cex =.5,col=" darkgrey ")
title("Local Regression ")
fit=loess(wage~age ,span=.2,data=Wage)
fit2=loess(wage~age ,span=.5,data=Wage)
lines(age.grid , predict (fit ,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),
      col="blue",lwd=2)
legend ("topright",legend=c("Span=0.2"," Span=0.5"),
        col=c("red","blue"),lty=1,lwd=2, cex =.8)



# In Class Excercise

# Smoothing Spline 

rm(list=ls())
library(splines)
# Set seed to 1


# Create the vector y by random 200 numbers from 1 to 20 
# and create the vector x as sequence generation from 1 to the lenght 
# of y for 201 numbers.


# Plot y


# Fit smoothing spline with cross-validation and line plot it with red color


# Fit smoothing spline with degree of freedom = 5 and line plot it with red color

