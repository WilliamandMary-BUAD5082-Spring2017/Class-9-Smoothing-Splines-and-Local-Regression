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

