# Functions to correct actual data in x and y
library(ggplot2)
correct.x <- function(x, y, data, weight=NULL){ 
  if(length(weight)<1) weight=.36
  # since .361 is the midpoint of the left and right estimates of optimal values.
  
  model <- smooth.spline(data[,x], data[,y], df=floor(length(unique(data[,x]))/2))

  f <- function(z) predict(model, x=z)$y
  fprime <- function(z) abs(predict(model, x=z, deriv=1)$y)
  a <- min(data[,x])
  b <- max(data[,x])
  
  const <- integrate(function(z) fprime(z), a, b, subdivisions=100*length(unique(data[,x])))$value
  trans <- sapply(data[,x], function(i) integrate(function(z) abs(fprime(z)), a, i, subdivisions=100*length(unique(data[,x])))$value*(b-a)/const + a)
  #   const <- sum(abs(fprime(data[,x])))
  #   trans <- cumsum(abs(fprime(data[,x])))*(b-a)/const+a                  
  
  data[,paste(x, "correctx", sep=".")] <- trans*weight + data[,x]*(1-weight)
  data[,paste(y, "fit", sep=".")] <- f(data[,x])
  
  data
}

correct.y <- function(x, y, data, weight=NULL){
  if(length(weight)<1) weight = .40
  # since .407 is the midpoint of the left and right estimates of optimal values.
  
  model <- smooth.spline(data[,x], data[,y], df=floor(length(unique(data[,x]))/2))
  f <- function(z) predict(model, x=z)$y
  fprime <- function(z) predict(model, x=z, deriv=1)$y
  f2prime <- function(z) predict(model, x=z, deriv=2)$y
  
  df <- data.frame(x=data[,x], y=data[,y], 
                   pred=f(data[,x]), 
                   deriv=fprime(data[,x]), 
                   deriv2 = f2prime(data[,x]))
  df$resid <- df$y - df$pred
  
  dy <- diff(range(df$y))
  dx <- diff(range(df$x))
  a <- dx/dy # aspect ratio : line "length" correction

  # linear correction
  df$ell <- abs(df$resid)
  data[,paste(y, "correcty", sep=".")] <- 
    df$pred + sign(df$resid)*(weight*(df$ell * sqrt(1 + a^2*df$deriv^2)) + (1-weight)*df$ell)
  data[,paste(y, "fit", sep=".")] <- f(data[,x])
  
#   # quadratic correction
#   fp <- a*df$deriv
#   f2p <- a*df$deriv2
#   v <- 1 + fp^2
#   lambdaminv <- 0.5*(sqrt(v^2+f2p*fp^2*df$ell) + v)
#   lambdapinv <- 0.5*(sqrt(v^2-f2p*fp^2*df$ell) + v)
#   data[,paste(y, "correcty", sep=".")] <- df$pred + sign(df$resid)*(
#     (df$resid>=0)*0.5*abs(lambdaminv)/sqrt(v) + 
#     (df$resid< 0)*0.5*abs(lambdapinv)/sqrt(v)
#     )
#   use one correction if neg. resid, another if positive
  
  data

}

correct <- function(x, y, data, type, weight=NULL){
  if(type=="x") correct.x(x, y, data, weight)
  else correct.y(x, y, data, weight)
}

# datasub <- read.csv("data/Ozone/Ozone-subset.csv")
# temp <- correct("Tmax", "Ozone", datasub, type="x")
# qplot(data=temp, x=Tmax.correctx, y=Ozone)
# temp2 <- correct("Tmax", "Ozone", datasub, type="y")
# qplot(data=temp2, x=Tmax, y=Ozone.correcty)
