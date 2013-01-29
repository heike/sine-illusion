f <- function(x) sin(x)
fprime <- function (x) cos(x)

createSine <- function(n=200, len=1, f=f, fprime=fprime) {
  x <- seq(0, 2*pi, length=n)
  l <- rep(len, length=length(x))
  fx <- f(x)
  ystart <- fx - .5*l
  yend <- fx + .5*l
  ell <- yend-ystart
  # now correct for line illusion in vertical direction
  dy <- diff(range(fx))
  dx <- diff(range(x))
  # fprime works in framework of dx and dy, but we represent it in framework of dx and dy+len
  # needs to be fixed by factor a:  
  a <- dy/(dy + len) 
  ellx <- ell / cos(atan(abs(a*fprime(x))))
  # make this a data frame - ggplot2 doesn't do well with floating vectors
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx)
  
  dframe
}

adjY <- function(df){
  dx <- diff(df$x)
  dy <- diff(df$y)
  df$aspratio <- abs(diff(range(c(df$ystart, df$yend)))/diff(range(df$x)))
  adj <- atan(dy/dx)
  adj <- c(adj, rev(adj)[1])
  adj <- 1/cos(atan(abs(aspratio*adj)))
  adj <- adj-mean(adj)+1
  
  df$ell <- df$ell*adj
  df$ystart <- df$y - .5*df$ell
  df$yend <- df$y + .5*df$ell
  
  df
}


getSecantSegment <- function(x, df, f, fprime){
  secSlope <- -1/fprime(x)
  elltemp <- df$ell[which.min(abs(df$x-x))]
  temp <- seq(min(df$x)-x, max(df$x)+x, .0001)
  leftend <- temp[which.min(abs(f(temp) + elltemp/2 - secSlope*(temp-x)))]
  rightend <- temp[which.min(abs(f(temp) - elltemp/2 - secSlope*(temp-x)))]
  yintercept <- -secSlope*x
  df2 <- data.frame(xstart=leftend, x=x, xend=rightend, ystart=(yintercept + secSlope*(leftend)), y=secSlope*x, yend = (yintercept + secSlope*(rightend)))
  df2$ell <- with(df2, sqrt((yend-ystart)^2+(xend-xstart)^2))
  df2$type <- "Perceived Width"
  return(df2)
}

suppressMessages(library(ggplot2))
dframe <- createSine(n = 40, len = 1, f=f, fprime=fprime)
dframe$type <- "Segments"

secantlines <- do.call("rbind", lapply(seq(0, 2*pi, length=40), function(i) getSecantSegment(i, dframe, sin, cos)))
secantlines2 <- do.call("rbind", lapply(seq(0, 2*pi, length=40), function(i) getSecantSegment(i, adjY(dframe), sin, cos)))

library(plyr)
dframe2 <- rbind.fill(dframe, secantlines)

qplot(x=xstart, xend=xend, y = ystart, yend=yend, geom="segment", data=dframe2, colour=type) +
  scale_colour_manual(values=c("blue", "black"))+
  theme_bw() + coord_fixed(ratio=1)+ 
  geom_segment(aes(x=xstart, xend=xend, y=ystart, yend=yend), data=secantlines, colour="blue")

qplot(x=x, y=ell, geom="line", data=subset(dframe2, type=="Perceived Width" & ell>.7))
qplot(x=x, xend=xend, y=ystart, yend=yend, geom="segment", data=adjY(dframe))+
  theme_bw() + coord_fixed(ratio=1)+ 
  geom_segment(aes(x=xstart, xend=xend, y=ystart, yend=yend), data=secantlines2, colour="blue") 