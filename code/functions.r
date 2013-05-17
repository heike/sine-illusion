#' This function has to be split in parts and renamed
#' 
#' Right now this function is a convoluted mess.  We need to have separate
#' functions that (1) create a data frame for a given function, its first and second derivatives and range in x
#' (2) a trig transform function
#' (3) a quadratic approach function - better even, make (2) and (3) one function and use a parameter to decide on the method to use.

createSine <- function(n=200, len=1, f=f, fprime=fprime, getquadapprox=FALSE, f2prime=getquadapprox, a=0, b=2*pi) {
  #  if(getquadapprox & !is.function(f2prime)) f2prime <- function(x) -1*f(x) # for backwards compatibility
  x <- seq(a, b, length=n+2)[(2:(n+1))]
  ell <- rep(len, length=length(x))
  fx <- f(x)
  ystart <- fx - .5*ell
  yend <- fx + .5*ell
  
  # now correct for line illusion in vertical direction
  dy <- diff(range(fx))
  dx <- diff(range(x))
  # fprime works in framework of dx and dy, but we represent it in framework of dx and dy+len
  # needs to be fixed by factor a:  
  a <- dy/(dy + len) 
  # ellx is based on the "trig" correction
  ellx <- ell / cos(atan(abs(a*fprime(x))))
  # ellx2 is based on linear approximation of f  
  ellx2 <- ell * sqrt(1 + a^2*fprime(x)^2)
  
  # make this a data frame - ggplot2 doesn't do well with floating vectors
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx, ellx2=ellx2)
  
  # third adjustment is based on quadratic approximation of f.
  # this needs two parts: correction above and below f(x)  
  #   if(getquadapprox & is.function(f2prime)){
  #     secseg <- do.call("rbind", lapply(dframe$x, function(i) getSecantSegment(i, dframe, f, fprime, f2prime)))
  #     dframe$ellx3.u <- secseg$sec.ell1
  #     dframe$ellx3.l <- secseg$sec.ell2
  #   }
  
  fp <- a*fprime(x)
  f2p <- a*f2prime(x)
  v <- 1 + fp^2
  lambdapinv <- (sqrt(v^2-f2p*fp^2*ell) + v)    
  lambdaminv <- -(sqrt(v^2+f2p*fp^2*ell) + v)
  
  
  dframe$ellx4.l <- abs(lambdapinv)*(4*sqrt(v))^-1
  dframe$ellx4.u <- abs(lambdam)*(4*sqrt(v))^-1
  
  dframe
}

getSecantSegment <- function(x0, df, f, fprime, f2prime){
  ell     <- sapply(x0, function(i) df$ell[which.min(abs(i-df$x))]/2)
  
  dy <- diff(range(df$y))
  dx <- diff(range(df$x))
  a <- dx/(dy + 2*ell) 
  
  fp <- a*fprime(x0)
  f2p <- a*f2prime(x0)
  v <- fp^2 + 1
  lambdam = (v + sqrt(v^2 + ell*f2p*fp^2))^-1
  lambdap = (v + sqrt(v^2 - ell*f2p*fp^2))^-1
  
  #---- Approximation
  
  x2 <- lambdap*fp+x0
  x1 <- lambdam*fp+x0
  y2 <- f(x0)-lambdap
  y1 <- f(x0)-lambdam
  #----
  
  df2 <- data.frame(x=x0, y=f(x0), deriv=fprime(x0),
                    sec.xstart=x1, sec.xend = x2, 
                    sec.ystart=y1, sec.yend = y2,
                    ell = 2*ell)
  
  #  df2$sec.ellp <- (4*abs(lambdap)*sqrt(1+fp^2))^-1
  #  df2$sec.ellm <- (4*abs(lambdam)*sqrt(1+fp^2))^-1
  df2$sec.ellp <- with(df2, sqrt((sec.yend-y)^2+(sec.xend-x)^2))
  df2$sec.ellm <- with(df2, sqrt((y-sec.ystart)^2+(x-sec.xstart)^2))
  df2$type <- "Perceived Width"
  df2$a <- a
  return(df2)
}