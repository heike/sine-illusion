library(gridExtra)

createSine <- function(n=200, len=1, f=f, fprime=fprime, f2prime=f2prime, a=0, b=2*pi) {
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
  
  fp <- a*fprime(x)
  f2p <- a*f2prime(x)
  lambdap <- (sqrt((fp^2+1)^2-f2p*fp^2*ell) + fp^2 + 1)^-1    
  lambdam <- -(sqrt((fp^2+1)^2+f2p*fp^2*ell) + fp^2 + 1)^-1    
  
  
  dframe$ellx4.l <- (4*abs(lambdap)*sqrt(1+fp^2))^-1
  dframe$ellx4.u <- (4*abs(lambdam)*sqrt(1+fp^2))^-1
  
  dframe
}


correctx <- function(z, fprime, a=-pi, b=pi, w=1) {
  # w = 1/(shrink+1)
  const <- integrate(function(x) abs(fprime(x)), a, b)$value
  trans <- sapply(z, function(i) integrate(function(x) abs(fprime(x)), a, i)$value*(b-a)/const + a)
  # alternatively to the rowMeans, you could report back  
  # trans*(1-w) + z*w
  trans*w + z*(1-w)
}


adjx <- function(df, fprime=fprime, w=1){
  df$xend <- df$xstart <- correctx(df$x, fprime=fprime, w=w)
  df
}

adjNone <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  df2 <- cbind(df, 
               sec.ell1 = df$ell/2, 
               sec.ell2 = df$ell/2)
  with(df2, cbind(rbind(df, df), 
                  rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), 
                        data.frame(seg.ystart=-sec.ell1, seg.yend=+sec.ell2, type="Adjustment")), 
                  adj="Correction: None")
  )
}

adjLinear <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  dy <- diff(range(df$y))
  dx <- diff(range(df$x))
  a <- dy/(dy+df$ell)
  df2 <- cbind(df, 
               sec.ell1 = df$ell/2*sqrt(1+a^2*fprime(df$x)^2), 
               sec.ell2 = df$ell/2*sqrt(1+a^2*fprime(df$x)^2))
  with(df2, cbind(rbind(df, df), 
                  rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), 
                        data.frame(seg.ystart=-sec.ell1, seg.yend=+sec.ell2, type="Adjustment")), 
                  adj="Correction: Linear")
       )
}


adjGeom <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  cbind(rbind(df, df), with(df, 
    rbind(data.frame(seg.ystart=y+ellx2/2, 
                     seg.yend=y-ellx2/2,
                     type="Segment"),
          data.frame(seg.ystart= ellx2/2, 
                     seg.yend=-ellx2/2,
                     type="Adjustment"))), 
    adj="Correction: Trigonometric")
}


adjQuad <- function(df, f=f, fprime=fprime, f2prime=f2prime){
        
  cbind(rbind(df, df), with(df, 
        rbind(data.frame(seg.ystart=y+ellx4.u, 
                         seg.yend=y-ellx4.l,
                         type="Segment"),
              data.frame(seg.ystart= ellx4.u, 
                         seg.yend=-ellx4.l,
                         type="Adjustment"))), 
        adj="Correction: Quadratic")
}