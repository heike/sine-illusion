
createSine <- function(n=200, len=1, f=f, fprime=fprime, f2prime=f2prime) {
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
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell)
  dframe$deriv <- fprime(x)
  #   dframe$ell.geom <- ell/cos(atan(abs(a*fprime(x))))
  #   dframe$ell.linear <- ell*sqrt(1+fprime(x)^2)
  dframe
}


adjLinear <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  df2 <- cbind(df, 
               sec.ell1 = df$ell/2*sqrt(1+fprime(df$x)^2), 
               sec.ell2 = df$ell/2*sqrt(1+fprime(df$x)^2))
  with(df2, cbind(rbind(df, df), 
                  rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), 
                        data.frame(seg.ystart=-sec.ell1, seg.yend=+sec.ell2, type="Adjustment")), 
                  adj="Linear")
       )
}


adjGeom <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  dy <- diff(range(df$y))
  dx <- diff(range(df$x))
  a <- dy/(dy+df$ell)
  df2 <- cbind(df, 
               sec.ell1 = df$ell/(2*cos(atan(abs(a*fprime(df$x))))), 
               sec.ell2 = df$ell/(2*cos(atan(abs(a*fprime(df$x))))))
  with(df2, cbind(rbind(df, df), 
                  rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), 
                        data.frame(seg.ystart=-sec.ell1, seg.yend=sec.ell2, type="Adjustment")), 
                  adj="Geometric"))
}


adjQuad <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  x0 <- df$x
  secSlope   <- -1/fprime(x0)
  ell.x0     <- df$ell/2
  lambda1 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 - 
                                           2* fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  lambda2 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 + 
                                           2* fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  
  df2 <- cbind(df, data.frame(sec.xstart = lambda1*fprime(x0) + x0, 
                              sec.xend = lambda2*fprime(x0) + x0, 
                              sec.ystart = f(x0) - lambda1,
                              sec.yend = f(x0) - lambda2))
  
  df2$sec.ell1 <- with(df2, sqrt((sec.yend-y)^2+(sec.xend-x)^2))
  df2$sec.ell2 <- with(df2, sqrt((y-sec.ystart)^2+(x-sec.xstart)^2))
  
  theta <- atan(df2$deriv)  
  seg.ystart <-  with(df2, y+ell/2*(1+cos(theta))-sec.ell1*cos(theta))
  seg.yend   <-  with(df2, y-ell/2*(1+cos(theta))+sec.ell2*cos(theta))

        
  cbind(rbind(df, df), 
        rbind(data.frame(seg.ystart=seg.ystart, 
                         seg.yend=seg.yend,
                         type="Segment"),
              data.frame(seg.ystart= df2$ell/2*(1+cos(theta))-df2$sec.ell1*cos(theta), 
                         seg.yend=-df2$ell/2*(1+cos(theta))+df2$sec.ell2*cos(theta),
                         type="Adjustment")), 
        adj="Quadratic")
}