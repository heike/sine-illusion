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
  a <- dx/(dy + len) * len/dy
  ellx <- ell / cos(atan(abs(a*fprime(x))))
  # make this a data frame - ggplot2 doesn't do well with floating vectors
  dframe <- data.frame(x=x, xstart=x, xend=x, y=fx, ystart=ystart, yend=yend, ell=ell, ellx = ellx)
  
  dframe
}

qplot(x=x, xend=xend, y = ystart, yend=yend, geom="segment", data=createSine(40,.5, f=f, fprime=function(x) abs(cos(x)))) +
  theme(panel.grid.major=element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.minor=element_blank(), panel.background=element_blank(),
        axis.title = element_blank(), axis.ticks = element_blank(), 
        axis.text = element_blank()
  ) +
  coord_equal(ratio=1) + 
  geom_segment(aes(x=x-0.02, xend=xend, y=ystart-fprime(2*x+.01)+fprime(2*x-0.01), yend=ystart))+  
  geom_segment(aes(x=x+0.02, xend=xend, y=ystart-fprime(2*x+.01)+fprime(2*x-0.01), yend=ystart))+ 
  geom_segment(aes(x=x-0.02, xend=xend, y=yend+fprime(2*x+.01)-fprime(2*x-0.01), yend=yend))+ 
  geom_segment(aes(x=x+0.02, xend=xend, y=yend+fprime(2*x+.01)-fprime(2*x-0.01), yend=yend))