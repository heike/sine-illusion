library(shiny)

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
  df2 <- cbind(df, sec.ell1 = df$ell/2*sqrt(1+fprime(df$x)^2), sec.ell2 = df$ell/2*sqrt(1+fprime(df$x)^2))
#   with(df2, cbind(df, seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2))
  with(df2, cbind(rbind(df, df), rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), data.frame(seg.ystart=-sec.ell1, seg.yend=+sec.ell2, type="Adjustment")), adj="Linear"))
}
adjGeom <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  dy <- diff(range(df$y))
  dx <- diff(range(df$x))
  a <- dy/(dy+df$ell)
  df2 <- cbind(df, sec.ell1 = df$ell/(2*cos(atan(abs(a*fprime(df$x))))), sec.ell2 = df$ell/(2*cos(atan(abs(a*fprime(df$x))))))
  with(df2, cbind(rbind(df, df), rbind(data.frame(seg.ystart=y-sec.ell1, seg.yend=y+sec.ell2, type="Segment"), data.frame(seg.ystart=-sec.ell1, seg.yend=sec.ell2, type="Adjustment")), adj="Geometric"))
}
adjQuad <- function(df, f=f, fprime=fprime, f2prime=f2prime){
  x0 <- df$x
  secSlope   <- -1/fprime(x0)
  ell.x0     <- df$ell/2
  lambda1 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 - 2* fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  lambda2 <- (-(fprime(x0)^2 + 1) + sqrt((fprime(x0)^2 + 1)^2 + 2* fprime(x0)^2*f2prime(x0)*ell.x0))/(fprime(x0)^2*f2prime(x0))
  
  leftend <- lambda1*fprime(x0)+x0
  rightend <- lambda2*fprime(x0)+x0
  df2 <- cbind(df, data.frame(sec.xstart=leftend, sec.xend=rightend, 
                    sec.ystart=f(x0) -lambda1,
                    sec.yend = f(x0) -lambda2))
  
  df2$sec.ell1 <- with(df2, sqrt((sec.yend-y)^2+(sec.xend-x)^2))
  df2$sec.ell2 <- with(df2, sqrt((y-sec.ystart)^2+(x-sec.xstart)^2))
#   cbind(df, getAdjLength(df2))
  cbind(rbind(df, df), getAdjLength(df2), adj="Quadratic")
}

getAdjLength<- function(df){
  theta <- atan(df$deriv)  
  seg.ystart <-  with(df, y+ell/2*(1+cos(theta))-sec.ell1*cos(theta))
  seg.yend   <-  with(df, y-ell/2*(1+cos(theta))+sec.ell2*cos(theta))
#  data.frame(seg.ystart=seg.ystart, seg.yend=seg.yend)
  rbind(data.frame(seg.ystart=seg.ystart, 
                   seg.yend=seg.yend,
                   type="Segment"),
        data.frame(seg.ystart= df$ell/2*(1+cos(theta))-df$sec.ell1*cos(theta), 
                   seg.yend=-df$ell/2*(1+cos(theta))+df$sec.ell2*cos(theta),
                   type="Adjustment")
        )
}

shinyServer(function(input, output) {
  suppressMessages(library(ggplot2))
  suppressPackageStartupMessages(library(gridExtra))
  
  output$sineIllusion <- reactivePlot(function() {
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime)[c(2:(input$obs+1)),]
    corr <- which(input$correct==c("none", "geom", "linear", "quad"))
    row2 <- input$nofunc
    
    dframeAdj <- cbind(rbind(df, df), with(df, rbind(data.frame(seg.ystart=y-ell/2, seg.yend=y+ell/2, type="Segment"), data.frame(seg.ystart=ell/2, seg.yend=-ell/2, type="Adjustment"))), adj="None")
    p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
            facet_grid(type~adj, scales="free_y") + theme_bw() + 
            coord_equal(ratio=1) + xlab("x") + ylab("y")
    if(corr==4){
      dframeAdj1 <- adjQuad(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
              facet_grid(type~adj, scales="free_y") + theme_bw() + 
              coord_equal(ratio=1) + xlab("x") + ylab("y")
#       p1 <- qplot(x=x, xend=x, y = seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
#         theme_bw() + coord_equal(ratio=1) + xlab("x") + ylab("y") + 
#         ylim(c(-input$ell*2+min(dframeAdj$y), input$ell*2+max(dframeAdj$y)))
#       p1f <- qplot(x=x, xend=x,y = seg.ystart-y, yend=seg.yend-y, geom="segment", data=dframeAdj) +
#         theme_bw() + xlab("x") + ylab("y") + coord_equal(ratio=1) + ylim(c(-input$ell, input$ell))
    } else if(corr==3){
      dframeAdj1 <- adjLinear(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
        facet_grid(type~adj, scales="free_y") + theme_bw() + 
        coord_equal(ratio=1) + xlab("x") + ylab("y")
#       p1 <- qplot(x=x, xend=x, y = seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
#         theme_bw() + coord_equal(ratio=1) + xlab("x") + ylab("y") + 
#         ylim(c(-input$ell*2+min(dframeAdj$y), input$ell*2+max(dframeAdj$y)))
#       p1f <- qplot(x=x, xend=x,y = seg.ystart-y, yend=seg.yend-y, geom="segment", data=dframeAdj) +
#         theme_bw() + xlab("x") + ylab("y") + coord_equal(ratio=1) + ylim(c(-input$ell, input$ell))
    } else if(corr==2){
      dframeAdj1 <- adjGeom(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
        facet_grid(type~adj, scales="free_y") + theme_bw() + 
        coord_equal(ratio=1) + xlab("x") + ylab("y")
#       p1 <- qplot(x=x, xend=x, y = seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
#         theme_bw() + coord_equal(ratio=1) + xlab("x") + ylab("y") + 
#         ylim(c(-input$ell*2+min(dframeAdj$y), input$ell*2+max(dframeAdj$y)))
#       p1f <- qplot(x=x, xend=x,y = seg.ystart-y, yend=seg.yend-y, geom="segment", data=dframeAdj) +
#         theme_bw() + xlab("x") + ylab("y") + coord_equal(ratio=1) + ylim(c(-input$ell, input$ell))
    }
    

#     
#     p0 <- qplot(x=x, xend=x, y = y+ell/2, yend=y-ell/2, geom="segment", data=dframe) +
#       theme_bw() + coord_equal(ratio=1) + xlab("x") + ylab("y") + 
#       ylim(c(-input$ell*2+min(dframe$y), input$ell*2+max(dframe$y)))
#     p0f <- qplot(x=x, xend=x, y = -ell/2, yend=ell/2, geom="segment", data=dframe) +
#       theme_bw() + xlab("x") + ylab("y") + coord_equal(ratio=1) + ylim(c(-input$ell, input$ell))
#     
#     if(row2){
#       if(corr==1) grid.arrange(p0, p0f, ncol=1, nrow=2, heights=c(2,1)) else grid.arrange(p0, p1, p0f, p1f, nrow=2, ncol=2, heights=c(2, 1)) 
#     } else{
#       if(corr==1) grid.arrange(p0) else grid.arrange(p0, p1, ncol=2, heights=c(2,1))
#     }
    grid.arrange(p1)
  })
})