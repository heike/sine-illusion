x <- seq(0, 2*pi+.01, length=100)
l <- rep(1, length(x))
ystart <- 4*sin(x) - .5*l
yend <- 4*sin(x) + .5*l
x1 <- cumsum(diff(x)[1]*(.4*cos(2*x)+1))

library(ggplot2)
qplot(x=x, xend=x, y = 2*sin(x)-.5*y, yend=2*sin(x)+.5*y, colour=I("blue"), geom="segment") +
  geom_segment(aes(x = 0, xend=2*pi, y=0, yend=0), colour="black") + 
  opts(plot.background=theme_blank(), panel.grid.major=theme_blank(), 
       panel.grid.minor=theme_blank(), panel.background=theme_blank())

qplot(x=x, xend=x, y = 2*sin(x), yend=2*sin(x)+y, colour=I("blue"), geom="segment") +
  geom_segment(aes(x = 0, xend=2*pi, y=0, yend=0), colour="black") + 
  opts(plot.background=theme_blank(), panel.grid.major=theme_blank(), 
       panel.grid.minor=theme_blank(), panel.background=theme_blank())

qplot(x=x1, xend=x1, y = ystart, yend=yend, colour=I("blue"), geom="segment") +
  geom_line(aes(x=x, y=cos(2*x)), colour=I("red")) +
  geom_segment(aes(x = 0, xend=2*pi, y=0, yend=0), colour="black") + 
  opts(plot.background=theme_blank(), panel.grid.major=theme_blank(), 
       panel.grid.minor=theme_blank(), panel.background=theme_blank())

qplot(x=x1, xend=x1, y = ystart, yend=yend, colour=I("blue"), geom="segment") +
  geom_line(aes(x=x, y=cos(2*x)), colour=I("red")) +
  geom_segment(aes(x = 0, xend=2*pi, y=0, yend=0), colour="black") + 
  opts(plot.background=theme_blank(), panel.grid.major=theme_blank(), 
       panel.grid.minor=theme_blank(), panel.background=theme_blank())+ylim(-10,10)

qplot(x=x, y=x1, geom="line") + geom_vline(xintercept=pi/2)