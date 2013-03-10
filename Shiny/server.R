library(shiny)
library(ggplot2)
source("./functions.R")

shinyServer(function(input, output) {
  suppressMessages(library(ggplot2))
  
  output$illusion <- reactivePlot(function() {
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime)[c(2:(input$obs+1)),]
    dframe$color <- "black"
    if(input$hidelines){
      dframe$color <- "grey"
      dframe$color[sample(1:input$obs, 2)] <- "black"
    }
    p1 <- qplot(x=x, xend=x, y=ystart, yend=yend, geom="segment", data=dframe, color=color) +
      theme_bw() + scale_color_manual(values=c("black" = "black", "grey" = "grey90"), guide="none") +
      scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                         labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                  expression(paste(pi,"/2")), expression(pi))) +
      coord_equal(ratio=1) + xlab("x") + ylab("y")
    print(p1)
  })
  
  output$xcorrect <- reactivePlot(function() {
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime)[c(2:(input$obs+1)),]
    minor.axis.correction <- correctx(seq(-pi, pi, pi/8), fprime, w=input$weight)
    dots <- data.frame(x = c(seq(-pi, pi, pi/8), rep(minor.axis.correction, times=1)), 
                       y = rep(c(-input$amp -input$ell/2-.5), each=2*length(minor.axis.correction)), 
                       adj = c(rep("Correction: None", times=length(minor.axis.correction)), 
                               rep(paste("X corrected, weight =", input$weight), times=length(minor.axis.correction))))
    dframeAdj <- rbind(cbind(dframe, adj="Correction: None"), cbind(adjx(dframe, fprime=fprime, w=input$weight), adj=paste("X corrected, weight =", input$weight)))
    
#     p1 <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframeAdj) +
#       facet_grid(.~adj, scales="free_x") + theme_bw() +
#       scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), minor_breaks=minor.axis.correction,
#                          labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, expression(paste(pi,"/2")), expression(pi))) +
#       geom_point(data=dots, aes(x=x, y=y), inherit.aes=FALSE) +
#       coord_equal(ratio=1) + xlab("x") + ylab("y")
    pa <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=dframe, main="Correction: None") + theme_bw() +
            scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), minor_breaks=minor.axis.correction,
                               labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, expression(paste(pi,"/2")), expression(pi))) +
            geom_point(data=subset(dots, adj=="Correction: None"), aes(x=x, y=y), inherit.aes=FALSE) +
            coord_equal(ratio=1) + xlab("x") + ylab("y") 
    pb <- qplot(x=xstart, xend=xend, y=ystart, yend=yend, geom="segment", data=adjx(dframe, fprime=fprime, w=input$weight),
                main=paste("Correction: X, weight =", input$weight)) + theme_bw() +
            scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), minor_breaks=minor.axis.correction,
                               labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, expression(paste(pi,"/2")), expression(pi))) +
            geom_point(data=subset(dots, adj!="Correction: None"), aes(x=x, y=y), inherit.aes=FALSE) +
            coord_equal(ratio=1) + xlab("x") + ylab("y")
    p1 <- grid.arrange(pa, pb, nrow=1)
    print(p1)
  })
  
  output$ycorrect <- reactivePlot(function() {
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime)[c(2:(input$obs+1)),]
    corr <- which(input$correct==c("none", "geom", "linear", "quad"))
    rat <- input$ell/(2*input$amp+input$ell)
    
    dframeAdj <- cbind(rbind(dframe, dframe), with(dframe, rbind(data.frame(seg.ystart=y-ell/2, seg.yend=y+ell/2, type="Segment"), data.frame(seg.ystart=ell/2, seg.yend=-ell/2, type="Adjustment"))), adj="Original Data")
    pa <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj, main="Uncorrected Data") +
      theme_bw() + coord_equal(ratio=1) + facet_grid(type~., scales="free_y") + 
      scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                         labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                  expression(paste(pi,"/2")), expression(pi))) +
      xlab("x") + ylab("y")
    
    pa1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=subset(dframeAdj, type=="Segment"), main="Uncorrected Data") +
            theme_bw() + coord_equal(ratio=1) + 
            scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                               labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                        expression(paste(pi,"/2")), expression(pi))) +
            xlab("x") + ylab("y")
    pa2 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", 
                 data=subset(dframeAdj, type=="Adjustment"), main="Uncorrected Segment Length") + theme_bw() + 
                  scale_x_continuous(breaks=seq(-pi, pi, by=pi/2),
                                     labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                              expression(paste(pi,"/2")), expression(pi))) +
                  xlab("x") + ylab("y") + coord_equal(ratio=1)
    if(corr==4){
      dframeAdj1 <- adjQuad(dframe, f, fprime, f2prime)
      title = "Correction: Quadratic"
    } else if(corr==3){
      dframeAdj1 <- adjLinear(dframe, f, fprime, f2prime)
      title = "Correction: Linear"
    } else if(corr==2){
      dframeAdj1 <- adjGeom(dframe, f, fprime, f2prime)
      title = "Correction: Trigonometric"
    } else {
      dframeAdj1 <- adjNone(dframe, f, fprime, f2prime)
      title = "Correction: None"
    }
    limits <- range(c(subset(dframeAdj1, type=="Adjustment")$seg.ystart, subset(dframeAdj, type=="Adjustment")$seg.ystart, 
                      subset(dframeAdj1, type=="Adjustment")$seg.yend, subset(dframeAdj, type=="Adjustment")$seg.yend))
    if(corr==1) { 
      pb1 <- pb2 <- pb <- ggplot(dframeAdj1, aes(x = x, y = y)) + geom_blank() + theme_minimal() + 
                      theme(axis.title=element_blank(), axis.text=element_blank(), 
                            axis.ticks=element_blank(),  axis.line=element_blank(), 
                            panel.grid=element_blank()) 
    } else{
        pb <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj1, main=title) +
          theme_bw() + coord_equal(ratio=1) + facet_grid(type~., scales="free_y")+
          scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                             labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                      expression(paste(pi,"/2")), expression(pi))) +
          xlab("x") + ylab("y")
        pb1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=subset(dframeAdj1, type=="Segment"), main=title) +
                theme_bw() + coord_equal(ratio=1) + 
                scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                                   labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                            expression(paste(pi,"/2")), expression(pi))) +
                xlab("x") + ylab("y")
        pb2 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", 
                     data=subset(dframeAdj1, type=="Adjustment"), main="Corrected Segment Length") +
                     theme_bw() + 
                     scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                                        labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                                 expression(paste(pi,"/2")), expression(pi))) +
                     xlab("x") + ylab("y") + ylim(limits) + coord_equal(ratio=1)
        pa2 <- pa2 + ylim(limits)
    }

    p1 <- grid.arrange(pa, pb, widths=c(.5, .5), nrow=1)
    #     p1 <- grid.arrange(arrangeGrob(pa1, pb1, nrow=1, widths=c(.5, .5)), arrangeGrob(pa2, pb2, nrow=1, widths=c(.5, .5)), heights=c(input$ell+input$amp*2, input$ell), nrow=2)
    print(p1)
  })
})