library(shiny)
library(ggplot2)
source("./functions.R")

#' Look at https://groups.google.com/forum/?fromgroups=#!topic/shiny-discuss/THb1Ql5E20s for shiny server user-level data collection

shinyServer(function(input, output) {
  suppressMessages(library(ggplot2))
  
  output$illusion <- renderPlot({
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime, a=-pi, b=pi)[c(2:(input$obs+1)),]
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
  
  output$xcorrect <- renderPlot({
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime, a=-pi, b=pi)[c(2:(input$obs+1)),]
    minor.axis.correction <- correctx(seq(-pi, pi, pi/8), fprime, w=input$weight)
    dots <- data.frame(x = c(seq(-pi, pi, pi/8), rep(minor.axis.correction, times=1)), 
                       y = rep(c(-input$amp -input$ell/2-.5), each=2*length(minor.axis.correction)), 
                       adj = c(rep("Correction: None", times=length(minor.axis.correction)), 
                               rep(paste("X corrected, weight =", input$weight), times=length(minor.axis.correction))))
    dframeAdj <- rbind(cbind(dframe, adj="Correction: None"), cbind(adjx(dframe, fprime=fprime, w=input$weight), adj=paste("X corrected, weight =", input$weight)))
    
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
  
  output$ycorrect <- renderPlot({
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime, a=-pi, b=pi)[c(2:(input$obs+1)),]
    corr <- which(input$correct==c("none", "geom", "linear", "quad"))
    rat <- input$ell/(2*input$amp+input$ell)
    
    dframeAdj <- cbind(rbind(dframe, dframe), with(dframe, rbind(data.frame(seg.ystart=y-ell/2, seg.yend=y+ell/2, type="Segment"), data.frame(seg.ystart=ell/2, seg.yend=-ell/2, type="Adjustment"))), adj="Original Data")

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
      dplot <- dframeAdj
    } else{
      dplot <- rbind(dframeAdj, dframeAdj1)
    }
    p <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dplot) +
      theme_bw() + coord_equal(ratio=1) + facet_grid(type~adj, scales="free_y", space="free_y") +
      scale_x_continuous(breaks=seq(-pi, pi, by=pi/2), 
                         labels=c(expression(-pi), expression(paste(-pi, "/2")), 0, 
                                  expression(paste(pi,"/2")), expression(pi))) +
      xlab("x") + ylab("y")
    print(p)
  })
})