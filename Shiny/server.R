library(shiny)
source("./functions.R")

shinyServer(function(input, output) {
  suppressMessages(library(ggplot2))
  
  output$sineIllusion <- reactivePlot(function() {
    f <- function(x) input$amp*sin(x)
    fprime <- function(x) input$amp*cos(x)
    f2prime <- function(x) -input$amp*sin(x)
    dframe <- createSine(n=input$obs+2, len=input$ell, f, fprime, f2prime)[c(2:(input$obs+1)),]
    corr <- which(input$correct==c("none", "geom", "linear", "quad"))
    
    dframeAdj <- cbind(rbind(dframe, dframe), with(dframe, rbind(data.frame(seg.ystart=y-ell/2, seg.yend=y+ell/2, type="Segment"), data.frame(seg.ystart=ell/2, seg.yend=-ell/2, type="Adjustment"))), adj="None")
    p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
            facet_grid(type~adj, scales="free_y") + theme_bw() + 
            coord_equal(ratio=1) + xlab("x") + ylab("y")
    if(corr==4){
      dframeAdj1 <- adjQuad(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
              facet_grid(type~adj, scales="free_y") + theme_bw() + 
              coord_equal(ratio=1) + xlab("x") + ylab("y")
    } else if(corr==3){
      dframeAdj1 <- adjLinear(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
        facet_grid(type~adj, scales="free_y") + theme_bw() + 
        coord_equal(ratio=1) + xlab("x") + ylab("y")
    } else if(corr==2){
      dframeAdj1 <- adjGeom(dframe, f, fprime, f2prime)
      dframeAdj <- rbind(dframeAdj, dframeAdj1)
      p1 <- qplot(x=x, xend=x, y=seg.ystart, yend=seg.yend, geom="segment", data=dframeAdj) +
        facet_grid(type~adj, scales="free_y") + theme_bw() + 
        coord_equal(ratio=1) + xlab("x") + ylab("y")
    }
    
    print(p1)
  })
})