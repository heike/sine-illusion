library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sine Illusion"),
  
  sidebarPanel(
    sliderInput("obs", 
                "Number of lines:", 
                min = 20, 
                max = 100, 
                value = 40, step=5),
    sliderInput("ell", "Line Length:", min=.5, max=10, value=1, step=.1),
    sliderInput("amp", "Amplitude:", min=.5, max=10, value=1, step=.1),
    conditionalPanel(condition="input.whichtab==1", 
                     checkboxInput("hidelines", "Hide lines", FALSE)
    ),
    conditionalPanel(condition="input.whichtab==3", 
      radioButtons("correct", "Correction Type:", c("No Correction" = "none", 
                                                    "Trigonometry Correction" = "geom", 
                                                  # "Linear Correction" = "linear", 
                                                    "Quadratic Correction" = "quad"), 
                   selected="No Correction")
    ),
    conditionalPanel(condition="input.whichtab==2", 
      sliderInput("weight", "Shrink by 1-w", min=0, max=1, value=1, step=.05)
    )
  ),
  mainPanel(
    tabsetPanel(id="whichtab", 
      tabPanel("Sine Illusion", plotOutput("illusion", width="auto"), value=1),
      tabPanel("Correct X Axis", plotOutput("xcorrect", width="auto"), value=2),
      tabPanel("Correct Y Axis", plotOutput("ycorrect", width="auto", height="600px"), value=3)#,
      #tabPanel("Temporary Transformation", plotOutput("temptransform"), value=4)      
      )
  )
))
