library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Sine Illusion"),
  
  sidebarPanel(
    sliderInput("obs", 
                "Number of lines:", 
                min = 10, 
                max = 100, 
                value = 40, step=5),
    sliderInput("ell", "Line Length:", min=.5, max=10, value=1, step=.1),
    sliderInput("amp", "Amplitude:", min=.5, max=10, value=1, step=.1),
    radioButtons("correct", "Correction Type:", c("No Correction" = "none", "Trigonometry Correction" = "geom", "Linear Correction" = "linear", "Quadratic Correction" = "quad"), selected="No Correction")
  ),
  #-----  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("sineIllusion")
  )
))
