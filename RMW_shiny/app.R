# creating shiny app to describe rescorla wagner model
rm(list=ls())
#retrieve RMW function
source ("000.Rescorla Wagner Model.R")


library (shiny)

#runExample("01_hello")
# Define UI ----
ui<- fluidPage(
  # App title ----
  titlePanel("RWM"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the learning rate ----
      sliderInput(inputId = "Alpha",
                  label = "Alpha:",
                  min = 0,
                  max = 1,
                  value = 0.3)
  
),
# Main panel for displaying outputs ----
mainPanel(
  
  # Output: plot ----
  plotOutput(outputId = "RWMplot")
  
)
  )
)
 #Define server logic required to draw a plot ----
server <- function(input, output) {
  output$RWMplot<-renderPlot({
 x<-value
    Alpha<-input$Alpha
    plot(update_RW(x, Alpha,lambda)[,1], ylim=c(0,1), type="l", col="black", ylab="", xlab="Trial", xaxt="n")
    lines(update_RW(x, Alpha,lambda)[,2], col="red")
    axis(1, at = 1:20)
    legend(x=9, y=0.5, legend = c("value", "prediction error"), 
           lty = c(1,1),col  = c("black", "red"))
  })
}

shinyApp(ui=ui, server=server)

