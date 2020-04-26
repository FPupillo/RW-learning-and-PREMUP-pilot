# creating shiny app to describe rescorla wagner model
rm(list=ls())
#retrieve RMW function
source ("010. Learning rule from Daw (2011).R")


library (shiny)

#runExample("01_hello")
# Define UI ----
ui<- fluidPage(
  # App title ----
  titlePanel("Q-learning model"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the learning rate ----
      sliderInput(inputId = "Alpha",
                  label = "Alpha:",
                  min = 0,
                  max = 1,
                  value = 0.15),
      # Input: Slider for the learning rate ----
      sliderInput(inputId = "Beta",
                  label = "Beta:",
                  min = 1,
                  max = 7,
                  value = 2.4)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: plot ----
      plotOutput(outputId = "Qplot")
      
    )
  )
)
#Define server logic required to draw a plot ----
server <- function(input, output) {
  output$Qplot<-renderPlot({
    Alpha<-input$Alpha
    Beta<-input$Beta
    plot(updateRL( Alpha,Beta)[,1], ylim = c(-1, 1),type="l", col="black", ylab="", xlab="Trial", xaxt="n", yaxt="n")
    axis(2, at=c(-1.0,-0.2,-0.4,-0.6,-0.80,0, 0.2,0.4,0.6,0.8,1.0))
    abline(h=c(-1.0,-0.2,-0.4,-0.6,-0.80,0.2,0.4,0.6,0.8,1.0), lty=1, col="grey")#for unsined PE
    abline(h=0, lty=1, col="black")#for the zero
    lines(updateRL( Alpha,Beta)[,4], col="red")
    abline(v=seq(1:20), lty=1, col="grey")
    axis(1, at = 1:20)
    r<- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 0, 0, 1, 1) 
    axis(3, at = c(1:20),  labels = r)
    mtext("Accuracy", 3, line=2)
    legend("bottomleft",inset =0.1, legend = c(expression(P['j=M, (c=I)']), "prediction error"), lty = c(1,1),col  = c("black", "red"))
  })
}

shinyApp(ui=ui, server=server)

#to deploy app /home/francesco/Desktop/PowerFolders/Frankfurt_University/Computational models/RW, Bayes, and PREMUP pilot/LR_shiny
