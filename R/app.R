#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
data("iris")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CIS625 - Homework 4 - Iris Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 30,
                        value = 15),
            
            radioButtons("haveSetosa",
                        "Setosa",c("Show"="ShowSetosa","Don't Show" = "DontShowSetosa")),
            
            radioButtons("haveVersicolor",
                         "Versicolor",c("Show"="ShowVersicolor","Don't Show" = "DontShowVersicolor")),
            
            radioButtons("haveVirginica",
                         "Virginica",c("Show"="ShowVirginica","Don't Show" = "DontShowVirginica"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- iris[iris$Species=="setosa",2]
        y    <- iris[iris$Species=="versicolor",2]
        z    <- iris[iris$Species=="virginica",2]
        
        bins <- seq(min(min(x), min(y), min(z)), max(max(x),max(y), min(z)), length.out = input$bins + 1)
        
        firstGraphMade <- F

        if(input$haveSetosa=="ShowSetosa")
        {
          hist(x, breaks = bins, col = rgb(0,1,0,0.25),
               xlab = 'centimeters (cm)',
               main = 'Setosa Sepal Width',
               #legend("bottomleft", c("Setosa", "Versicolor", "Virginica")),
              # legend("bottomleft", c("Setosa", "Versicolor", "Virginica"), col=c(rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,0,0,0.25)))
               add = firstGraphMade)
          firstGraphMade <- T
        }
        # draw the histogram with the specified number of bins
        
        if(input$haveVersicolor=="ShowVersicolor")
        {
          hist(y, breaks = bins, col = rgb(0,0,1,0.25),
               xlab = 'centimeters (cm)',
               main = 'Versicolor Sepal Width', 
               add=firstGraphMade)
          firstGraphMade <- T
        }
        
        if(input$haveVirginica=="ShowVirginica")
        {
          hist(z, breaks = bins, col = rgb(1,0,0,0.25),
               xlab = 'centimeters (cm)',
               main = 'Versicolor Sepal Width', 
               add=firstGraphMade)
        }
    })
    
    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- iris[iris$Species=="versicolor",2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
