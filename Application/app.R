#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#In this project, I will be looking at the 'iris' dataset found within R. This project compares
# Setosa, Versicolor, and Virginica flower's relative sepal width, along with comparitive relationships
# to Sepal Width and Length between and within different flower types. The code below works to make divisional
# frequency work with a histogram graph along with toggles for each flower type on both the aforementioned
# histogram as well as the scatterplot


#Imports Shiny library
library(shiny)
# Brings in the iris data set (comes with r)
data("iris")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CIS625 - Homework 4 - Iris Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        #Sidebar object
        sidebarPanel(
          #The variable this widget provides to 'input' is called 'bins'
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 30,
                        value = 15),
            #This radio button has varible, 'haveSetosa', with a value of either 'ShowSetosa' or 'DontShowSetosa'. This value is provided to input, like before.
            radioButtons("haveSetosa",
                        "Setosa",c("Show"="ShowSetosa","Don't Show" = "DontShowSetosa")),
            #Same as above, however it is for Versicolor.
            radioButtons("haveVersicolor",
                         "Versicolor",c("Show"="ShowVersicolor","Don't Show" = "DontShowVersicolor")),
            #Same as above, however it is for Virginica.
            radioButtons("haveVirginica",
                         "Virginica",c("Show"="ShowVirginica","Don't Show" = "DontShowVirginica"))
        ),#End of sidebar

        # Show a plot of the generated distribution
        mainPanel(
          #Show Histogram (below)
           plotOutput("distPlot"),
           #Show ScatterPlot (below)
           plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    #HISTOGRAM
    output$distPlot <- renderPlot({
        #Each var grabs only sepal width values from its proper species (values taken from same row)
        x    <- iris[iris$Species=="setosa",2]
        y    <- iris[iris$Species=="versicolor",2]
        z    <- iris[iris$Species=="virginica",2]
        # generate bins based on input$bins from ui.R
        bins <- seq(min(min(x), min(y), min(z)), max(max(x),max(y), min(z)), length.out = input$bins + 1)
        
        #This is because histograms can be 'added' or drawn on top of one another, but the first one can not have this tag, as there is nothing for it to be added to.
        # This variable provides a way to know if we should add, until we have graphed something, it should not.
        firstGraphMade <- F

        #If the haveSetosa radio button has show selected...
        if(input$haveSetosa=="ShowSetosa")
        {
          # Make a histogram, with the Setosa values, has breaks equal to the calculated sequence, and label our axis.
          hist(x, breaks = bins, col = rgb(0,1,0,0.25),
               xlab = 'centimeters (cm)',
               main = 'Setosa Sepal Width Frequency',
               add = firstGraphMade)
          # We made a graph, it is no longer our first graph. Subsequent hist calls should be in add mode, so we update firstGraphMade.
          firstGraphMade <- T
        }

        #If the haveVersicolor radio button has show selected...
        if(input$haveVersicolor=="ShowVersicolor")
        {
          # Make a histogram, with the Versicolor values, has breaks equal to the calculated sequence, and label our axis.
          hist(y, breaks = bins, col = rgb(0,0,1,0.25),
               xlab = 'centimeters (cm)',
               main = 'Versicolor Sepal Width', 
               add=firstGraphMade)
          # We made a graph, it is no longer our first graph. Subsequent hist calls should be in add mode, so we update firstGraphMade.
          firstGraphMade <- T
        }
        
        #If the haveVirginica radio button has show selected...
        if(input$haveVirginica=="ShowVirginica")
        {
          # Make a histogram, with the Virginica values, has breaks equal to the calculated sequence, and label our axis.
          hist(z, breaks = bins, col = rgb(1,0,0,0.25),
               xlab = 'centimeters (cm)',
               main = 'Virginica Sepal Width', 
               add=firstGraphMade)
          #No need to update firstGraphMade. We have nothing more to do.
        }
    })
    #SCATTERPLOT
    output$distPlot2 <- renderPlot({
      #All the Lengths
      y    <- iris[,1]
      #All the Widths
      x    <- iris[,2]
      #Create a Color column in the graph.
      iris$Color
      #If a row has 'setosa' as its Species, make its color value in that row green.
      iris$Color[iris$Species == "setosa"] = rgb(0,1,0,input$haveSetosa=="ShowSetosa")
      #If a row has 'versicolor' as its Species, make its color value in that row blue.
      iris$Color[iris$Species == "versicolor"] = rgb(0,0,1,input$haveVersicolor=="ShowVersicolor")
      #If a row has 'virginica' as its Species, make its color value in that row red.
      iris$Color[iris$Species == "virginica"] = rgb(1,0,0,input$haveVirginica=="ShowVirginica")
        
      #Graph the width and length values, make the dots 150% their normal size, label the x axis, label the y axis, and label the plot.
      plot(x,y, col=iris$Color, cex = 1.5, pch=19, xlab = 'Sepal Width (cm)', ylab = 'Sepal Length (cm)', main = 'Sepal Width Against Sepal Length')
      # draw the histogram with the specified number of bins

    })# Done with the Scatterplot
}

# Run the application 
shinyApp(ui = ui, server = server)
