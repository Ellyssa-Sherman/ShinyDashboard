# Assignment Requirements:
# 1) Read in a csv formatted file regrex1.csv. | DONE
# 2) Output a scatter plot | DONE
# 3) Add a button to model the data (linear model) | DONE
# 4) Output a plot of the linear model overlayed on the original data. | DONE
# 5) Output the slope, intercept, and correlation coefficient | DONE
# 6) Push the functional app to a Binder git repo.  
# 7) Submit the git URLs for Shiny App.

# Additional Challenges (no graded)
# 1) create a button to export the plots
# 2) incorporate Plotly interactive graphs
# 3) Provide interactive controls to change the parameters of the linear model

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Group 5 | Regrex1 Data | Shiny Dashboard"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Input: File selecting option
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Input: Checkbox if file has header
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Input: Select quotes
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Input: Select number of rows to display
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            tags$hr(), # Horizontal line
            
            actionButton("lmPlot", "Linear Model"),
            tags$hr(), # Horizontal line
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           plotOutput("lmPlot"),
           h5("Slope"),
           textOutput("Slope"),
           h5("Y-Intercept"),
           textOutput("Intercepts"),
           h5("Correlation Coefficient"),
           textOutput("R.squared"),


        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    LinearModel <- eventReactive(input$lmPlot, {
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
    })
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y)
    })
    
    output$lmPlot <- renderPlot({
       # y <- dataInput()$y
       # x <- dataInput()$x
       # lmPlot <- lm(y ~ x)
       plot(dataInput()$x,dataInput()$y)
       abline(LinearModel())
    )}
       
# Display slope, intercept (coefficients), and correlation coefficient (r.squared)
    output$R.squared <- renderText({
        
        paste("Adjusted R2 = ",signif(summary(LinearModel())$adj.r.squared, 3))
        
    })
    output$Intercepts <- renderText({
        
        paste("Intercept =",signif(LinearModel()$coef[[1]], 3))
        
    })
    output$Slope <- renderText({
        
        paste("Slope =",signif(LinearModel()$coef[[2]], 3))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)