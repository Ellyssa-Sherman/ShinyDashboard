# Assignment Requirements:
# 1) Read in a csv formatted file regrex1.csv. | DONE
# 2) Output a scatter plot | DONE
# 3) Add a button to model the data (linear model) | DONE
# 4) Output a plot of the linear model overlayed on the original data.
# 5) Output the slope, intercept, and correlation coefficient
# 6) Push the functional app to a Binder git repo.  
# 7) Submit the git URLs for each jupyter notebook repo (one for R and one for pyton). The URL must contain the notebook (.ipynb) the exported html of the completed assignment (.html) and a modified README with the badge icon for the binder notebook used to create the assignment.  YOU ARE ALLOWED MULTIPLE UPLOAD ATTEMPTS.  LOAD EACH URL SEPARATELY.

# 
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
            # Horizontal line for presentation of information
            tags$hr(),
            
            # Input: Checkbox if file has header
            checkboxInput("header", "Header", TRUE),
            tags$hr(), # Horizontal line
            
            # Input: Select separator
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            tags$hr(), # Horizontal line
            
            # Input: Select quotes
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            tags$hr(), # Horizontal line
            
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
           tableOutput("contents"),
           textOutput("summary"),
           textOutput("summary1"),
           textOutput("summary2")
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
    })
        
    output$summary <- renderPrint({
        y <- dataInput()$y
        x <- dataInput()$x
        lmPlot <- lm(y ~ x)
        # attributes(summary(lmPlot))
        summary(lmPlot)$slope
        
        summary1(lmPlot)$coefficients
        summary2(lmPlot)$r.squared
        # Display slope, intercept (coefficients), and correlation coefficient (r.squared)
    })
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
