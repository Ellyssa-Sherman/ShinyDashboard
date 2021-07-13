# Example Application for Ellyssa Sherman's repo called ShinyDashboard imported from Dr. Freitas’ repo
# Helpful links on Shiny here:
#    http://shiny.rstudio.com/
#    http://rstudio.github.io/shinydashboard/get_started.html
#    https://shiny.rstudio.com/gallery/file-upload.html

library(shiny)

# Define UI for application
ui <- fluidPage(
    
    # Application title
    titlePanel("Data Dashboard"),
    
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
      
        ),
        
    # Show plots of the generated data
        mainPanel(
            tableOutput("contents"),
            plotOutput("distPlot"),
            plotOutput("lmPlot")

        )
    )
)

# Define server for all plots
server <- function(input, output) {
    
    dataInput <- reactive({
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
        
    })
    
    output$contents <- renderTable({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'orange', border = 'white')
    })
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x, dataInput()$y)
    })
    
    output$breaks <- renderText({
        x    <- faithful[, 2]
        seq(min(x), max(x), length.out = input$bins + 1)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)