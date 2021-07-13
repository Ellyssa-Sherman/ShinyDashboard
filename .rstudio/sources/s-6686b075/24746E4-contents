# Example Application for Ellyssa Sherman's repo called ShinyDashboard imported from Dr. Freitas’ repo
# Helpful links on Shiny here:
#    http://shiny.rstudio.com/
#    http://rstudio.github.io/shinydashboard/get_started.html
#    https://shiny.rstudio.com/gallery/file-upload.html

library(shiny)

# Define UI for application that draws a histogram
# This gives you a fluid/adaptable webpage, by resizing and restocking elements
ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("bins", # ID
                        "Number of bins:", # Label for ID
                        min = 1,
                        max = 50,
                        value = 30) # Starting value
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("contents"),
            plotOutput("distPlot")
            
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'orange', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)