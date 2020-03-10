#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(

    # Application title
    titlePanel("ChAT app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("rank",
                        "Rank:",
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("chbPlot"),
           plotOutput("chbDiffscalePlot")
        )
    )
)

server <- function(input, output) {

    output$chbPlot <- renderPlot({
        
        # plot
        compare_CHB(m1, m2, 
                    distp[input$rank,]$ID, 
                    distp[input$rank,]$CH, 
                    distp[input$rank,]$B, 
                    cycle=c(1,2,3,4,5),
                    distp[input$rank,]$value)
        
        
    })
    output$chbDiffscalePlot <- renderPlot({
        compare_CHB_diffscale(m1, m2, 
                              distp[input$rank,]$ID, 
                              distp[input$rank,]$CH, 
                              distp[input$rank,]$B, 
                              cycle=c(1,2,3,4,5),
                              distp[input$rank,]$value)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
