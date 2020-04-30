#
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
    titlePanel("NNMF baseline/followup component 1 (O1; SLOW)"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            plotOutput("nnmfPlot", click='nnmf_click')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput('componentPlot'),
            plotOutput("hyPlot_nnmf")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ch <- 'O1'
    b <- 'SLOW'
    output$nnmfPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(nnH, aes(x=V2.x, y=V2.y))+geom_point()
    })
    
    output$componentPlot <- renderPlot({
        ggplot(nnW, aes(x=E, y=V2.y))+geom_line(color='black')+
        geom_line(aes(x=E,y=V2.x), color='grey')
    })
    
    output$hyPlot_nnmf <- renderPlot({
        nP <- nearPoints(nnH, input$nnmf_click, threshold=10, maxpoints=1)
        print(typeof(as.numeric(nP$nsrrid)))
        id <- as.numeric(nP$nsrrid)
        data_baseline <- tibble(E=1:l, 
                                RELPSD=(tsm[,1:379])[,id])
        data_followup <- tibble(E=1:l, 
                                RELPSD=(tsm[,380:758])[,id])
        p1 <- ggplot(data_baseline,aes(x=E, y=RELPSD))+geom_point()+
            ggtitle(ball$demo$nsrrid[as.numeric(nP$nsrrid)])#+
            #ylim(0,0.01)
        p2 <- ggplot(data_followup, aes(x=E, y=RELPSD))+geom_point()#+
            #ylim(0,0.01)
        p1/p2
    })
}

# Run the application 
shinyApp(ui = ui, server = server)