#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dataH <- Hall %>% separate(ID, c('study','session','nsrrid'))
dataW <- Wall %>% separate(ID, c('study','session','nsrrid'))
IDs <- unique(dataW$nsrrid)
n_IDs <- length(IDs)
# UI for NMF dynamics explorer (one individual)
ui <- fluidPage(

    # Application title
    titlePanel("NMF dynqmics explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("ID",
                        "Individual:",
                        min = 1,
                        max = n_IDs,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("twoNightPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$twoNightPlot <- renderPlot({
        df <- filter(dataH, 
                     nsrrid==sym(!!IDs[[input$ID]]),
                     str_detect(component, 'V*')) %>%
            group_by(component) %>% 
            mutate(value = normalize(value))
        ggplot(df, aes(x=E, y=value, color=component, shape=as.factor(STAGE_N)))+
            geom_point()+
            scale_color_brewer(palette='Set1')+
            facet_wrap(~session, nrow=2, scales='free')
    }, height=800, width=800)
}

# Run the application 
shinyApp(ui = ui, server = server)
