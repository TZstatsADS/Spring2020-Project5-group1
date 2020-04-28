

library(shiny)
ui <- fluidPage(
        titlePanel("Simulation"),
        sliderInput(inputId = 'N', 
                    label = 'Number of points', 
                    value = 3000, 
                    min = 300, 
                    max = 10000),
        actionButton(inputId = 'Start',
                     label = 'Start'),
        actionButton(inputId = 'Next', 
                     label = 'Next day'),
        actionButton(inputId = 'Auto', 
                     label = 'Auto'),
        actionButton(inputId = 'Stop', 
                     label = 'Stop'),
        plotOutput("Simulation"),
)

