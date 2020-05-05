

library(shiny)
ui <- fluidPage(
        titlePanel("Simulation"),
        
        sidebarLayout(
        sidebarPanel(
        sliderInput(inputId = 'N', 
                    label = 'Number of points', 
                    value = 3000, 
                    min = 100, 
                    max = 10000),
        sliderInput(inputId = 'Num_public', 
                    label = 'Number of public places', 
                    value = 20, 
                    min = 4, 
                    max = 100),
        
        actionButton(inputId = 'Start',
                     label = 'Start'),
        actionButton(inputId = 'Next', 
                     label = 'Next day'),
        actionButton(inputId = 'Auto', 
                     label = 'Auto'),
        actionButton(inputId = 'Stop', 
                     label = 'Stop'),
        fluidRow(
        column(7,numericInput(inputId = 'SkipNum', 
                     label = 'Skip days', 
                     value = 5, 
                     min = 1,
                     max = 30, 
                     width = 120),
        ),
        column(2, 
        actionButton(inputId = 'Skip', 
                     label = 'Skip'),
        ),
        ),
        sliderInput(inputId = 'pc', 
                    label = 'Probability of being infected', 
                    value = 0.1, 
                    min = 0, 
                    max = 1),
        checkboxInput(inputId = 'mask', 
                      label = 'Wear Masks'),
        checkboxInput(inputId = 'socialdist', 
                      label = 'Social Distance'),
        checkboxInput(inputId = 'closerest', 
                      label = 'Close restaurant'),
        checkboxInput(inputId = 'quarantine', 
                      label = 'Quarantine'),
        checkboxInput(inputId = 'vaccine', 
                      label = 'R & D vaccine'),
        actionButton(inputId = 'expendhos', 
                     label = 'Expand Hospital', 
                     width = 260),
        textInput(inputId = 'gifname', 
                  label = 'File name',
                  value = 'output'),
        downloadButton("downloadgif", "Download"),
        ),
        mainPanel(
        plotOutput("Simulation"),
        textOutput("Infected"),
        textOutput("Death"),
        textOutput("Cured"),
        textOutput("Hospital"),
        textOutput("All"),
        )
        )
)

