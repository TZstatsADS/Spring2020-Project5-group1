

library(shiny)
library(shinydashboard)

header <- dashboardHeader(title='Random Walk')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Home', 
                 tabName = 'Home', 
                 icon = icon('home')
        )
    )
)

body <- dashboardBody(
    tags$head(
       
    ),
    tabItems(
        # Home
        tabItem(tabName='Home',
                fluidPage(
                    
                    # Application title
                    titlePanel("Simulation"),
                    # Show a plot of the generated distribution
                    sliderInput(inputId = 'N', 
                                label = 'Number of points', 
                                value = 3000, 
                                min = 300, 
                                max = 10000),
                    actionButton(inputId = 'change_N', 
                                 label = 'Select N'),
                    plotOutput("Simulation"),
                )
        )
    )
)
    
ui <- dashboardPage(
    skin='black',
    header=header,
    sidebar=sidebar,
    body=body
)