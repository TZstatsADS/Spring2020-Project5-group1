#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    autoInvalidate <- reactiveTimer(500)
    
    data = reactiveVal(NULL)
    new_data = reactiveVal(NULL)
    condition = reactiveVal(NULL)
    v = reactiveVal(NULL)
    infectious_ability = reactiveVal(NULL)
    protection_ability = reactiveVal(NULL)
    data_public = reactiveVal(NULL)
    people_duration = reactiveVal(NULL)
    before_place_info = reactiveVal(NULL)
    
    observeEvent(input$change_N,{
        intialize = intialize_points(input$N, R, P)
        data2 = intialize[[1]]
        
        condition2 = data.frame(condition = rep(1, input$N), duration = rep(0, input$N))
        condition2 = initialize_infector(condition2, 5)
        
        v2 = rnorm(input$N, 2, 0.8)
        v2[v2 < 0] = 0
        
        
        infectious_ability2 = rep(1, input$N)

        protection_ability2 = rnorm(input$N, 1, 0.3)
        protection_ability2[protection_ability2 < 0.05] = 0.05
        protection_ability2[protection_ability2 > 1] = 1

        data_public2 = intialize_public_place(R, P, input$N, Num_public, Hospital_capacity)

        people_duration2 = data.frame(place = rep(0, input$N), place_index = rep(0, input$N), duration = rep(0, input$N))

        before_place_info2 = data.frame(index = NULL, X = NULL, Y = NULL, v = NULL, condition = NULL)

        data(data2)
        new_data(data2)
        condition(condition2)
        v(v2)
        infectious_ability(infectious_ability2)
        protection_ability(protection_ability2)
        data_public(data_public2)
        people_duration(people_duration2)
        before_place_info(before_place_info2)
        
    })
    
    observeEvent(autoInvalidate(), {
        if(is.null(data())) return()
        
        condition2 = condition()
        condition2$duration = condition2$duration + 1
        condition(condition2)

        people_duration2 = people_duration()
        people_duration2$duration = people_duration2$duration + 1
        people_duration(people_duration2)

        # Step 3
        new_data2 = random_walk(data(), v())
        new_data(new_data2)
        # Step 4
        new_data2 = through_wall(data(), new_data(), R, alpha)
        new_data(new_data2)
        # Step 5
        condition2 = infection(new_data(), v(), condition(), infectious_ability(), protection_ability())
        condition(condition2)
        # Step 6
        temp = condition_change(condition(), v(), transform_probability, speed)
        condition(temp[[1]])
        v(temp[[2]])
        # Step 8
        temp = moveto_restaurant(data(), new_data(), data_public(), people_duration(), before_place_info(), v(), condition())
        new_data(temp[[1]])
        before_place_info(temp[[2]])
        v(temp[[3]])
        data_public(temp[[4]])
        people_duration(temp[[5]])
        # Step 9
        temp = outof_restaurant(new_data(), data_public(), people_duration(), before_place_info(), v(), condition())
        new_data(temp[[1]])
        before_place_info(temp[[2]])
        v(temp[[3]])
        data_public(temp[[4]])
        people_duration(temp[[5]])
        data(new_data())
    })
    
    output$Simulation <- renderPlot({
        if(is.null(data())) return()
        
        plot_points(data(), condition(), data_public())

    })
    
})
