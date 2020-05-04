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
    
    session = reactiveValues()
    session$timer = reactiveTimer(Inf)
    session$whole_data = NULL
    session$vaccine = NULL
    session$vacstart = NULL
    
    current = reactiveValues()
    current$data = NULL
    current$new_data = NULL
    current$condition = NULL
    current$v = NULL
    current$infectious_ability = NULL
    current$protection_ability = NULL
    current$data_public = NULL
    current$people_duration = NULL
    current$before_place_info = NULL
    current$Time = NULL
    current$vacday = NULL
    
    initialize = function(){
        
        req(input$Num_public)
        req(input$N)
        
        session$whole_data = data.frame(X = NULL, Y = NULL, Condition = NULL, Time = NULL)
        
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
        
        data_public2 = intialize_public_place(R, P, input$N, input$Num_public, Hospital_capacity)
        
        people_duration2 = data.frame(place = rep(0, input$N), place_index = rep(0, input$N), duration = rep(0, input$N))
        
        before_place_info2 = data.frame(index = NULL, X = NULL, Y = NULL, v = NULL, condition = NULL)
        
        current$data = data2
        current$new_data = data2
        current$condition = condition2
        current$v = v2
        current$infectious_ability = infectious_ability2
        current$protection_ability = protection_ability2
        current$data_public = data_public2
        current$people_duration = people_duration2
        current$before_place_info = before_place_info2
        current$Time = 0
        current$vacday = 0
        
        session$vacstart = runif(1, 8, 15)
        session$whole_data = rbind(session$whole_data, cbind(current$data, Condition = current$condition$condition, Time = rep(0, input$N)))
    }

    forward = function(){
        
        current$Time = current$Time + 1
        current$condition$duration = current$condition$duration + 1

        current$people_duration$duration = current$people_duration$duration + 1
        
        # Step 3
        current$new_data = random_walk(current$data, current$v)
        # Step 4
        current$new_data = through_wall(current$data, current$new_data, R, alpha)
        # Step 5
        current$condition = infection(current$new_data, current$v, current$condition, current$infectious_ability, current$protection_ability, input$pc)
        # Step 6
        temp = condition_change(current$condition, current$v, transform_probability, speed)
        current$condition = temp[[1]]
        current$v = temp[[2]]
        # Step 8
        temp = moveto_restaurant(current$data, current$new_data, current$data_public, current$people_duration, current$before_place_info, current$v, current$condition)
        current$new_data = temp[[1]]
        current$before_place_info = temp[[2]]
        current$v = temp[[3]]
        current$data_public = temp[[4]]
        current$people_duration = temp[[5]]
        # Step 9
        temp = outof_restaurant(current$new_data, current$data_public, current$people_duration, current$before_place_info, current$v, current$condition)
        current$new_data = temp[[1]]
        current$before_place_info = temp[[2]]
        current$v = temp[[3]]
        current$data_public = temp[[4]]
        current$people_duration = temp[[5]]
        
        
        # Step 18
        current$condition = recrudesce(current$condition, current$vacday > session$vacstart)
        # Step 19
        if(input$vaccine){
            current$vacday = current$vacday + 1
            if(current$vacday > session$vacstart)
                current$protection_ability = vaccine(current$condition, current$protection_ability)
        }
        
        
        
        session$whole_data = rbind(session$whole_data, cbind(current$new_data, Condition = current$condition$condition, Time = rep(current$Time, nrow(current$data))))
        
        current$data = current$new_data
        
    }
    
    observeEvent(input$Start, {
        initialize()
    })
    
    observeEvent(input$Next, {
        if(is.null(current$data)) 
            initialize()
        forward()
    })
    
    observeEvent(input$Auto,{
        session$timer = reactiveTimer(1000)
        observeEvent(session$timer(),{
            if(is.null(current$data)) 
                initialize()
            forward()
        })
    })
    
    observeEvent(input$Stop,{
        session$timer = reactiveTimer(Inf)
    })
    
    observeEvent(input$Skip,{
        if(is.null(current$data)) 
            initialize()
        for(i in 1:input$SkipNum)
            forward()
    })
    
    observeEvent(input$vaccine, {
        if(input$vaccine)
            session$vaccine = current$Time
    })
    
    
    
    output$Simulation = renderPlot({
        
        if(is.null(current$data)) return()
        
        plot_points(current$data, current$condition, current$data_public)

    })
    
    output$downloadgif = downloadHandler(
        filename = function() {
            paste0(input$gifname, '.gif')
        },
        content = function(file) {
            generate_gif(session$whole_data, current$data_public, current$Time, file)
        }
    )
    

    
    output$Infected = renderText(
        paste0('Infected:', sum(current$condition$condition %in% c(2:5)))
    )
    
    output$Death = renderText(
        paste0('Death:', sum(current$condition$condition == 6))
    )
    
    output$Cured = renderText(
        paste0('Cured:', sum(current$condition$condition == 7))
    )
    
})










