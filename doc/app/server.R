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
    session$social_distance = FALSE
    session$close_restaurant = FALSE
    session$quarantine = FALSE
    session$close_station = FALSE
    session$last_quarantine = 0
    session$expend_hospital = 0
    session$RN = NULL
    session$NN = NULL
    session$Num_publicN = NULL
    session$centers = NULL
    session$Num_city = NULL
    
    
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
    current$before_hos_info = NULL
    current$v_initial = NULL
    current$travel_info = NULL
    current$Travler = NULL
    current$Time = NULL
    current$vacday = NULL
    current$ir1 = 0
    current$ir2 = 0
    current$ir3 = 0
    current$station_avail = 0
    
    initialize = function(){
        
        req(input$Num_public)
        req(input$N)
        req(input$Num_city)
        session$Num_city = input$Num_city
        
        session$whole_data = data.frame(X = NULL, Y = NULL, Condition = NULL, Time = NULL, City = NULL)
        session$centers = generate_center(R, session$Num_city)
        
        if(session$Num_city == 1){
            session$NN = input$N
            session$Num_publicN = input$Num_public
        }
        session$RN = list()
        session$RN[[1]] = R
        if(session$Num_city > 1){
            session$NN = c(input$N, round(runif(session$Num_city -1, 0.4, 0.7)*input$N))
            session$Num_publicN = c(input$Num_public, round(runif(session$Num_city -1, 0.4, 0.7)*input$Num_public))
            for(i in 2:session$Num_city){
                session$RN[[i]] = runif(1, 0.5, 0.9)*R
            }
        }
        
        current$data = list()
        current$new_data = list()
        current$condition = list()
        current$v = list()
        current$infectious_ability = list()
        current$protection_ability = list()
        current$data_public = list()
        current$people_duration = list()
        current$before_place_info = list()
        current$before_hos_info = list()
        current$v_initial = list()
        current$travel_info = list()
        
        
        
        for(i in 1:session$Num_city ){
            
            intialize = intialize_points(session$NN[i], session$RN[[i]], P, session$centers$x0[i], session$centers$y0[i])
            data2 = intialize[[1]]
            
            condition2 = data.frame(condition = rep(1, session$NN[i]), duration = rep(0, session$NN[i]))
            
            v2 = rnorm(session$NN[i], 2, 0.8)
            v2[v2 < 0.1] = 0.1
            
            infectious_ability2 = rep(1, session$NN[i])
            
            protection_ability2 = rnorm(session$NN[i], 1, 0.3)
            protection_ability2[protection_ability2 < 0.05] = 0.05
            protection_ability2[protection_ability2 > 1] = 1
            
            if(input$mask)
                protection_ability2 = protection_ability2/2
            
            data_public2 = intialize_public_place(session$RN[[i]], P, session$NN[i], session$Num_publicN[i], Hospital_capacity, session$centers$x0[i], session$centers$y0[i])
            
            people_duration2 = data.frame(place = rep(0, session$NN[i]), place_index = rep(0, session$NN[i]), duration = rep(0, session$NN[i]))
            
            before_place_info2 = data.frame(index = NULL, X = NULL, Y = NULL, v = NULL, condition = NULL)
            
            before_hos_info2 = data.frame(index = NULL, X = NULL, Y = NULL, v = NULL, condition = NULL)
            
            travel_info2 = data.frame(index = NULL, UNI = NULL, destination = NULL)
            
            current$data[[i]] = data2
            current$new_data[[i]] = data2
            current$condition[[i]] = condition2
            current$v[[i]] = v2
            current$infectious_ability[[i]] = infectious_ability2
            current$protection_ability[[i]] = protection_ability2
            current$data_public[[i]] = data_public2
            current$people_duration[[i]] = people_duration2
            current$before_place_info[[i]] = before_place_info2
            current$before_hos_info[[i]] = before_hos_info2
            current$v_initial[[i]] = v2
            current$travel_info[[i]] = travel_info2
            current$Travler = c(NULL)
            current$Time = 0
            current$vacday = 0
            
            session$last_quarantine = 0
            session$expend_hospital = 0
            session$vacstart = runif(1, 8, 15)
            session$whole_data = rbind(session$whole_data, cbind(current$data[[i]], Condition = current$condition[[i]]$condition, Time = rep(0, session$NN[i]), City = rep(i, session$NN[i])))
        
        }
        current$condition[[1]] = initialize_infector(current$condition[[1]], 5)
    }

    forward = function(){
        
        current$Time = current$Time + 1
        
        if(input$vaccine)
            current$vacday = current$vacday + 1
        
        current$ir1 = 0
        current$ir2 = 0
        current$ir3 = 0
        current$station_avail = 0
        for(i in 1:session$Num_city){
            current$ir1 = current$ir1 + sum(current$condition[[i]]$condition == 1)
            current$ir2 = current$ir2 + sum(current$condition[[i]]$condition == 2)
            current$ir3 = current$ir3 + sum(current$condition[[i]]$condition == 3)
            current$station_avail = current$station_avail + sum(current$condition[[i]]$condition %in% c(1,2,3,7))
        }
        current$ir1 = current$ir1/current$station_avail
        current$ir2 = current$ir2/current$station_avail
        current$ir3 = current$ir3/current$station_avail
        
        
        for(i in 1:session$Num_city){
            
            current$condition[[i]]$duration = current$condition[[i]]$duration + 1
            current$people_duration[[i]]$duration = current$people_duration[[i]]$duration + 1
            
            # Step 3
            current$new_data[[i]] = random_walk(current$data[[i]], current$v[[i]])
            # Step 4
            current$new_data[[i]] = through_wall(current$data[[i]], current$new_data[[i]], session$RN[[i]], alpha, session$centers$x0[i], session$centers$y0[i])
            # Step 5
            current$condition[[i]] = infection(current$new_data[[i]], current$v[[i]], current$condition[[i]], current$infectious_ability[[i]], current$protection_ability[[i]], input$pc, session$social_distance, social_distance)
            # Step 6
            temp = condition_change(current$condition[[i]], current$v[[i]], transform_probability, speed)
            current$condition[[i]] = temp[[1]]
            current$v[[i]] = temp[[2]]
            
            #Step 15
            if(!session$close_station & session$Num_city > 1){
                current$condition[[i]] = visitors(current$people_duration[[i]], current$condition[[i]], current$ir1, current$ir2, current$ir3)
                
                temp = moveout_station(current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], session$centers$x0[i], session$centers$y0[i], session$RN[[i]])
                current$new_data[[i]] = temp[[1]]
                current$data_public[[i]] = temp[[2]]
                current$people_duration[[i]] = temp[[3]]
                
                temp = movein_station(current$new_data[[i]], current$data_public[[i]], current$condition[[i]], current$people_duration[[i]], city = i, session$Num_city)
                current$new_data[[i]] = temp[[1]]
                current$data_public[[i]] = temp[[2]]
                current$people_duration[[i]] = temp[[3]]
            }

            # Step 8
            if(!session$close_restaurant){
                temp = moveto_restaurant(current$data[[i]], current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], current$before_place_info[[i]], current$v[[i]], current$condition[[i]])
                current$new_data[[i]] = temp[[1]]
                current$before_place_info[[i]] = temp[[2]]
                current$v[[i]] = temp[[3]]
                current$data_public[[i]] = temp[[4]]
                current$people_duration[[i]] = temp[[5]]
            }
            # Step 9
            if(!session$close_restaurant) # Step 13
                temp = outof_restaurant(current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], current$before_place_info[[i]], current$v[[i]], current$condition[[i]])
            else
                temp = close_restaurant(current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], current$before_place_info[[i]], current$v[[i]], current$condition[[i]])
            current$new_data[[i]] = temp[[1]]
            current$before_place_info[[i]] = temp[[2]]
            current$v[[i]] = temp[[3]]
            current$data_public[[i]] = temp[[4]]
            current$people_duration[[i]] = temp[[5]]
            # Step 10
            temp = moveto_hospital(current$data[[i]], current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], current$before_hos_info[[i]], current$v[[i]], current$condition[[i]])
            current$new_data[[i]] = temp[[1]]
            current$before_hos_info[[i]] = temp[[4]]
            current$v[[i]] = temp[[5]]
            current$data_public[[i]] = temp[[2]]
            current$people_duration[[i]] = temp[[3]]
            # Step 11
            current$condition[[i]] = symptom_hos_change(current$condition[[i]], current$people_duration[[i]])
            # Step 12
            temp = moveout_hospital(current$data[[i]], current$new_data[[i]], current$data_public[[i]], current$people_duration[[i]], current$before_hos_info[[i]], current$v[[i]], current$condition[[i]])
            current$new_data[[i]] = temp[[1]]
            current$before_hos_info[[i]] = temp[[4]]
            current$v[[i]] = temp[[5]]
            current$data_public[[i]] = temp[[2]]
            current$people_duration[[i]] = temp[[3]]
            # Step 13
            ## Quarantine
            if(session$quarantine == TRUE){
                temp = start_Quarantine(current$v[[i]], current$v_initial[[i]], current$people_duration[[i]], Quarantine_ratio, condition = current$condition[[i]]$condition)
                current$v[[i]] = temp[[1]]
                session$last_quarantine = temp[[2]]
            }
            if(session$quarantine == FALSE & session$last_quarantine == 1){
                temp = end_Quarantine(current$v[[i]], current$v_initial[[i]], current$people_duration[[i]], condition = current$condition[[i]]$condition)
                current$v[[i]] = temp[[1]]
                session$last_quarantine = temp[[2]]
            }
            # Step 14
            temp = close_contacts(current$data[[i]], current$new_data[[i]], current$v[[i]], current$v_initial[[i]], current$people_duration[[i]], condition = current$condition[[i]]$condition)
            current$v[[i]] = temp[[1]]
            current$people_duration[[i]] = temp[[2]]
            
            temp = close_contacts_end14(current$data[[i]], current$new_data[[i]], current$v[[i]], current$v_initial[[i]], current$people_duration[[i]], condition = current$condition[[i]]$condition)
            current$v[[i]] = temp[[1]]
            current$people_duration[[i]] = temp[[2]]
            
            current$protection_ability[[i]] = protection_ability_quarantine(current$v[[i]], current$people_duration[[i]], current$protection_ability[[i]])
            
            
            # Step 18
            current$condition[[i]] = recrudesce(current$condition[[i]], current$vacday > session$vacstart)
            # Step 19
            if(current$vacday > session$vacstart)
                current$protection_ability[[i]] = vaccine(current$condition[[i]], current$protection_ability[[i]])
            
            
            
            
            session$whole_data = rbind(session$whole_data, cbind(current$new_data[[i]], Condition = current$condition[[i]]$condition, Time = rep(current$Time, nrow(current$new_data[[i]])), City = rep(i, nrow(current$new_data[[i]]))))
            
            current$data[[i]] = current$new_data[[i]]
            
        }
        
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
    
    observeEvent(input$mask,{
        if(is.null(session$Num_city))
            return()
        
        if(input$mask){
            for(i in 1:session$Num_city){
                current$protection_ability[[i]] = current$protection_ability[[i]]/2
                current$protection_ability[[i]][current$protection_ability[[i]] < 0.05] = 0.05
            }
        } else{
            for(i in 1:session$Num_city){
                current$protection_ability[[i]] = current$protection_ability[[i]]*2
            }
            
        }
            
    })
    
    observeEvent(input$socialdist, {
        session$social_distance = input$socialdist
    })
    
    observeEvent(input$closerest, {
        session$close_restaurant = input$closerest
    })
    
    observeEvent(input$quarantine, {
        session$quarantine = input$quarantine
    })
    
    observeEvent(input$expendhos, {
        session$expend_hospital = session$expend_hospital + 1
        if(session$expend_hospital <= 5){
            for(i in 1:session$Num_city){
                 current$data_public[[i]][current$data_public[[i]]$Class == 1, 'Capacity'] = round(current$data_public[[i]][current$data_public[[i]]$Class == 1, 'Capacity'] * 1.5)
            }
        }
    })
    
    observeEvent(input$vaccine, {
        if(input$vaccine)
            session$vaccine = current$Time
    })
    
    observeEvent(input$closestation, {
        session$close_station = input$closestation
    })
    
    output$Simulation = renderPlot({
        
        if(is.null(current$data)) return()
        
        data_now = data.frame(NULL)
        condition_now = data.frame(NULL)
        data_public_now = data.frame(NULL)
        
        for(i in 1:session$Num_city ){
            data_now = rbind(data_now, current$data[[i]])
            condition_now = rbind(condition_now, current$condition[[i]])
            data_public_now = rbind(data_public_now, current$data_public[[i]])
        }
        
        xmax = max(session$centers$x0) + max(R)
        xmin = min(session$centers$x0) - max(R)
        ymax = max(session$centers$y0) + max(R)
        ymin = min(session$centers$y0) - max(R)
        
        plot_points(data_now, condition_now, data_public_now, c(xmin, xmax), c(ymin,ymax), session$centers)

    })
    
    output$downloadgif = downloadHandler(
        filename = function() {
            paste0(input$gifname, '.gif')
        },
        content = function(file) {
            dp = data.frame(NULL)
            for(i in 1:session$Num_city)
                dp = rbind(dp, current$data_public[[i]])
            
            xmax = max(session$centers$x0) + max(R)
            xmin = min(session$centers$x0) - max(R)
            ymax = max(session$centers$y0) + max(R)
            ymin = min(session$centers$y0) - max(R)
            
            generate_gif(session$whole_data, dp, current$Time, file, c(xmin, xmax), c(ymin,ymax))
        }
    )
    
    print_infected = reactive({
        if(is.null(current$data)) return()
        print_text = paste0('Infected: ')
        for(i in 1:session$Num_city)
            print_text = paste0(print_text, 'City:', i, ' Num:', sum(current$condition[[i]]$condition %in% c(2:5)), ';   ')
        print_text
    })
    
    print_death = reactive({
        if(is.null(current$data)) return()
        print_text = paste0('Death: ')
        for(i in 1:session$Num_city)
            print_text = paste0(print_text, 'City:', i, ' Num:', sum(current$condition[[i]]$condition == 6), ';   ')
        print_text
    })
    
    print_cured = reactive({
        if(is.null(current$data)) return()
        print_text = paste0('Cured: ')
        for(i in 1:session$Num_city)
            print_text = paste0(print_text, 'City:', i, ' Num:', sum(current$condition[[i]]$condition == 7), ';   ')
        print_text
    })
    
    print_hospital = reactive({
        if(is.null(current$data)) return()
        print_text = paste0('Hospital: \n')
        for(i in 1:session$Num_city)
            print_text = paste0(print_text, 'City:', i, ' Capacity:', sum(current$data_public[[i]][current$data_public[[i]]$Class == 1, 'Capacity']), ' Current:', sum(current$data_public[[i]][current$data_public[[i]]$Class == 1, 'Current']), ';   \n')
        print_text
    })
    
    
    output$Time = renderText(
        paste0('Day ', current$Time)
    )
    
    output$Infected = renderText(
        print_infected()
    )
    
    output$Death = renderText(
        print_death()
    )
    
    output$Cured = renderText(
        print_cured()
    )
    
    output$Hospital = renderText(
        print_hospital()
    )
    
    output$Author = renderText(
        paste0('Create by Shuxin Chen, Junyan Guo, Vikki Sui, Jinxu Xiang and Ziqin Zhao.')
    )
})










