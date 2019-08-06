shinyServer(function(input, output,session){
  
  # tab2  
  seasonInput <- eventReactive(input$update, {
    switch(as.character(input$season),
           "2014-2015" = season_1415,
           "2015-2016" = season_1516,
           "2016-2017" = season_1617,
           "2017-2018" = season_1718,
           "2018-2019" = season_1819
    )
  }, ignoreNULL = FALSE)
  
  dateInput <- eventReactive(input$update, { 
    switch(as.character(input$season),
           "2014-2015" = input$ConditionDate1,
           "2015-2016" = input$ConditionDate2,
           "2016-2017" = input$ConditionDate3,
           "2017-2018" = input$ConditionDate4,
           "2018-2019" = input$ConditionDate5
    )
    
    
  }, ignoreNULL = FALSE)
  
  
  all_data_frames <-  eventReactive(input$update, { 
    season <- seasonInput()
    date <- dateInput()
    setup(season, date)
  }, ignoreNULL = FALSE)
  
  output$text1<-renderText({
    alldf <- all_data_frames()
    testtext1 <- as.character(alldf[[1]]$home_team[1])
    paste(testtext1)
    
  })
  output$text_vs<-renderText({
    paste("vs.")
    
  })
  
  output$text2<-renderText({
    alldf <- all_data_frames()
    testtext2 <- as.character(alldf[[1]]$away_team[1])
    paste(testtext2)
    
  })

  
  output$gameflow<-renderPlotly({
    visualize_gameflow(all_data_frames())
    
  })
  
  output$table<-DT::renderDataTable({
    make_table(all_data_frames())
  })
  
  output$text3<-renderText({
    alldf <- all_data_frames()
    testtext3 <- as.character(alldf[[1]]$home_team[1])
    paste(testtext3)
    
  })
  output$network1<-renderVisNetwork({
    alldf <- all_data_frames()
    visualize_home_network(alldf)
    
  })
  
  output$text4<-renderText({
    alldf <- all_data_frames()
    testtext4 <- as.character(alldf[[1]]$away_team[1])
    paste(testtext4)
    
  })
  output$network2<-renderVisNetwork({
    alldf <- all_data_frames()
    visualize_away_network(alldf)
    
  })
  
  output$shot<-renderImage({
    shot_list <- c('11-14','11-15','11-18','11-21','11-22','11-26','11-30','12-15','12-29',
                   '12-31','1-3','1-13','1-17','1-19','1-25','2-4','2-7','2-18','2-21','2-28',
                   '3-4','3-12','3-13','3-20','3-22')
    if(input$season == '2014-2015' && input$ConditionDate1 %in% shot_list){
      list(src = sprintf("%s.jpg",input$ConditionDate1),contentType = 'image/jpg',
           width = 400, height=220,
           alt = "This is alternate text")
    }
    else{
      # data_not_exist()
      list(src="Unavailable.PNG")
    }
    
  }, deleteFile = FALSE)
  
  
  ###### tab3 #####
  
  
  # output$Brief_Intro<-renderText({
  #   paste("Select two games and then click update view in order to generate several visualizations from each match-up. 
  #         The visualizations will be displayed side-by-side so it is very easy to compare the two games. 
  #         Several of the visualizations are interactive so see below for instructions.")
  #   
  # })
  
  seasonInput_compare1 <- eventReactive(input$update_compare, {
    switch(as.character(input$season_compare1),
           "2014-2015" = season_1415,
           "2015-2016" = season_1516,
           "2016-2017" = season_1617,
           "2017-2018" = season_1718,
           "2018-2019" = season_1819
    )
  }, ignoreNULL = FALSE)
  
  dateInput_compare1 <- eventReactive(input$update_compare, { 
    switch(as.character(input$season_compare1),
           "2014-2015" = input$ConditionDate1_compare1,
           "2015-2016" = input$ConditionDate2_compare1,
           "2016-2017" = input$ConditionDate3_compare1,
           "2017-2018" = input$ConditionDate4_compare1,
           "2018-2019" = input$ConditionDate5_compare1
    )
    
    
  }, ignoreNULL = FALSE)
  
  
  all_data_frames_compare1 <-  eventReactive(input$update_compare, { 
    setup(seasonInput_compare1(), dateInput_compare1())
  }, ignoreNULL = FALSE)
  
  
  
  
  
  
  
  # 
  # seasonInput_compare2 <- eventReactive(input$update_compare, {
  #   switch(as.character(input$season_compare2),
  #          "2014-2015" = season_1415,
  #          "2015-2016" = season_1516,
  #          "2016-2017" = season_1617,
  #          "2017-2018" = season_1718,
  #          "2018-2019" = season_1819
  #   )
  # }, ignoreNULL = FALSE)
  # 
  # dateInput_compare2 <- eventReactive(input$update_compare, { 
  #   switch(as.character(input$season_compare2),
  #          "2014-2015" = input$ConditionDate1_compare2,
  #          "2015-2016" = input$ConditionDate2_compare2,
  #          "2016-2017" = input$ConditionDate3_compare2,
  #          "2017-2018" = input$ConditionDate4_compare2,
  #          "2018-2019" = input$ConditionDate5_compare2
  #   )
    
    
  # }, ignoreNULL = FALSE)
  
  
  all_data_frames_compare2 <-  eventReactive(input$update_compare, { 
    season_compare2 <-  switch(as.character(input$season_compare2),
                "2014-2015" = season_1415,
                "2015-2016" = season_1516,
                "2016-2017" = season_1617,
                "2017-2018" = season_1718,
                "2018-2019" = season_1819
         )
    date_compare2 <-  switch(as.character(input$season_compare2),
                "2014-2015" = input$ConditionDate1_compare2,
                "2015-2016" = input$ConditionDate2_compare2,
                "2016-2017" = input$ConditionDate3_compare2,
                "2017-2018" = input$ConditionDate4_compare2,
                "2018-2019" = input$ConditionDate5_compare2
         )
    setup(season_compare2, date_compare2)
  }, ignoreNULL = FALSE)
  
  
  
  
  
  # compare1
  
  output$text1_compare1<-renderText({
    alldf_compare1 <- all_data_frames_compare1()
    testtext1_compare1 <- as.character(alldf_compare1[[1]]$home_team[1])
    testtext2_compare1 <- as.character(alldf_compare1[[1]]$away_team[1])
    paste(testtext1_compare1, "     vs.     ", testtext2_compare1,  sep = "   ")
  })
  
  
  # output$text2_compare1<-renderText({
  #   alldf_compare1 <- all_data_frames_compare1()
  #   testtext2_compare1 <- as.character(alldf_compare1[[1]]$away_team[1])
  #   paste(testtext2_compare1)
  #   
  # })
  output$shots_compare1<-renderImage({
    shot_list <- c('11-14','11-15','11-18','11-21','11-22','11-26','11-30','12-15','12-29',
                   '12-31','1-3','1-13','1-17','1-19','1-25','2-4','2-7','2-18','2-21','2-28',
                   '3-4','3-12','3-13','3-20','3-22')
    if(input$season_compare1 == '2014-2015' && input$ConditionDate1_compare1 %in% shot_list){
    # visualize_shots(input$ConditionDate1_compare1)
      list(src = sprintf("%s.jpg",input$ConditionDate1_compare1),contentType = 'image/jpg',
           width = 400, height=220,
           alt = "This is alternate text")
      }
    else{
      # data_not_exist()
      list(src="Unavailable.PNG")
    }

  }, deleteFile = FALSE)

  output$gameflow_compare1<-renderPlotly({
    alldf_compare1 <- all_data_frames_compare1()
    visualize_gameflow(alldf_compare1)
    
  })
  
  output$table_compare1<-DT::renderDataTable({
    alldf_compare1 <- all_data_frames_compare1()
    make_table( alldf_compare1 )
  })
  
  output$text3_compare1<-renderText({
    alldf_compare1 <- all_data_frames_compare1()
    testtext3_compare1 <- as.character(alldf_compare1[[1]]$home_team[1])
    paste(testtext3_compare1)
    
  })
  output$network1_compare1<-renderVisNetwork({
    alldf_compare1 <- all_data_frames_compare1()
    visualize_home_network(alldf_compare1)
    
  })
  
  output$text4_compare1<-renderText({
    alldf_compare1 <- all_data_frames_compare1()
    testtext4_compare1 <- as.character(alldf_compare1[[1]]$away_team[1])
    paste(testtext4_compare1)
    
  })
  output$network2_compare1<-renderVisNetwork({
    alldf_compare1 <- all_data_frames_compare1()
    visualize_away_network(alldf_compare1)
    
  })
  
  
  
  # compare 2
  
  output$text1_compare2<-renderText({
    alldf_compare2 <- all_data_frames_compare2()
    testtext1_compare2 <- as.character(alldf_compare2[[1]]$home_team[1])
    testtext2_compare2 <- as.character(alldf_compare2[[1]]$away_team[1])
    paste(testtext1_compare2, "     vs.     ", testtext2_compare2,  sep = "   ")
    
  })
  
  
  output$shots_compare2<-renderImage({

    shot_list <- c('11-14','11-15','11-18','11-21','11-22','11-26','11-30','12-15','12-29',
                   '12-31','1-3','1-13','1-17','1-19','1-25','2-4','2-7','2-18','2-21','2-28',
                   '3-4','3-12','3-13','3-20','3-22')
    if(input$season_compare2 == '2014-2015' && input$ConditionDate1_compare2 %in% shot_list){
      # visualize_shots(input$ConditionDate1_compare2)
      list(src=sprintf("%s.jpg", input$ConditionDate1_compare2),contentType = 'image/jpg',
           width = 400, height=220,
           alt = "This is alternate text")
      }
    else{
      # data_not_exist()
      list(src="Unavailable.PNG")
      
    }


  }, deleteFile = FALSE)
  
  output$gameflow_compare2<-renderPlotly({
    visualize_gameflow(all_data_frames_compare2())
    
  })
  
  output$table_compare2<-DT::renderDataTable({
    make_table(all_data_frames_compare2())
    
  })
  
  output$text3_compare2<-renderText({
    alldf_compare2 <- all_data_frames_compare2()
    testtext3_compare2 <- as.character(alldf_compare2[[1]]$home_team[1])
    paste(testtext3_compare2)
    
  })
  output$network1_compare2<-renderVisNetwork({
    alldf_compare2 <- all_data_frames_compare2()
    visualize_home_network(alldf_compare2)
    
  })
  
  output$text4_compare2<-renderText({
    alldf_compare2 <- all_data_frames_compare2()
    testtext4_compare2 <- as.character(alldf_compare2[[1]]$away_team[1])
    paste(testtext4_compare2)
    
  })
  output$network2_compare2<-renderVisNetwork({
    alldf_compare2 <- all_data_frames_compare2()
    visualize_away_network(alldf_compare2)
    
  })
  
  
  
  output$video <- renderUI({
   tags$video(src= "cook.pm4", type="video/mp4", width="350px", height= "350px", controls = "controls")
 })

  
  
  
  
})
