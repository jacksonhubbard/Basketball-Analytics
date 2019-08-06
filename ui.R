source("Master_Functions.R")

# pkgs
library(shiny)
library(shinyWidgets)
library(shinythemes)


  
# ui
ui <- tagList(

shinyUI(

  # layout
  navbarPage('Basketball Analytics Pipeline',
             theme = shinytheme("cosmo"),
             
            
             # tab 1: landing page
             tabPanel(title = "Home", 
                      
                      tags$head(
                        tags$link(href="app.css", rel="stylesheet", type="text/css")
                      ),
                      
                      # parent container
                      tags$div(class="landing-wrapper",
                               
                               # child element 1: images
                               tags$div(class="landing-block background-content",
                                        
                                        # background
                                        img(src="Duke_Basketball_13.png")
                                        
                               ),
                               
                               # child element 2: content
                               tags$div(class="landing-block foreground-content",
                                        tags$div(class="foreground-text",
                                                 tags$h1("WELCOME"),
                                                 tags$p("This shiny app demonstrates
                                                        how to analyze a basketball game.")
                                                 
                                                 )
                               )
                               )
                      ),
             
             
             ##### tab 2: single game #####
             tabPanel("Single Game",
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        
                        column(3, 
                               img( height =130, width =130,
                                    src= "https://ojwgq1ostm42ulxuw45kfbt8-wpengine.netdna-ssl.com/reunions/wp-content/uploads/sites/9/2019/01/logo_-duke-university-blue-devils-devil-head-d.png",
                                    style="display: block; margin-left: auto; margin-right: auto;"
                               )
                        ),
                        column(1),
                        column(2,
                               textOutput("text1"), 
                               tags$head(tags$style("#text1{
                                                    color: '#868e96' ;
                                                    margin-top: auto;
                                                    margin-bottom: auto;
                                                    margin-right: 0 rem;
                                                    margin-left: 1 rem;
                                                    font-weight: 460;
                                                    line-height: 5.2;
                                                    font-size: 2.820625rem ;
                                                    font-family: Source Sans Pro;
                                                    font-style: normal;
                                                    }")
                                           )
                               ),
                        column(2,textOutput("text_vs"), 
                               tags$head(tags$style("#text_vs{
                                                    color: '#868e96' ;
                                                    margin-top: auto;
                                                    margin-bottom: auto;
                                                    margin-right: 0 rem;
                                                    margin-left: 1 rem;
                                                    font-weight: 460;
                                                    line-height: 5.2;
                                                    font-size: 2.820625rem ;
                                                    font-family: Source Sans Pro;
                                                    font-style: normal;
                                                    }")
                               )),
                        column(3, 
                               textOutput("text2"),
                               tags$head(tags$style("#text2{
                                                    color: '#868e96';
                                                    margin-top: auto;
                                                    margin-bottom: auto;
                                                    margin-right: 0 rem;
                                                    margin-left: 1rem;
                                                    font-weight: 460;
                                                    line-height: 5.2;
                                                    font-size: 2.820625rem ;
                                                    font-family: Source Sans Pro;
                                                    font-style: normal;
                                                    }")
                                           )
                               )
                               ),
                      
                      # App title ----
                      
                      tags$br(),
                      
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        width = 3,
                        
                        # Input: Selector for choosing dataset ----
                        
                        selectInput(inputId = "season",
                                    label = "Choose a season:",
                                    choices = c("2014-2015", "2015-2016", "2016-2017", 
                                                "2017-2018", "2018-2019")),
                        
                        tags$head(tags$style("
                                             .jhr{
                                             display: inline;
                                             vertical-align: middle;
                                             padding-left: 10px;
                                             
                                             }")),
                          # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season == '2014-2015'",
                          
                          pickerInput(inputId = "ConditionDate1",
                                      label = "Choose a game",
                                      choices = season_1415$date,
                                      choicesOpt = list(content =logofunction(season_1415)))
                        ),
                        
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season == '2015-2016'",
                          
                          pickerInput(inputId = "ConditionDate2",
                                      label = "Choose a game",
                                      choices = season_1516$date,
                                      choicesOpt = list(content =logofunction(season_1516)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season == '2016-2017'",
                          
                          pickerInput(inputId = "ConditionDate3",
                                      label = "Choose a game",
                                      choices = season_1617$date,
                                      choicesOpt = list(content =logofunction(season_1617)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season == '2017-2018'",
                          
                          pickerInput(inputId = "ConditionDate4",
                                      label = "Choose a game",
                                      choices = season_1718$date,
                                      choicesOpt = list(content =logofunction(season_1718)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season == '2018-2019'",
                          
                          pickerInput(inputId = "ConditionDate5",
                                      label = "Choose a game",
                                      choices = season_1819$date,
                                      choicesOpt = list(content =logofunction(season_1819)))
                        ),
                        
                        actionButton("update", "Update View"),
                        h6("Select any two games you like and then click Update View to compare them.
          Several of the visualizations are interactive, please see instructions below.")
                        
                       
                        ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        fluidRow(

                          column(6, h3("Game Flow"),
                                 plotlyOutput("gameflow")
                          ),
                          column(6, h3("Game Summary Stats"),
                                 tags$br(),
                                 tags$br(),
                                 tags$br(),
                                 DT::dataTableOutput("table"),
                                 tags$br()
                          )
                          
                        ),
                        
                        fluidRow(
                          h6("Hovering over the game flow chart will show a legend of each team’s score at the respective time. 
                             In order to expand upon a certain period of time, click and drag to form a rectangle around the region. Double click to reset.")),
                        
                        fluidRow(
                          h3('Points and Assist Network'),
                          
                          # Assit Network Visualization
                          column(6, 
                                 textOutput("text3"),
                                 tags$head(tags$style("#text3{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network1")),
                          column(6, 
                                 textOutput("text4"),
                                 tags$head(tags$style("#text4{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network2"))
                                
                         
                                 ),
                        fluidRow(
                          h6("In the Points and Assists Network, 
                          the size of each node reflects how many points that player scored 
                          and the width and color of each edge represents the number of directed assists from player A to player B. 
                          Hover over a node or an edge to see more information. 
                          There is also a temporal/dynamic version of this graph to reflect how the network grows over the course of the game.")),
                        fluidRow(
                          h3("Shot Charts"),
                          column(6,
                                 
                                 tags$br(),
                                 tags$br(),
                                 imageOutput("shot")
                                 
                          ),
                          column(6,
                                 
                                 h6("On the shot charts graph, the court is divided into several zones. 
                                     Each team’s shooting percentage within the zone is displayed."))
                          )

                        
                                 )
                                 ),
             
             # tabPanel("Video",
             #          mainPanel( uiOutput("video"),
             #                     tags$video(src= "cook.mp4", type="video/mp4", width="350px", height= "350px", controls = "controls")
             #                     
             #                     )
             #          ),
             
             ##### tab 3: Compare two games #####
             tabPanel("Compare Games",
                      tags$br(),
                      tags$br(),
                      fluidRow(
                        
                        column(3, 
                               img( height =130, width =130,
                                    src= "https://ojwgq1ostm42ulxuw45kfbt8-wpengine.netdna-ssl.com/reunions/wp-content/uploads/sites/9/2019/01/logo_-duke-university-blue-devils-devil-head-d.png",
                                    style="display: block; margin-left: auto; margin-right: auto;"
                               )
                        ),
                        
                        column(3,
                               textOutput("text1_compare1"),
                               tags$head(tags$style("#text1_compare1{
                                                    color: '#868e96';
                                                    margin-top: 0rem;
                                                    margin-bottom: 0.5rem;
                                                    font-weight: 380;
                                                    line-height: 3.2;
                                                    font-size: 2.620625rem ;
                                                    font-family: Source Sans Pro;
                                                    font-style: normal;
                                                    }"))
                                
                               
                               ),
                        
                        column(1),
                        
                        
                        column(3,
                               textOutput("text1_compare2"),
                               tags$head(tags$style("#text1_compare2{
                                                    color: '#868e96';
                                                    margin-top: 0rem;
                                                    margin-bottom: 0.5rem;
                                                    font-weight: 380;
                                                    line-height: 3.2;
                                                    font-size: 2.620625rem ;
                                                    font-family: Source Sans Pro;
                                                    font-style: normal;
                                                    }"))
                                 )
                        
                        # column(2, 
                        #        textOutput("text2_compare2"),
                        #        tags$head(tags$style("#text2_compare2{
                        #                             color: '#868e96';
                        #                             margin-top: 0rem;
                        #                             margin-bottom: 0.5rem;
                        #                             font-weight: 420;
                        #                             line-height: 3.2;
                        #                             font-size: 1.820625rem ;
                        #                             font-family: Source Sans Pro;
                        #                             font-style: normal;
                        #                             }"))
                        # )
                               ),
                      
                      # App title ----
                      
                      tags$br(),
                      
                      # Sidebar panel for inputs ----
                      sidebarPanel(
                        width = 3,
                        
                        # Input (Compare1): Selector for choosing dataset ----
                        
                        selectInput(inputId = "season_compare1",
                                    label = "Choose a season:",
                                    choices = c("2014-2015", "2015-2016", "2016-2017", 
                                                "2017-2018", "2018-2019")),
                        
                        # tags$head(tags$style("
                        #                      .jhr{
                        #                      display: inline;
                        #                      vertical-align: middle;
                        #                      padding-left: 10px;
                        #                      }")),
                        
                        
                        
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare1 == '2014-2015'",
                          
                          pickerInput(inputId = "ConditionDate1_compare1",
                                      label = "Choose a game",
                                      
                                      choices = season_1415$date,
                                      choicesOpt = list(content =logofunction(season_1415)))
                        ),
                        
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare1 == '2015-2016'",
                          
                          pickerInput(inputId = "ConditionDate2_compare1",
                                      label = "Choose a game",
                                      choices = season_1516$date,
                                      choicesOpt = list(content =logofunction(season_1516)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare1 == '2016-2017'",
                          
                          pickerInput(inputId = "ConditionDate3_compare1",
                                      label = "Choose a game",
                                      choices = season_1617$date,
                                      choicesOpt = list(content =logofunction(season_1617)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare1 == '2017-2018'",
                          
                          pickerInput(inputId = "ConditionDate4_compare1",
                                      label = "Choose a game",
                                      choices = season_1718$date,
                                      choicesOpt = list(content =logofunction(season_1718)))
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare1 == '2018-2019'",
                          
                          pickerInput(inputId = "ConditionDate5_compare1",
                                      label = "Choose a game",
                                      choices = season_1819$date,
                                      choicesOpt = list(content =logofunction(season_1819)))
                        ),
                        
                        
                        
                        
                        tags$br(),
                        tags$br(),
                        # Input (Compare2): Selector for choosing dataset ----
                        
                        selectInput(inputId = "season_compare2",
                                    label = "Choose a season:",
                                    choices = c("2014-2015", "2015-2016", "2016-2017", 
                                                "2017-2018", "2018-2019")),
                        
                        # tags$head(tags$style("
                        #                      .jhr{
                        #                      display: inline;
                        #                      vertical-align: middle;
                        #                      padding-left: 10px;
                        #                      }")),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare2 == '2014-2015'",
                          
                          pickerInput(inputId = "ConditionDate1_compare2",
                                      label = "Choose a game",
                                      choices = season_1415$date,
                                      choicesOpt = list(content =logofunction(season_1415))
                                      
                          )
                          
                        ),
                        
                        # Conditional Panel: Based on the season
                        
                        conditionalPanel(
                          condition = "input.season_compare2 == '2015-2016'",
                          
                          pickerInput(inputId = "ConditionDate2_compare2",
                                      label = "Choose a game",
                                      choices = season_1516$date,
                                      choicesOpt = list(content =logofunction(season_1516))
                          )
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare2 == '2016-2017'",
                          
                          pickerInput(inputId = "ConditionDate3_compare2",
                                      label = "Choose a game",
                                      choices = season_1617$date,
                                      choicesOpt = list(content =logofunction(season_1617))
                          )
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare2 == '2017-2018'",
                          
                          pickerInput(inputId = "ConditionDate4_compare2",
                                      label = "Choose a game",
                                      choices = season_1718$date,
                                      choicesOpt = list(content =logofunction(season_1718))
                          )
                        ),
                        # Conditional Panel: Based on the season
                        conditionalPanel(
                          condition = "input.season_compare2 == '2018-2019'",
                          
                          pickerInput(inputId = "ConditionDate5_compare2",
                                      label = "Choose a game",
                                      choices = season_1819$date,
                                      choicesOpt = list(content =logofunction(season_1819))
                          )
                        ),
                        tags$head(
                          tags$style(HTML("
                                          .btn {
                                          background-color: white;
                                          color: black;
                                          text-align: center;
                                          text-decoration: none;
                                          display: inline-block;
                                          font-size: 14px;
                                          border-radius: 3px;
                                          border-color: #D3D3D3;
                                          
                                          
                                          }
                                          
                                          "))
                          ),
                        actionButton("update_compare", "Update View"),
                        tags$br(),
                        tags$br(),
                      
                        # textOutput("Brief_Intro")
                        h6("Select any two games you like and then click Update View to compare them.
          Several of the visualizations are interactive, please see instructions to the right")
                          ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(  
                        
                       
                          
       
                        fluidRow(
                          h3("Game Flow"),
                    
                          h6("Hovering over the game flow chart will show a legend of each team’s score at the respective time. 
                             In order to expand upon a certain period of time, click and drag to form a rectangle around the region. Double click to reset."),
                   
                          
                          column(6,
                                 plotlyOutput("gameflow_compare1")
              
                          ),
                          column(6, 
                                 plotlyOutput("gameflow_compare2")
                          )
                          
                          
                        ),
                     
                        
                        fluidRow(
                          h3("Game Summary Stats"),
                          column(6,
                                 tags$br(),
                                 DT::dataTableOutput("table_compare1"),
                                 tags$br()
                          ),
                          column(6, 
                                 tags$br(),
                                 DT::dataTableOutput("table_compare2"),
                                 tags$br()
                          )
                          
                        ),
                        
                        conditionalPanel(
                        condition = "input.season_compare1 == '2014-2015' || input.season_compare2 == '2014-2015' ",
                      
                        fluidRow(
                            h3("Shot Charts"),
                            h6("On the shot charts graph, the court is divided into several zones. 
                                     Each team’s shooting percentage within the zone is displayed."),
                           column(6,
                                  # plotOutput("shots_compare1")
                                  tags$br(),
                                  tags$br(),
                                  imageOutput("shots_compare1")
                                 
                          ),
  
                            column(6, 
                                  # plotOutput("shots_compare2")
                                  tags$br(),
                                  tags$br(),
                                 imageOutput("shots_compare2")
                          )
                          
                          

                        )),
                        
                        
                        
                        fluidRow(
                          h3('Points and Assist Network'),
                          
                          h6("In the Points and Assists Network, 
                          the size of each node reflects how many points that player scored 
                          and the width and color of each edge represents the number of directed assists from player A to player B. 
                          Hover over a node or an edge to see more information. 
                          There is also a temporal/dynamic version of this graph to reflect how the network grows over the course of the game."),
                          
                          
                         
                          # Assit Network Visualization
                          column(6, 
                                 textOutput("text3_compare1"),
                                 tags$head(tags$style("#text3_compare1{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network1_compare1")
                                
                                 ),
                          
                          column(6, 
                                 textOutput("text3_compare2"),
                                 tags$head(tags$style("#text3_compare2{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network1_compare2"))
                          
                                 ),
                        fluidRow(
                          column(6, 
                                 textOutput("text4_compare1"),
                                 tags$head(tags$style("#text4_compare1{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network2_compare1")),
                          column(6, 
                                 textOutput("text4_compare2"),
                                 tags$head(tags$style("#text4_compare2{
                                                      color: '#868e96';
                                                      margin-top: 0rem;
                                                      margin-bottom: 0.5rem;
                                                      font-weight: 160;
                                                      line-height: 1.2;
                                                      font-size: 1.820625rem ;
                                                      font-family: Source Sans Pro;
                                                      font-style: normal;
                                                      }"
                                   )
                                 ),
                                 visNetworkOutput("network2_compare2"))
                          
                          
                                 )
                        
                                 )
                          )
             
             
             
                                 )
                                 )
)

