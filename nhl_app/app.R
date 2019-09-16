################## NHL WIN PREDICTION APP ################## 
################## MARIE ERWOOD ################## 
################## MSC DATA SCIENCE PROJECT ################## 

library(shiny)

ui <- fluidPage(
  
  titlePanel("NHL Win Predictions"),
  
  tabsetPanel(
    
    tabPanel("Team Summary", fluid = TRUE,
             
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("home_team1", "Home Team", team_names),
                 selectInput("season1", "Season", season)
                 
               ),
               
               
               mainPanel(
                 dataTableOutput("summary1")
               )
             )
        ),
    
    tabPanel("Game Predictions", fluid = TRUE,
             
             
             sidebarLayout(
               sidebarPanel(
                 selectInput("home_team2", "Home Team", team_names),
                 selectInput("away_team2", "Away Team", team_names),
                 selectInput("acc", "Show model accuracy?", c("Yes", "No"))
                 
               ),
               
               
               mainPanel(
                 br(),
                 dataTableOutput("preds"),
                 br(),
                 textOutput("accuracy"),
                 br(),
                 dataTableOutput("tot_scores"),
                 br(), 
                 plotOutput("score_dist")
               )
             )
        )
  )
  
)
    
    
server <- function(input, output) {
      
      output$summary1 <- renderDataTable({
        
        nst %>%
          select(season, gameDate, Team, everything())%>%
          filter(Team==input$home_team1)%>%
          filter(season==input$season1)
      })
      
      
      output$summary2 <- renderDataTable({
        
        nst_avg %>%
          
          filter(Team==input$home_team2)%>%
          filter(awayTeam==input$away_team2)%>%
          filter(season==2018)
      })
      
      output$accuracy <- renderText({
        
        a <- e_res$overall[1]
        
        ifelse(input$acc=="Yes", HTML(paste0("Accuracy: ", a)), "")
        
      })
      
      output$preds <- renderDataTable({
        
        preds_final %>%
          filter(homeTeam==input$home_team2)%>%
          filter(awayTeam==input$away_team2)
      })
      
      output$score_dist <- renderPlot({
        
        points_boot%>%
          filter(id == input$home_team2)%>%
          ggplot(aes(x = sums_scaled, fill = id))+
          geom_bar()+
          xlab("Points prediction")+
          ylab("Outcome frequency (50,000 iterations)")+
          ylim(0,6000)+
          theme_light()+
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
          ggtitle(paste0("Overall season points predictions: ", 
                         points_boot$id[points_boot$id==input$home_team2]))
        
      })
      
      output$tot_scores <- renderDataTable({
        
        points_boot_summary%>%
          select(Team:point_prediction)%>%
          filter(Team == input$home_team2)
        
      })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
    
