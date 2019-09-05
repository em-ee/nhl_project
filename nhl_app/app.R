
library(shiny)

ui <- fluidPage(
   

   titlePanel("NHL Game Predictions"),
   
 
   sidebarLayout(
      sidebarPanel(
         selectInput("home_team", "Home Team", team_names),
         selectInput("away_team", "Away Team", team_names),
         selectInput("season", "Season", season)

      ),
      

      mainPanel(
         dataTableOutput("preds"),
         dataTableOutput("avgs")
      )
   )
)


server <- function(input, output) {
  
  output$preds <- renderDataTable({
    
    nst_avg %>%
      select(season:pred_win)%>%
      filter(Team==input$home_team)%>%
      filter(awayTeam==input$away_team)%>%
      filter(season==input$season)
  })
   

   output$avgs <- renderDataTable({
     
     nst_avg %>%
       select(season:home_win, TOI:PDO)%>%
       filter(Team==input$home_team)%>%
       filter(awayTeam==input$away_team)%>%
       filter(season==input$season)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

