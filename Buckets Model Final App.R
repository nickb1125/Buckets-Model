
## add buckets betting picks

### add percentile plot for team stats
### Add ast vs pts plot for best team players

library(plotly)
library(DT)
library(data.table)
library(scales)
library(shiny)
library(dplyr)
library(kableExtra)
library(viridis)
library(janitor)
library(formattable)
library(tidyr)
library(stringr)

nba_teams <- (read.csv('/Users/nickbachelder/Buckets Model/nba_team') %>% dplyr::select(-X))$abbreviation
upcoming_games_pred <- read.csv('/Users/nickbachelder/Buckets Model/upcoming_games_pred') %>% dplyr::select(-X)
colnames(upcoming_games_pred) <- c('Home Team', 'Away Team', 'Game Date', 'Predicted', 'Probability')
final_reg_season_model <- readRDS("/Users/nickbachelder/Buckets Model/final_reg_season_model.rds")
curr_inj <- read.csv('/Users/nickbachelder/Buckets Model/curr_inj') %>% dplyr::select(-X)
elo_track <- read.csv('/Users/nickbachelder/Buckets Model/ELO_track') %>% dplyr::select(-X)
starters_predicted <- read.csv('/Users/nickbachelder/Buckets Model/starters_predicted') %>% dplyr::select(-X)
buckets_playoff_records <- read.csv('/Users/nickbachelder/Buckets Model/buckets_playoff_records')%>% dplyr::select(-X)
buckets_reg_records <- read.csv('/Users/nickbachelder/Buckets Model/buckets_reg_records')%>% dplyr::select(-X)



predict_df_10 <- read.csv('/Users/nickbachelder/Buckets Model/predictdf_10') %>% dplyr::select(-X)
data_for_means = data.frame(predict_df_10)

means <- colMeans(data_for_means)
sds <- apply(data_for_means,2,sd)


matchups <- read.csv('/Users/nickbachelder/Buckets Model/matchups') %>% dplyr::select(-X)
labels <- matchups$Matchup
matchups <- matchups[names(predict_df_10)] %>% mutate(ROAD_WIN_P_A = as.numeric(0))
predict_matchup_scaled <-sweep(matchups, 2, means, "-")
predict_matchup_scaled <-sweep(predict_matchup_scaled, 2, sds, "/")
predict_matchup_scaled$Matchup <- labels

ui <- navbarPage("Buckets Model",
                 tabPanel("Buckets Predictions",
  
  fluidRow(
    
    column(12,
           wellPanel(
             DTOutput("upcoming_games_pred")
           )
           
    )
  )),
    
    tabPanel("Test Games",
    
    fluidRow(
    
    column(6,
           wellPanel(
             selectInput("home_selection", "Select Home Team", choices = nba_teams)
           )
    ),
    
    column(6,
           wellPanel(
             selectInput("away_selection", "Select Away Team", choices = nba_teams)
           )
    ),
    
    column(4,
           wellPanel(
             textOutput("inj_home")
           )
    ),
    
    column(4,
           wellPanel(
             tags$head(tags$style("#winner {color: black;
                                 font-size: 50px;
                                 font-family: Times New Roman;
                                 font-style: bold;
                                 }"
             )),
             tags$head(tags$style("#prob_home_win {color: black;
                                 font-size: 25px;
                                 font-family: Times New Roman;
                                 }"
             )),
             div(style="text-align:center",
                 textOutput("winner"),
                 textOutput("prob_home_win")
             )
    )
    ),
    
    column(4, 
           wellPanel(
             textOutput("inj_away")
           )
    )),
  fluidRow(
    
    column(12,
           wellPanel(
             plotOutput("elo_vs"),
             textOutput('team_plot_title'),
             formattableOutput("team_plot")
           )
    )
  ),
  
  fluidPage(
    
    column(12,  align="center",
             plotlyOutput("starter_plot")
    )
    
  )
  ),
  tabPanel('Accuracy History',
           fluidRow(
             column(6, 
                    wellPanel(
                      textOutput('buckets_r_records_t'),
                      formattableOutput("buckets_r_records")
                    )
             ),
             column(6, 
                    wellPanel(
                      textOutput('buckets_p_records_t'),
                      formattableOutput("buckets_p_records")
                    )
             )
           )
           )

)


server <- function(input, output) {
  
  output$prob_home_win <- reactive({
    
    chosen <- predict_matchup_scaled %>% filter(Matchup == paste(input$away_selection, '@', input$home_selection, sep = ''))
    prob <- as.numeric(predict(final_reg_season_model, newdata = chosen, type = 'prob')$Yes)
    ifelse(is.na(prob), NA, paste('Confidence:', round(ifelse(prob > 0.5, prob, 1 - prob), 2)))
    
    
  })
  
  output$winner <- reactive({
    
    chosen <- predict_matchup_scaled %>% filter(Matchup == paste(input$away_selection, '@', input$home_selection, sep = ''))
    prob <- as.numeric(predict(final_reg_season_model, newdata = chosen, type = 'prob')$Yes)
    ifelse(prob > 0.5, input$home_selection, input$away_selection)
    
    
  })
  
  output$inj_home <- reactive({
    
    team_inj_h <- curr_inj %>% filter(TEAM == input$home_selection) %>% dplyr::select(PLAYER)
    team_inj_h <- as.vector(team_inj_h[,1])
    paste('Key Injuries:', paste(team_inj_h, collapse = ', '))
    
    
  })
  
  output$inj_away <- reactive({
    
    team_inj_a <- curr_inj %>% filter(TEAM == as.character(input$away_selection)) %>% dplyr::select(PLAYER)
    team_inj_a <- as.vector(team_inj_a[,1])
    paste('Key Injuries:', paste(team_inj_a, collapse = ', '))
    
    
    
  })
  
  elo_vs_dat <- reactive({
    elo_track %>% filter(Team %in% c(input$home_selection, input$away_selection)) %>% filter(Season %in% c(2017, 2018, 2019, 2020, 2021)) %>% mutate(Date = as.Date(Date))
  })
  
  output$elo_vs <- renderPlot({
    
    ggplot(data=elo_vs_dat(), aes(x=Date, y=ELO, group=Team, color = Team)) +
      geom_line(size = 1) +
      scale_x_date(labels = date_format("%Y")) +
      ggtitle('Strength of Matchup Teams Over Time')
    
  })
  
  starter_dat <- reactive({
    splot <- starters_predicted %>% filter(TEAM_ABB %in% c(input$home_selection, input$away_selection) )
  })
  
  output$starter_plot <- renderPlotly({
    
    plot_ly(starter_dat(), x = ~REB, y = ~AST, z = ~PTS,color = ~TEAM_ABB, text = ~PLAYER_NAME) %>% add_markers() %>% layout(autosize = F, height = 800, width = 800) %>%
      add_text(textposition = "top right")
    
  })
  
  output$team_plot <- renderFormattable({
    
    team_data <- data.frame(t(predict_matchup_scaled %>% dplyr::select(PTS_H, PTS_A, AST_H, AST_A, REB_H, REB_A, FG_P_H, FG_P_A, FG_P3_H, FG_P3_A, FT_P_H, FT_P_A, AVG_PTS_ALLOWED_A, AVG_PTS_ALLOWED_H, TEAM_PLAYOFF_GAMES_P5_H, TEAM_PLAYOFF_GAMES_P5_A,  Matchup) %>%
                    filter(Matchup == paste(input$away_selection, '@', input$home_selection, sep = '')) %>% 
                    dplyr::select(-Matchup) %>% 
                    rename(FG3_P_H = FG_P3_H, FG3_P_A = FG_P3_A) %>% 
                    pivot_longer(cols = everything()) %>%  
                    mutate(group = sub("\\_.*", "", name)) %>% 
                    mutate(value = pnorm(value)*100) %>%
                    mutate(team = paste(ifelse(str_sub(name, -1) == 'H', input$home_selection, input$away_selection))) %>% select(-name) %>%
                    pivot_wider(id_cols = team, names_from = group)  %>%
                    mutate(AVG = 100 - AVG)
                  %>% rename(
                    'FG Percent' = FG,
                    'FT Percent' = FT,
                    '3pt FG Percent' = FT,
                    'PTS Allowed' = AVG,
                    'Player Experience' = TEAM
                  )
    )) %>%
      row_to_names(row_number = 1) %>% mutate_if(is.numeric, round, digits=2)
    
    formattable(team_data, lapply(1:nrow(team_data), function(row) {
      area(row, col = c(1,2)) ~ color_bar("lightblue", function(x){x/100} ) } ) )
    
  })
  
  output$upcoming_games_pred <- renderDT(datatable(upcoming_games_pred, class = 'cell-border stripe'))
  
  output$team_plot_title <- renderText('Team Statistic Percentiles Over Last 10 Games')
  
  output$buckets_r_records <- renderFormattable({

    formattable(buckets_reg_records %>% mutate_if(is.numeric, function(x){round(100*x,2)}), lapply(1:nrow(buckets_reg_records), function(row) {
      area(row, col = c(1,2)) ~ color_bar("lightblue", function(x){x/100} ) } ) )
    
  })
  
  output$buckets_r_records <- renderFormattable({
    
    formattable(buckets_reg_records %>% mutate(Year = as.character(Year)) %>% mutate_if(is.numeric, function(x){round(100*x,2)}))
    
  })
  
  output$buckets_p_records <- renderFormattable({
    
    formattable(buckets_playoff_records %>% mutate(Year = as.character(Year)) %>% mutate_if(is.numeric, function(x){round(100*x,2)}))
    
  })
  
  output$buckets_r_records_t <- renderText('Buckets Model Accuracies Regular Seasons')
  output$buckets_p_records_t <- renderText('Buckets Model Accuracies Playoffs')
  

}


shinyApp(ui = ui, server = server)