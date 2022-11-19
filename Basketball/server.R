library(dplyr)
library(knitr)
library(readr)
library(ggplot2)
library(shiny)
library(tidyr)
library(plotly)
library(grid) 
library(jpeg) 
library(RCurl)


#setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))
nba <- read.csv('NBA_16_17.csv')
courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

#### FUNCTIONS ----

three_sizes = function(data, size1, size2, size3, border1, border2){
  # If  shot_count <= border1, then new_size = size1,
  # If border1 > shot_count <= piir2, then new_size = size2,
  # If shot_count > border2, then new_size = size3
  
  # sinu kood
  new <- data %>%
    mutate(new_size = ifelse(shot_count <= border1, 
                             size1, 
                             ifelse(shot_count <= border2,
                                    size2,
                                    ifelse(shot_count > border2, size3,0)
                             )
    )
    )
  return(new)
}

hexagon = function(x, y, area){
  r = sqrt(2*sqrt(3)/9*area)
  x_coord=x+c(0, sqrt(3)/2*r, sqrt(3)/2*r, 0,-sqrt(3)/2*r, -sqrt(3)/2*r) 
  y_coord=y+c(r, r/2, -r/2, -r, -r/2, r/2)
  
  # return(data.frame("x" = x_coord,
  #                   "y" = y_coord))
  
  return(list(x_coord, y_coord))
}

#### preworking data ----

nba_agr = nba %>%
  mutate(x = round(LOC_X, -1),
         y = round(LOC_Y,-1)) %>%
  group_by(PLAYER_NAME, TEAM_NAME, x, y) %>%
  summarise(shot_count =n(), 
            hit_rate = mean(SHOT_MADE_FLAG)*100) 


nba_agr <- three_sizes(nba_agr, 30,100,200, 1,5)



#joonise loomise funktsioon

plot_player = function(data, player){
  
  chosen_player = filter(data, PLAYER_NAME == player)
  
  
  
  if (nrow(chosen_player)>0){
    
    new_data <- chosen_player[rep(1:nrow(chosen_player), 1, each = 6),]
    new_data <- new_data %>% 
      mutate(X6=0, 
             Y6=0, 
             indx=0)
    
    for (i in 1:nrow(chosen_player)){
      hex <- hexagon(as.integer(chosen_player[i,'x']),
                     as.integer(chosen_player[i,'y']),
                     as.integer(chosen_player[i,'new_size']))
      
      
      new_data[(1 + (i-1)*6):(6 + (i-1)*6),'X6' ] <-hex[[1]]
      new_data[(1 + (i-1)*6):(6 + (i-1)*6),'Y6' ] <-hex[[2]]
      new_data[(1 + (i-1)*6):(6 + (i-1)*6),'indx' ] <- i
      # for (j in 1:6){
      #   new_data[(i-1)*6+j,'X6'] <- hex[j, "x"]
      #   new_data[(i-1)*6+j,'Y6'] <- hex[j, "y"]
      #   new_data[(i-1)*6+j,'indx'] <- i
      # }
    }
    
    plot <-ggplot(new_data, aes(x=X6, y=Y6)) + 
      annotation_custom(court, -250, 250, -50, 420) + 
      geom_polygon(aes(fill = hit_rate, group = indx))+
      coord_fixed(ratio=1) + ylim(-50,300) + scale_size_area() +
      scale_fill_gradient2(midpoint= 50,
                           low='#ABCDBC', 
                           mid='#EFF471', 
                           high='#D9060B',  
                           space = "rgb",
                           na.value = "grey50", 
                           guide = "colourbar")
  } else {
    plot <- ggplot(data, aes(x=NULL, y=NULL)) + 
      annotation_custom(court, -250, 250, -50, 420)
  }
  
  return(plot)
}

teams <- nba %>%
  group_by(TEAM_NAME)%>%
  summarise()%>%
  dplyr::pull(TEAM_NAME)


function(input, output, session) {
  
  observe({
    
    # Sinu kood
    ## Vali v?lja nende m?ngijate nimed, kes kuuluvad (v?i on kuulunud) 
    ## valitud meeskonda
    
    players_in_team = nba %>%
      filter(TEAM_NAME==input$team) %>%
      group_by(PLAYER_NAME)%>%
      summarise()%>%
      dplyr::pull(PLAYER_NAME)
    
    updateSelectizeInput(session, "player",
                         label = "Choose player",
                         choices = players_in_team,
                         selected = head(players_in_team, 1))
  })
  
  output$plot <- renderPlot({
    plot_player(nba_agr, input$player)
  })
  
}