#### SETUP ----

library(grid) 
library(jpeg) 
library(RCurl)
library(ggplot2)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

#### Functions ----

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
  
  return(data.frame("x" = x_coord,
                    "y" = y_coord))
}

#### Reading  and modifing data ----

nba <- read.csv('NBA_16_17.csv')

nba_agr = nba %>%
  mutate(x=round(LOC_X, -1),
         y = round(LOC_Y,-1)) %>%
  group_by(PLAYER_NAME, TEAM_NAME, x, y) %>%
  summarise(shot_count=n(), hit_rate= mean(SHOT_MADE_FLAG)*100) %>%
  ungroup()

courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                    width=unit(1,"npc"), height=unit(1,"npc"))

chosen_player <-filter(nba_agr, PLAYER_NAME == 'Kristaps Porzingis')

#### Creating plot set by step ----

ggplot(chosen_player, aes(x=x, y=y, color=hit_rate,
                          size=shot_count)) + 
  annotation_custom(court, -250, 250, -50, 420) + geom_point()+
  coord_fixed(ratio=1) + ylim(-50,300) + scale_size_area()



nba_agr <- three_sizes(nba_agr, 1,9,35, 1,5)


chosen_player = filter(nba_agr, PLAYER_NAME == 'Kristaps Porzingis')

ggplot(chosen_player, aes(x=x, y=y, color=hit_rate,
                                      size=new_size)) + 
  annotation_custom(court, -250, 250, -50, 420) + 
  geom_point()+
  coord_fixed(ratio=1) + 
  ylim(-50,300) + 
  scale_size_area()

ggplot(chosen_player, aes(x=x, y=y, color=hit_rate,
                          size=new_size)) + 
  annotation_custom(court, -250, 250, -50, 420) + geom_point()+
  coord_fixed(ratio=1) + 
  ylim(-50,300) + 
  scale_size_area() +
  scale_color_gradient2(midpoint= mean(chosen_player$hit_rate),
                        low='#ABCDBC', 
                        mid='#EFF471', 
                        high='#D9060B',  
                        space = "rgb",
                        na.value = "grey50", 
                        guide = "colourbar")




nba_agr <- three_sizes(nba_agr, 30,100,200, 1,5)
chosen_player <- filter(nba_agr, PLAYER_NAME == "DeAndre' Bembry")

new_data <- chosen_player[rep(1:nrow(chosen_player),1,each=6),]
new_data <- new_data %>% 
  mutate(X6=0, 
         Y6=0, 
         indx=0)

for (i in 1:nrow(chosen_player)){
  hex <- hexagon(as.integer(chosen_player[i,'x']),
                     as.integer(chosen_player[i,'y']),
                     as.integer(chosen_player[i,'new_size']))
  
  for (j in 1:6){
    new_data[(i-1)*6+j,'X6'] <- hex[j, "x"]
    new_data[(i-1)*6+j,'Y6'] <- hex[j, "y"]
    new_data[(i-1)*6+j,'indx'] <- i
  }
}

ggplot(new_data, aes(x=X6, y=Y6)) + 
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
