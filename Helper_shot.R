# title: "Shot Charts"
# author: "Shixing Cao","Jackson Hubbard"
# drawing court credit to : "Ewen Gallic"  http://editerna.free.fr/wp/
# Preprocessing SportVU dataset reference to "Pre-processing for Shot Charts.ipynb"
# date: "07/19/2019"

library(dplyr)
library(grid)
library(ggplot2)
library(gridExtra)
library(gtable)


# Load Dateset
pbp <- read.csv("games_1415.csv", stringsAsFactors=F, na.strings=c("", " ", "NA"))
source("Master_Functions.R")

###############
## FUNCTIONS ##
###############

# http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
# Function to create circles
circle_fun <- function(center=c(0,0), diameter=1, npoints=500, start=0, end=2){
  tt <- seq(start*pi, end*pi, length.out=npoints)
  data.frame(
    x = center[1] + diameter / 2 * cos(tt),
    y = center[2] + diameter / 2 * sin(tt)
  )
}

# Gives y coordinates of the opposite side
rev_y <- function(y) 94-y

# Converts inches to feet
inches_to_feet <- function(x) x/12



# From x and y coordinates for a line (represented by a polygon here),
# a number of group and a short description
# creates a data.frame for this line
# in order to use it with ggplot2.
new_coords <- function(x, y, group, descri){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group <- group
  new_coords_df$side <- 1
  group <- group + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = x, y = rev_y(y))
  new_coords_df2$group <- group
  new_coords_df2$side <- 2
  group <<- group + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  
  return(new_coords_df)
}

new_coords_2 <- function(x, y, group_2, descri, color){
  new_coords_df <- data.frame(x = x, y = y)
  new_coords_df$group_2 <- group_2
  new_coords_df$side <- 1
  group_2 <- group_2 + 1
  
  # The same thing for the opposite side
  new_coords_df2 <- data.frame(x = 50-x, y = rev_y(y))
  new_coords_df2$group_2 <- group_2
  new_coords_df2$side <- 2
  group_2 <<- group_2 + 1
  
  # On reunit les donnees
  new_coords_df <- rbind(new_coords_df, new_coords_df2)
  new_coords_df$descri <- descri
  new_coords_df$Color <- color
  
  return(new_coords_df)
}



# Given the angle theta and the court data frame,
# rotates the coordinates of the court by an angle theta
rotate_court <- function(court, theta=pi/2){
  court_r <- court
  court_r$x <- court_r$x / 180 * pi
  court_r$y <- court_r$y / 180 * pi
  matrice_r <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), ncol = 2)
  coords_r <- apply(court_r[,c("x","y")], 1, function(x) x %*% matrice_r)
  court_r$x <- coords_r[1,] ; court_r$y <- coords_r[2,]
  court_r$x <- court_r$x * 180 / pi
  court_r$y <- court_r$y * 180 / pi
  return(court_r)
}



###############
## THE COURT ##
###############

# 3 pts circle (the width of line is 2 inches)
cercle_3pts.out <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(9))*2)
cercle_3pts.in <- circle_fun(center = c(25,inches_to_feet(63)), diameter = (20+inches_to_feet(7))*2)
# Basket circle
cercle_rim <- circle_fun(center = c(25,5+3/12), diameter = 1.5)
# Free throw circle
cercle_lf.out <- circle_fun(center = c(25,19), diameter = 6*2)
cercle_lf.in <- circle_fun(center = c(25,19), diameter = (6-2/12)*2)
# Center circle
cercle_mid.out <- circle_fun(center = c(25,47), diameter = 6*2)
cercle_mid.in <- circle_fun(center = c(25,47), diameter = (6-2/12)*2)


group <- 1 # We assign the first group, and it gets incremented with each use of new_coords()
court <- new_coords(c(0-1/6,0-1/6,53 + 1/6,53 + 1/6), c(0 - 1/6,0,0,0 - 1/6), group = group, descri = "End Line 1")
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,0,0), y = c(0,47-1/12,47-1/12,0), group = group, descri = "Side Line 1"))
court <- rbind(court, new_coords(x = c(50,50,50+1/6,50+1/6), y = c(0,47-1/12,47-1/12,0), group = group, descri = "Side Line 2"))
court <- rbind(court, new_coords(x = c(47,47,53,53), y = c(28,28+1/6,28+1/6,28), group = group, descri = "Mark Coach right"))
court <- rbind(court, new_coords(x = c(inches_to_feet(51),inches_to_feet(51),inches_to_feet(51)+1/6,inches_to_feet(51)+1/6), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts Bottom Left"))
court <- rbind(court, new_coords(x = c(50-inches_to_feet(51)-1/6,50-inches_to_feet(51)-1/6,50-inches_to_feet(51),50-inches_to_feet(51)), y = c(0,inches_to_feet(63),inches_to_feet(63),0), group = group, descri = "3pts Bottom Right"))
court <- rbind(court, new_coords(x = c(19,19,19+1/6,19+1/6), y = c(0,19,19,0), group = group, descri = "Lane Line left"))
court <- rbind(court, new_coords(x = c(31-1/6,31-1/6,31,31), y = c(0,19,19,0), group = group, descri = "Lane Line Right"))
court <- rbind(court, new_coords(x = c(19,19,31,31), y = c(19-1/6,19,19,19-1/6), group = group, descri = "Free Throw Line"))
court <- rbind(court, new_coords(x = c(22, 22, 28, 28), y = c(4-1/6,4,4,4-1/6), group = group, descri = "Board"))
court <- rbind(court, new_coords(x = c(cercle_3pts.out[1:250,"x"], rev(cercle_3pts.in[1:250,"x"])),
                                 y = c(cercle_3pts.out[1:250,"y"], rev(cercle_3pts.in[1:250,"y"])), group = group, descri = "3pts Circle"))
court <- rbind(court, new_coords(x = c(cercle_lf.out[1:250,"x"], rev(cercle_lf.in[1:250,"x"])),
                                 y = c(cercle_lf.out[1:250,"y"], rev(cercle_lf.in[1:250,"y"])), group = group, descri = "Free Throw Circle"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(7,8,8,7), group = group, descri = "Mark 1 Lane Line left"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "Mark 2 Lane Line Left"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(2),14+inches_to_feet(2)), group = group, descri = "Mark 3 Lane Line Left"))
court <- rbind(court, new_coords(x = c(19-0.5,19-0.5,19,19), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "Mark 4 Lane Line Left"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(7,8,8,7), group = group, descri = "Mark 1 Lane Line Right"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(11,11+inches_to_feet(2),11+inches_to_feet(2),11), group = group, descri = "Mark 2 Lane Line Right"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(14+inches_to_feet(2),14+inches_to_feet(4),14+inches_to_feet(4),14+inches_to_feet(2)), group = group, descri = "Mark 3 Lane Line Right"))
court <- rbind(court, new_coords(x = c(0-1/6,0-1/6,50+1/6,50+1/6), y = c(94/2-1/12,94/2, 94/2, 94/2-1/12), group = group, descri = "Mid Court Line"))
court <- rbind(court, new_coords(x = c(31,31,31+0.5,31+0.5), y = c(17+inches_to_feet(4),17+inches_to_feet(6),17+inches_to_feet(6),17+inches_to_feet(4)), group = group, descri = "Mark 4 Lane Line Right"))
court <- rbind(court, new_coords(x = c(cercle_mid.out[250:500,"x"], rev(cercle_mid.in[250:500,"x"])),
                                 y = c(cercle_mid.out[250:500,"y"], rev(cercle_mid.in[250:500,"y"])), group = group, descri = "Center Circle"))
court <- rbind(court, new_coords(x = cercle_rim[,"x"], y = cercle_rim[,"y"], group = group, descri = "Rim"))

#E5E2CA
group_2<- 1 # We assign the first group, and it gets incremented with each use of new_coords()
court_2 <- new_coords_2(x = c(0,0,25,25),
                        y = c(0,47,47,0), group = group_2, descri = "left court", color = "#FFFAE1")
court_2 <- rbind(court_2,new_coords_2(x = c(25,25,50,50),
                                      y = c(0,47,47,0), group = group_2, descri = "right court", color = "#E5E2CA"))
court_2 <- rbind(court_2, new_coords_2(x = c(inches_to_feet(51),inches_to_feet(51),cercle_3pts.out[250:126,"x"],25,inches_to_feet(51)),
                                 y = c(0,inches_to_feet(63),cercle_3pts.out[250:126,"y"],0,0), group = group_2, descri = "3pts left", color = "#FFCCAC"))
court_2 <- rbind(court_2,  new_coords_2(x = c(50-inches_to_feet(51),50-inches_to_feet(51),cercle_3pts.out[1:126,"x"],25,inches_to_feet(51)),
                                        y = c(0,inches_to_feet(63),cercle_3pts.out[1:126,"y"],0,0), group = group_2, descri = "3pts right", color = "#E99787"))
court_2 <- rbind(court_2,new_coords_2(c(19,19,31,31), c(0,19,19,0), group = group_2, descri = "Lane", color = "#CE5A57"))
# court_2 <- rbind(court_2, new_coords_2(x = c(cercle_lf.out[1:250,"x"], 19),
#                                  y = c(cercle_lf.out[1:250,"y"],19), group = group_2, descri = "Free Throw Circle",color = "lane"))



decimal_to_percentage <- function(decimal){
  decimal <- decimal*100
  decimal <- formatC(decimal, digits = 2, format = "f")
  decimal <- paste(decimal,"%", sep = " ", collapse = NULL)
  return(decimal)
}

report<-function(dataset){
  
  lane_count <- 0
  inside_right <- 0
  inside_left <- 0
  outside_left <- 0
  outside_right <- 0
  lane_count_made <- 0
  inside_right_made <- 0
  inside_left_made <- 0
  outside_left_made<- 0
  outside_right_made <- 0
  
  
  for (i in 1:nrow(dataset)) {
    x <- dataset$x[i]
    y <- dataset$y[i]
    event_descrip <- dataset$event.descrip[i] 
    
    if(y>=47){
      x <- 50-x
      y <- 94-y
    }
    
    
    
  # Lane Area
  if( ((x>=19) & (x<=31)) & (y<19))
  {
    lane_count <- lane_count+1
    if(event_descrip == 'made shot'){
      lane_count_made <-  lane_count_made+1
    }
  }
  # Inside 3 Point Line
  else if( (y>=inches_to_feet(63)) & (sqrt((x-25)^2+(y-inches_to_feet(63))^2) < 20.75) ) {
    if(x>=25){
      inside_right <- inside_right+1
      if(event_descrip == 'made shot'){
        inside_right_made <-  inside_right_made+1
      }
    }else{
      inside_left <- inside_left+1
      if(event_descrip == 'made shot'){
      inside_left_made <-  inside_left_made+1
    }
  }}
  
  # Inside 3 point line left (the straight part)
  else if((y>0)&(y<inches_to_feet(63)) & (x>inches_to_feet(51)) & (x<19)) {
    inside_left <- inside_left+1
    if(event_descrip == 'made shot'){
      inside_left_made <-  inside_left_made+1
    }
    }
  
  # Inside 3 point line right (the straight part)
  else if((y>0)&(y<inches_to_feet(63)) & (x<(50-inches_to_feet(51))) & (x>31)) {
  inside_right <- inside_right+1
  if(event_descrip == 'made shot'){
    inside_right_made <-  inside_right_made+1
  }
  }
  
  # Outside 3 point line 
  else if( (y<47) & (x<=25) & (sqrt((x-25)^2+(y-inches_to_feet(63))^2) >= 20.75)  ){
    outside_left <- outside_left+1
    if(event_descrip == 'made shot'){
      outside_left_made <-  outside_left_made+1
    }
    
    }
  else if( (y<47) & (x>25) & (sqrt((x-25)^2+(y-inches_to_feet(63))^2) >= 20.75)  ){
    outside_right <- outside_right+1
    if(event_descrip == 'made shot'){
      outside_right_made <-  outside_right_made+1
    }
  }
  

}
  
  lane_count_rate <- lane_count_made / lane_count 
  inside_right_rate <- inside_right_made / inside_right 
  inside_left_rate <- inside_left_made / inside_left 
  if(outside_left == 0 ){ outside_left_rate <- 0 }
  else{
  outside_left_rate <- outside_left_made / outside_left  }
  
  if(outside_right == 0 ){ outside_right_rate <- 0 }
  else{
  outside_right_rate <- outside_right_made / outside_right  
  }
   
return (list(
  decimal_to_percentage(lane_count_rate), 
  decimal_to_percentage(inside_left_rate), 
  decimal_to_percentage(inside_right_rate), 
  decimal_to_percentage(outside_left_rate), 
  decimal_to_percentage(outside_right_rate)))
  
}
  



visualize_shots<- function(date){
  # select one game from season dataframe
  #date = "1-11"
  one_game <- pbp[which(pbp$game_id == date), ]
  one_game$bc_team<- as.character(one_game$bc_team)
  one_game$p1_team_id<- as.character(one_game$p1_team_id)
  one_game$p10_team_id<- as.character(one_game$p10_team_id)

  home_team <- get_team(season_1415,date)[[1]]
  away_team <- get_team(season_1415,date)[[2]]
  home_color <- generate_colors(home_team)[[1]]
  away_color <- generate_colors(away_team)[[1]]

  # rename column names of dataframe
  colnames(one_game)[colnames(one_game)=="bc_x"] <- "x"
  colnames(one_game)[colnames(one_game)=="bc_y"] <- "y"
  
  
 


  ###########
  ## PLOTS ##
  ###########

  # add a Color column
  one_game <- one_game %>%
    mutate(Color = ifelse(bc_team == home_team, home_color,
                          ifelse(bc_team == away_team, away_color, "none")))
  # divide one datafrane into two: home team and away team
  home_shots <- one_game[ which(one_game$bc_team == home_team), ]
  zone_rate_home <- report(home_shots)
 
  # rotate shot location
  home_shots <- rotate_court(home_shots, theta = pi/2)
  
  away_shots <- one_game[ which(one_game$bc_team == away_team), ]
  zone_rate_away <- report(away_shots)
 
  # rotate shot location
  away_shots <- rotate_court(away_shots, theta = pi/2)
  
  danci <- "sanfanxian"
  
 
  
  # Whole court and shot loc with rotation and zone percentage
    shot_chart <- ggplot(width = 500, height = 350) +
    geom_polygon(data = rotate_court(court_2, theta = pi/2), aes(x = x, y = y, group = group_2, fill = Color))+
    scale_fill_identity()+
    geom_polygon(data = rotate_court(court, theta = pi/2), aes(x = x, y = y, group = group), col = "black") +
    geom_point(data = home_shots, aes(x = x, y = y, color = Color, shape = event.descrip),size=2.5) +
    geom_point(data = away_shots, aes(x = x, y = y, color = Color, shape = event.descrip),size=2.5) +
    scale_shape_manual("Shot Type",values = c(16, 1))+
    scale_color_identity("Team", labels = c(home_team,away_team) , breaks = c(home_color,away_color), guide = "legend")+
    annotate("text", x = -10, y = -10, label = zone_rate_home[[2]])+
    annotate("text", x = -10, y = -25, label = zone_rate_home[[1]])+
    annotate("text", x = -10, y = -40, label = zone_rate_home[[3]])+
    annotate("text", x = 37, y = -10, label = zone_rate_home[[4]])+
    annotate("text", x = 37, y = -40, label = zone_rate_home[[5]])+
    annotate("text", x = 104, y = -40, label = zone_rate_away[[2]])+
    annotate("text", x = 104, y = -25, label = zone_rate_away[[1]])+
    annotate("text", x = 104, y = -10, label = zone_rate_away[[3]])+
    annotate("text", x = 57, y = -40, label = zone_rate_away[[4]])+
    annotate("text", x = 57, y = -10, label = zone_rate_away[[5]])+
    coord_equal() +
    xlim(-20,115) +
    ylim(-54,1) +
    theme(line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = NA),
          plot.margin=unit(c(-4,0,-4,0),"cm"),
          legend.margin=margin(t = -0.5, unit='cm'),
          legend.position="bottom", legend.box = "horizontal"
          # legend.text=element_text(family = "Times New Roman",size=23),
          # legend.title=element_text(family = "Times New Roman",size=23) 
    )
    
    #ggsave(sprintf("%s.png",date))
    filename <- paste(date, ".jpg", sep = "")
    ggsave(filename= filename, plot= shot_chart)
    shot_chart
}





data_not_exist <- function(){  
   ggplot() +
    geom_text(aes( x = 50, y = 70,label="Relevant Data for Shot Chart is unavailable."), size = 5)+
    coord_equal() +
    xlim(0,100) +
    ylim(0,70) +
    theme(line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(), axis.title = element_blank(),
          panel.background = element_rect(fill = NA),
          legend.position="bottom", legend.box = "horizontal"
        
    )}




generate_all_shotcharts <- function (list_games) {
  setwd("~/Desktop/Data+/Basketball Analytics")
  for(i in list_games) {
    #i <- 5
    #season_df <- season_1516
    #season <- season_df[1,]$season
    date <- i
    
    visualize_shots(date)
    #urls <- get_urls(season_df, date)
    #pbp_url <- as.character(urls[[1]])
    #boxscore_url <- as.character(urls[[2]])
    #teamstats_url <- as.character(urls[[3]])
    
    #pbp <- scrape_pbp(url = pbp_url)
    #cleaned_pbp <- clean_pbp(pbp = pbp)
    #team_stats <- scrape_teamstats(url = teamstats_url)
    
    #both_all_stats <- scrape_and_clean_boxscore(url = boxscore_url)
    
    #nodes_and_edges <- clean_for_network(pbp = cleaned_pbp, list_both_all_stats = both_all_stats)
    #home_team <- nodes_and_edges[[5]]
    #away_team <- nodes_and_edges[[6]]
    
    #visualize_home_network(nodes_and_edges)
    #visualize_away_network(nodes_and_edges)
    #gameflow(clean_pbp, season_df, date)
    #make_table(team_stats_df = team_stats, home_team = home_team, away_team= away_team)
    
    #away_temp_net(nodes_and_edges = nodes_and_edges, season = season, date = date)
    #home_temp_net(nodes_and_edges = nodes_and_edges, season = season, date = date)
    #make_network <- make_network(nodes_and_edges = nodes_and_edges)
    
    
    
    
    
    
  }

  
}

# run this code once in order to generate images that we will put into app
# need to use images directly instead of using data because the data is private
# list of games that we have SportVU data on
list_games <- c("11-18", "11-30", "12-15", "12-29", "12-31", "1-3", "1-17", "1-19", "1-25", "2-4", "2-7", "2-18", "2-21", "2-28", "3-4", "3-13", "3-20", "3-22")
#generate_all_shotcharts(list_games)
