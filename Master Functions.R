

library(magrittr)
library(stringr)
library(naniar)
library(RCurl)
library(jsonlite)
library(dplyr)
library(plotly)
library(rvest)
library(igraph)
library(RColorBrewer)
library(ggplot2)
library(scales)

library(tidyverse)
library(ggraph)
library(tidygraph)
library(visNetwork)

# temporal networks
# library(sna)
# library(tsna)
# library(ndtv)
# library(mapmate)

library(DT)


# BASE REFERENECE DATA FRAME
# grabs files created by Base dateframe.Rmd stored in folder
# contains date, opponet, score, and url's needed to scrape pbp and boxscore

#setwd("~/Desktop/Data+/Basketball Analytics")
season_1415 <- read.csv("season_1415.csv")
season_1516 <- read.csv("season_1516.csv")
season_1617 <- read.csv("season_1617.csv")
season_1718 <- read.csv("season_1718.csv")
season_1819 <- read.csv("season_1819.csv")





scrape_pbp <- function(url) {
  url <- as.character(url)
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  
  time_stamp_html <- html_nodes(webpage,'.time-stamp')
  time_stamp <- html_text(time_stamp_html)
  
  game_details_html <- html_nodes(webpage,'#gamepackage-qtrs-wrap .game-details')
  game_details <- html_text(game_details_html) %>% as.character()
  
  score_html <- html_nodes(webpage,'.combined-score')
  score <- html_text(score_html)
  
  ImgNode <- html_nodes(webpage, css = "#gamepackage-qtrs-wrap .team-logo")
  link <- html_attr(ImgNode, "src")
  
  away_name_html <- html_nodes(webpage,'.away .long-name')
  away_name <- html_text(away_name_html)
  
  away_logo_node <- html_nodes(webpage,'.away .team-logo')
  away_logo_link <- html_attr(away_logo_node, "src")
  
  home_name_html <- html_nodes(webpage,'.home .long-name')
  home_name <- html_text(home_name_html)
  
  home_logo_node <- html_nodes(webpage,'.home .team-logo')
  home_logo_link <- html_attr(home_logo_node, "src")
  
  
  # make data frame- url = link of image for each event
  pbp <-data.frame(time_stamp = time_stamp, game_details = as.character(game_details), score = score, url = link, home_team = home_name, away_team = away_name, stringsAsFactors = FALSE)
  
  # compare url to home/away team logo url's
  pbp$team <- case_when(pbp$url == home_logo_link ~ home_name,
                        pbp$url == away_logo_link ~ away_name)  
  # get rid of time outs
  pbp <- subset(pbp, ! grepl("Timeout", pbp$game_details))   
  
  pbp$home_score <- as.numeric(sapply(strsplit(pbp$score, "-"), '[', 2))
  pbp$away_score <- as.numeric(sapply(strsplit(pbp$score, "-"), '[', 1))
  
  
  
  # Add an index column
  
  pbp$Id <- seq.int(nrow(pbp))
  # split time, convert it to minutes in numeric type
  
  time_minutes = list()
  second_half_start <- 10000
  first_ot_start <- 10000
  second_ot_start <- 10000
  third_ot_start <- 10000
  fourth_ot_start <- 10000
  last_row_minute <-  minute <- as.numeric(sapply(strsplit(pbp$time_stamp[1],":"),`[`, 1))
  period <- 1
  for (i in 1:nrow(pbp)) {
    time = pbp$time_stamp[i]
    minute <- as.numeric(sapply(strsplit(time,":"),`[`, 1))
    second <- as.numeric(sapply(strsplit(time,":"),`[`, 2))
    time_absolute <- 20-(minute+second/60)
    
    if((minute-last_row_minute)>2){
      period <- period + 1
      if(period==2){second_half_start <- pbp$Id[i]}
      else if(period==3){first_ot_start <- pbp$Id[i]}
      else if(period==4){second_ot_start <- pbp$Id[i]}
      else if(period==5){third_ot_start <- pbp$Id[i]}
      else{fourth_ot_start <- pbp$Id[i]}
    }
    
    last_row_minute <- minute
    
    if (i >= second_half_start){
      time_absolute<-time_absolute + 20
      if(i >= first_ot_start){
        time_absolute <- (5-(minute+second/60)) + 40
        
        
        if(i>second_ot_start){
          time_absolute <- (5-(minute+second/60)) + 45
          
          
          if(i>third_ot_start){
            time_absolute <- (5-(minute+second/60)) + 50
            
            
            if(i>fourth_ot_start){
              time_absolute <- (5-(minute+second/60)) + 55
              
            }
          }
        }
      }
    }
    time_minutes <- append(time_minutes, time_absolute)
  }
  
  
  pbp$time_min <- time_minutes
  
  return(pbp)
}









clean_pbp <- function(pbp) {
  # takes obs w/ description including "make" and gets scorer's name
  #pbp$scorer <-  ifelse(grepl("made", pbp$game_details),
  #  strsplit(pbp$game_details, "made", fixed = TRUE) %>% 
  #             sapply(extract, 1), NA)
  pbp$scorer <-  ifelse(grepl("made", pbp$game_details),
                        vapply(strsplit(pbp$game_details,"made"), `[`, 1, FUN.VALUE=character(1)), NA)
  
  #pbp$scorer <- pbp$scorer %>% stringr::word(1, 2, sep = fixed(" "))
  
  # fix scorer to only include events with basket
  # na_events <- c("Jump Ball", "Foul on", "Official TV", "End of", "rebound", "missed", "Steal", "Block", "turnover")
  #pbp <- pbp %>% replace_with_na_all(condition = ~.x %in% na_events)
  
  
  
  
  #pbp$assist <- strsplit(pbp$game_details, ". ", fixed = TRUE) %>% 
  # sapply(extract, 2) %>% 
  #  strsplit("by ", fixed = TRUE) %>% sapply(extract, 2) %>%stringr::str_remove("\\.$")
  
  pbp$assist <-  ifelse(grepl("Assisted by ", pbp$game_details),
                        vapply(strsplit(pbp$game_details,"Assisted by "), `[`, 2, FUN.VALUE=character(1)), NA) %>% stringr::str_remove("\\.$")
  
  pbp$points <- case_when(grepl("Free Throw", pbp$game_details) & !is.na(pbp$scorer) ~ 1,
                          grepl("Three Point", pbp$game_details) & !is.na(pbp$scorer) ~ 3,
                          grepl("made Jumper", pbp$game_details) & !is.na(pbp$scorer) ~ 2,
                          grepl("made Layup", pbp$game_details) & !is.na(pbp$scorer) ~ 2,
                          grepl("made Dunk", pbp$game_details) & !is.na(pbp$scorer) ~ 2,
                          grepl("made Two", pbp$game_details) & !is.na(pbp$scorer) ~ 2)
  
  pbp$points[is.na(pbp$points)] <- 0
  
  pbp <- pbp %>% 
    mutate(scorer = str_replace(scorer, "Jr.", "")) 
  pbp <- pbp %>% 
    mutate(scorer = str_replace(scorer, "Jr", "")) 
  pbp <- pbp %>% 
    mutate(scorer = str_replace(scorer, "III", ""))
  pbp <- pbp %>% 
    mutate(scorer = str_replace(scorer, "II", "")) 
  pbp <- pbp %>% 
    mutate(scorer = str_replace(scorer, "IV", "")) 
  
  pbp <- pbp %>% 
    mutate(assist = str_replace(assist, "Jr.", ""))
  pbp <- pbp %>% 
    mutate(assist = str_replace(assist, "Jr", ""))
  pbp <- pbp %>% 
    mutate(assist = str_replace(assist, "III", ""))
  pbp <- pbp %>% 
    mutate(assist = str_replace(assist, "II", ""))
  pbp <- pbp %>% 
    mutate(assist = str_replace(assist, "IV", ""))
  
  # if name is V.J. we need to make it V. in order to be consistent
  pbp <- pbp %>% 
    mutate(scorer = if_else(substr(scorer, 4,4) == ".", paste(substr(scorer, 1,1), substr(scorer, 5, nchar(scorer)), sep = ""), as.character(scorer)))
  
  pbp <- pbp %>% 
    mutate(assist = if_else(substr(assist, 4,4) == ".", paste(substr(assist, 1,1), substr(assist, 5, nchar(assist)), sep = ""), as.character(assist)))
  
  pbp <- data.frame(lapply(pbp, trimws), stringsAsFactors = FALSE)
  
  
  
  return(pbp)
}






scrape_and_clean_boxscore <- function (url) {
  url <- as.character(url)
  
  webpage <- read_html(url)
  
  # read the website to get player names
  away_names <- html_nodes(webpage,'.gamepackage-away-wrap .abbr , .gamepackage-away-wrap .team-name') %>% html_text()
  home_names <- html_nodes(webpage,'.gamepackage-home-wrap .abbr , .gamepackage-home-wrap .team-name') %>% html_text()
  
  both_teams_names <- c(away_names, home_names)
  num_rows_boxscore <- as.numeric(length(both_teams_names))
  num_players <- num_rows_boxscore - 2
  
  
  
  # read website to get player points
  # points will be listed away and then home
  both_players_points <- html_nodes(webpage,'td.pts') %>% html_text() 
  both_players_points <- as.numeric(both_players_points) %>% na.omit()
  # remove the two highest point values as these are team overall points
  #both_players_points <- both_players_points[both_players_points != max(both_players_points)]
  #both_players_points <- both_players_points[both_players_points != max(both_players_points)]
  
  
  
  # read website to get player assists 
  # assists will be listed away and then home
  both_players_assists <- html_nodes(webpage,'td.ast') %>% html_text()
  both_players_assists <- as.numeric(both_players_assists) %>% na.omit()
  # remove the two highest point values as these are team overall points
  #both_players_assists <- both_players_assists[both_players_assists != max(both_players_assists)]
  #both_players_assists <- both_players_assists[both_players_assists != max(both_players_assists)]
  
  
  # read website to determine home and away team
  team_away <- html_nodes(webpage,'.away .long-name') %>% html_text()
  team_home <- html_nodes(webpage,'.home .long-name') %>% html_text()
  
  
  
  
  
  index_away <- as.numeric(which(grepl(team_away, both_teams_names)))
  index_home <- as.numeric(which(grepl(team_home, both_teams_names)))
  
  
  home_player_names <- both_teams_names[(index_home +1):num_rows_boxscore] 
  home_player_names <- na.omit(home_player_names)
  
  away_player_names <- both_teams_names[2 : (index_home -1)] 
  away_player_names <- na.omit(away_player_names)
  
  
  
  home_player_points <- both_players_points[(index_home):(num_rows_boxscore-1)]
  home_player_points <- na.omit(home_player_points)
  
  away_player_points <- both_players_points[1:(index_home-2)] 
  away_player_points <- na.omit(away_player_points)
  
  
  home_player_assists <- both_players_assists[index_home:(num_rows_boxscore-1)]
  home_player_assists <- na.omit(home_player_assists)
  
  away_player_assists <- both_players_assists[1:(index_home-2)] 
  away_player_assists <- na.omit(away_player_assists)
  
  
  
  
  
  home_all_stats <- data.frame(name = home_player_names, points = home_player_points, assists = home_player_assists)
  
  away_all_stats <- data.frame(name = away_player_names, points = away_player_points, assists = away_player_assists)
  
  
  # ***********************************************************************
  # bosxcore includes Jr. but pbp doesnt- make consistent by dropping jr.??
  # ALSO NEED TO ACCOUNT FOR II AND III- SEE MSU GAME 2015 SEMIS
  
  home_all_stats <- home_all_stats %>% 
    mutate(name = str_replace(name, "Jr.", ""))
  home_all_stats <- home_all_stats %>% 
    mutate(name = str_replace(name, "Jr", ""))
  home_all_stats <- home_all_stats %>% 
    mutate(name = str_replace(name, "III", "")) 
  home_all_stats <- home_all_stats %>% 
    mutate(name = str_replace(name, "II", ""))
  home_all_stats <- home_all_stats %>% 
    mutate(name = str_replace(name, "IV", ""))
  
  away_all_stats <- away_all_stats %>% 
    mutate(name = str_replace(name, "Jr.", ""))
  away_all_stats <- away_all_stats %>% 
    mutate(name = str_replace(name, "Jr", "")) 
  away_all_stats <- away_all_stats %>% 
    mutate(name = str_replace(name, "III", "")) 
  away_all_stats <- away_all_stats %>% 
    mutate(name = str_replace(name, "II", ""))
  away_all_stats <- away_all_stats %>% 
    mutate(name = str_replace(name, "IV", ""))
  
  
  
  
  
  
  # if name is V.J. we need to make it V. in order to be consistent
  away_all_stats <- away_all_stats %>% 
    mutate(name = if_else(substr(name, 4,4) == ".", paste(substr(name, 1,2), substr(name, 5, nchar(name)), sep = ""), as.character(name)))
  
  home_all_stats <- home_all_stats %>% 
    mutate(name = if_else(substr(name, 4,4) == ".", paste(substr(name, 1,2), substr(name, 5, nchar(name)), sep = ""), as.character(name)))
  
  
  
  home_all_stats <- data.frame(lapply(home_all_stats, trimws), stringsAsFactors = FALSE)
  away_all_stats <- data.frame(lapply(away_all_stats, trimws), stringsAsFactors = FALSE)
  
  list_both_all_stats <- list(home_all_stats, away_all_stats, team_home, team_away)
  
  
  
  
  
  
  
  
  
  
  return(list_both_all_stats)     
}






scrape_teamstats <- function(url) {
  url <- as.character(url)
  #Reading the HTML code from the website
  webpage <- read_html(url)
  
  
  cat_names_html <- html_nodes(webpage,'#teamstats-wrap td:nth-child(1)')
  cat_names <- html_text(cat_names_html)
  cat_names <- str_replace_all(cat_names, "[\t\n]" , "")
  
  away_all_stats_html <- html_nodes(webpage,'#teamstats-wrap td:nth-child(2)')
  away_all_stats <- html_text(away_all_stats_html)
  
  home_all_stats_html <- html_nodes(webpage,'#teamstats-wrap td~ td+ td')
  home_all_stats <- html_text(home_all_stats_html)
  
  
  away_name_html <- html_nodes(webpage,'.away .long-name')
  away_name <- html_text(away_name_html)
  
  home_name_html <- html_nodes(webpage,'.home .long-name')
  home_name <- html_text(home_name_html)
  
  
  
  
  # make data frame- url = link of image for each event
  team_stats <-data.frame(away_stats = away_all_stats, categories <- cat_names, home_stats = home_all_stats, stringsAsFactors = FALSE)
  
  colnames(team_stats) <- c("away_stats", "category", "home_stats")
  
  return(team_stats)
}








clean_for_network <- function (pbp, list_both_all_stats) {
  
  home_all_stats <- list_both_all_stats[[1]]
  away_all_stats <- list_both_all_stats[[2]]
  team_home <- list_both_all_stats[[3]]
  team_away <- list_both_all_stats[[4]]
  
  
  # get rid of players with no points and no assists b/c dont want them in network
  # keep player if he has 1+ assist or 1+ point
  cut_home_all_stats <- home_all_stats
  cut_home_all_stats$scored <- ifelse(cut_home_all_stats$points >0, TRUE, FALSE)
  cut_home_all_stats$assisted <- ifelse(cut_home_all_stats$assists >0, TRUE, FALSE)
  cut_home_all_stats$remove <- (case_when(cut_home_all_stats$scored >0 ~ FALSE,
                                          cut_home_all_stats$assisted >0 ~ FALSE))
  cut_home_all_stats <- cut_home_all_stats[!is.na(cut_home_all_stats$remove),]
  cut_away_all_stats <- away_all_stats
  cut_away_all_stats$scored <- ifelse(cut_away_all_stats$points >0, TRUE, FALSE)
  cut_away_all_stats$assisted <- ifelse(cut_away_all_stats$assists >0, TRUE, FALSE)
  cut_away_all_stats$remove <- (case_when(cut_away_all_stats$scored >0 ~ FALSE,
                                          cut_away_all_stats$assisted >0 ~ FALSE))
  cut_away_all_stats <- cut_away_all_stats[!is.na(cut_away_all_stats$remove),]
  
  plays_with_FG <- pbp[which(pbp$points == 2 | pbp$points == 3 | pbp$points == 1), ]
  
  
  plays_with_FG <- data.frame(lapply(plays_with_FG, trimws), stringsAsFactors = FALSE)
  
  
  # clean up variables of pbp
  
  # this deals with players who have double intial names such as V.J.
  plays_with_FG$scorer_last <- sapply(strsplit(as.character(plays_with_FG$scorer),' '), "[", 3)
  plays_with_FG$scorer_last <- case_when(is.na(plays_with_FG$scorer_last) ~ sapply(strsplit(as.character(plays_with_FG$scorer),' '), "[", 2), 
                                         ! is.na(plays_with_FG$scorer_last) ~ sapply(strsplit(as.character(plays_with_FG$scorer),' '), "[", 3))
  
  plays_with_FG$assist_last <- sapply(strsplit(as.character(plays_with_FG$assist),' '), "[", 3)
  plays_with_FG$assist_last <- case_when(is.na(plays_with_FG$assist_last) ~ sapply(strsplit(as.character(plays_with_FG$assist),' '), "[", 2), 
                                         ! is.na(plays_with_FG$assist_last) ~ sapply(strsplit(as.character(plays_with_FG$assist),' '), "[", 3))
  
  
  #plays_with_FG$scorer_last <- sapply(strsplit(as.character(plays_with_FG$scorer),' '), "[", 2)
  plays_with_FG$scorer_first <- sapply(strsplit(as.character(plays_with_FG$scorer),' '), "[", 1) 
  plays_with_FG$scorer_first <- paste(substr(plays_with_FG$scorer_first, 1, 1), ".", sep = "")
  plays_with_FG$scorer_f.last <- paste(plays_with_FG$scorer_first, plays_with_FG$scorer_last, sep = " ")
  
  
  #plays_with_FG$assist_last <- sapply(strsplit(as.character(plays_with_FG$assist),' '), "[", 2)
  plays_with_FG$assist_first <- sapply(strsplit(as.character(plays_with_FG$assist),' '), "[", 1) 
  plays_with_FG$assist_first <- paste(substr(plays_with_FG$assist_first, 1, 1), ".", sep = "")
  plays_with_FG$assist_f.last <- ifelse(! is.na(plays_with_FG$assist_last), paste(plays_with_FG$assist_first, plays_with_FG$assist_last, sep = " "), NA)
  
  new_vars <- c("time_min", "team", "scorer_f.last", "assist_f.last", "points")
  points_assists <- plays_with_FG[new_vars]
  
  
  home_points_assists <- points_assists[which(points_assists$team == team_home),]
  away_points_assists <- points_assists[which(points_assists$team == team_away),]
  
  
  
  
  # now get rid of na's = plays with no assists
  home_assists_df <- home_points_assists %>% na.omit(assist_f.last)
  away_assists_df <- away_points_assists %>% na.omit(assist_f.last)
  
  # format df to be time, assist, scorer
  home_assists_df <- home_assists_df[, c("assist_f.last", "scorer_f.last", "time_min")]
  away_assists_df <- away_assists_df[, c("assist_f.last", "scorer_f.last", "time_min")]
  
  # manipulate assists data frame to only have unique assist --> shooter paths and track count
  library(plyr)
  home_assists_notime <- home_assists_df
  home_assists_notime$count = 1
  home_assists_notime <- ddply(home_assists_notime,.(assist_f.last,scorer_f.last),nrow)
  colnames(home_assists_notime)[3] <- "count"
  
  away_assists_notime <- away_assists_df
  away_assists_notime$count = 1
  away_assists_notime <- ddply(away_assists_notime,.(assist_f.last,scorer_f.last),nrow)
  colnames(away_assists_notime)[3] <- "count"
  
  
  
  # if we want only last name
  #duke_all_stats$player <- sapply(strsplit(as.character(duke_all_stats$name),'. '), "[", 2)
  # other_all_stats$player <- sapply(strsplit(as.character(other_all_stats$name),'. '), "[", 2)
  
  # reorder by column name
  cut_home_all_stats <- cut_home_all_stats[c("name", "points", "assists")]
  cut_home_points <- cut_home_all_stats[c("name", "points")]
  cut_away_all_stats <- cut_away_all_stats[c("name", "points", "assists")]
  cut_away_points <- cut_away_all_stats[c("name", "points")]
  
  #print(duke_all_stats)
  
  nodes_and_edges <- list(cut_home_points, home_assists_notime, cut_away_points, away_assists_notime, team_home, team_away, cut_home_all_stats, cut_away_all_stats, away_points_assists, home_points_assists)
  
  return(nodes_and_edges)
}






# overwrited implementation of autocurve.edges
# found online to try to force edges to not overlap https://stackoverflow.com/questions/16875547/using-igraph-how-to-force-curvature-when-arrows-point-in-opposite-directions
# this fixes issue of autocurve.edges also curving edges for unique observations.

autocurve.edges2 <-function (graph, start = 0.5)
{
  cm <- count.multiple(graph)
  mut <-is.mutual(graph)  #are connections mutual?
  el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
              collapse = ":")
  ord <- order(el)
  res <- numeric(length(ord))
  p <- 1
  while (p <= length(res)) {
    m <- cm[ord[p]]
    mut.obs <-mut[ord[p]] #are the connections mutual for this point?
    idx <- p:(p + m - 1)
    if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
      r <- 0
    }
    else {
      r <- seq(-start, start, length = m)
    }
    res[ord[idx]] <- r
    p <- p + m
  }
  res
}


get_team <- function(dataset,gamedate){
    row_selected <-dataset[dataset$date == gamedate,]
    location <- row_selected$location
    opponent<- as.character(row_selected$opponet)
    duke <- "Duke"
    
    home_team = ""
    away_team = ""
    
    if(location == 'vs'){
      home_team <- duke
      away_team <- opponent
    }
    else{
      home_team <- opponent
      away_team <-duke
    }
    return(list(home_team,away_team))
}


# credit for team color database (contained primary and secondary colors)-
# https://www.reddit.com/r/CFB/comments/5vln5y/i_created_a_google_sheets_with_the_hex_codes_of/

# edited to add more options for colors (explained below), added Duke opponets from 2014-2019 that weren't in dataset
# Color 1-true main color (from Reddit source)
# Color 2- true secondary color (from Reddit source)
# Color 3- main color unless it is close to Duke Blue, then changed to be Black- used for gameflow line chart
# Color 4- secondary color unless secondary color was white- then changed to orange/yellow- used for labels on 
# temporal network since background was white


colors <- read.csv("Team_Colors.csv",stringsAsFactors=F)


generate_colors <- function(team) {
  
 
  
  # if team is in database, use their colors
  # if not listed, use default colors (red and black)
  if( team %in% colors$Team){
    #primary <- colors[colors$Team == team, 2]
    primary <- colors$Color.1[colors$Team == team]
    #secondary <- colors[colors$Team == team, 3]
    secondary <- colors$Color.2[colors$Team == team]
    third <- colors$Color.3[colors$Team == team]
    fourth <- colors$Color.4[colors$Team == team]
  }
  else {
    primary <- "#f60404"
    secondary <- "#000000"
    third <- "#f60404"
    fourth <- "#000000"
  }
  return(list(primary, secondary, third, fourth))
  
}



get_urls <- function(season_df, date) {
  
  pbp_url <- season_df[season_df$date == date,]$pbp_url 
  boxscore_url <- season_df[season_df$date == date,]$boxscore_url
  teamstats_url <- season_df[season_df$date == date,]$teamstats_url
  
  list_urls <- list(pbp_url, boxscore_url, teamstats_url)
  return(list_urls)
  
  
}







visualize_home_network <- function(all_data_frames) {
  nodes_and_adges <- all_data_frames[[3]]
  make_home_network(nodes_and_adges)
  
}


make_home_network <- function (nodes_and_edges) {
  
  # generate colors for edges
  # vals <- brewer.pal(n = 9, name = "PuBu")
  # i <- 4
  # list_colors <- vector()
  # while(i < 9) {
  #     list_colors <- c(list_colors, vals[i])
  #     i <- i + 1
  # }
  # pal <- list_colors
  
  
  # load data from input
  cut_home_points <- nodes_and_edges[[1]]
  home_assists_notime <- nodes_and_edges[[2]]
  team_home <- nodes_and_edges[[5]]
  cut_home_all_stats <- nodes_and_edges[[7]]
  
  # get colors for each team by calling function
  home_colors <- generate_colors(team_home)
  
  # generate colors for edges
  shadesOfGrey <- colorRampPalette(c("grey60", "grey5"))
  n_colors <- max(cut_home_all_stats$assists)
  pal <- shadesOfGrey(n_colors)
  
  # set up for network
  cut_list_home_players <-   cut_home_points$name
  cut_list_home_points <- as.numeric(cut_home_points$points)
  
  
  home_weighted_net <- graph_from_data_frame(d= home_assists_notime, vertices=cut_list_home_players, directed=T) 
  
  # set graph attributes
  #V(home_weighted_net)$size <- sqrt(cut_list_home_points) * 5
  #V(home_weighted_net)$size <- cut_list_home_points 
  
  V(home_weighted_net)$size <- case_when(cut_list_home_points == 0 ~ 0,
                                         (cut_list_home_points >= 1 & cut_list_home_points < 5) ~ 5,
                                         (cut_list_home_points >= 5 & cut_list_home_points < 10) ~ 10,
                                         (cut_list_home_points >= 10 & cut_list_home_points < 20) ~ 17,
                                         (cut_list_home_points >= 20 & cut_list_home_points < 30) ~ 25,
                                         (cut_list_home_points >= 30) ~ 40)
  
  
  
  V(home_weighted_net)$color <- as.character(home_colors[[1]])
  V(home_weighted_net)$frame.color <- as.character(home_colors[[1]])
  V(home_weighted_net)$label.cex <- 1.5
  V(home_weighted_net)$label.font <- 2
  V(home_weighted_net)$label.color <- as.character(home_colors[[2]])
  
  E(home_weighted_net)$color <- pal[E(home_weighted_net)$count]
  E(home_weighted_net)$width <- home_assists_notime$count *3
  
  #curves = autocurve.edges2(home_weighted_net)
  
  
  
  
  # convert to visNetwork
  home_visnet <- toVisNetworkData(home_weighted_net, idToLabel = TRUE)
  home_nodes <- home_visnet$nodes
  home_edges <- home_visnet$edges
  
  # add labels for when you hover
  home_nodes$title <- paste0(cut_list_home_players, "<br>",
                             "Points: ", cut_home_points$points,"<br>", 
                             "Assists: ", cut_home_all_stats$assists, "</p>")
  
  home_edges$title <- paste0("Count from ", home_assists_notime$assist_f.last, "<br>",
                             " to ", home_assists_notime$scorer_f.last, ": ",
                             home_assists_notime$count, "</p>")
  
  # prevents bidirectional arrows
  home_edges$smooth <- TRUE
  
  # make labels inside nodes- min size of 3 font so they are still labeled
  home_nodes$shape <- "circle"
  #home_nodes$font.size = sqrt(as.numeric(cut_home_points$points))* 5 
  #home_nodes$font.size = as.numeric(cut_home_points$points)
  
  home_nodes$font.size<- case_when(cut_list_home_points == 0 ~ 0,
                                   (cut_list_home_points >= 1 & cut_list_home_points < 5) ~ 5,
                                   (cut_list_home_points >= 5 & cut_list_home_points < 10) ~ 10,
                                   (cut_list_home_points >= 10 & cut_list_home_points < 20) ~ 17,
                                   (cut_list_home_points >= 20 & cut_list_home_points < 30) ~ 25,
                                   (cut_list_home_points >= 30) ~ 40)
  
  home_nodes$font.size <- case_when(nchar(home_nodes$id) >= 15 ~ home_nodes$font.size * 0.6,
                                    (nchar(home_nodes$id) < 15 & nchar(home_nodes$id) > 12) ~ home_nodes$font.size * 0.7,
                                    (nchar(home_nodes$id) <= 12  & nchar(home_nodes$id) > 6 ) ~ home_nodes$font.size,
                                    nchar(home_nodes$id) <= 6 ~ home_nodes$font.size * 1.5)
  
  home_nodes$font.color = as.character(home_colors[[2]])
  home_edges$color <- pal[home_edges$count]
  
  
  
  home_vis <- visNetwork(home_nodes, home_edges, height = "600px", width = "100%", main = paste(team_home, "Points/Assists Network")) %>%
    #visIgraphLayout(layout = "layout_with_graphopt") %>%
    #visNodes(scaling = list(min = 10, max = 100)) %>%
    #visOptions(autoResize = TRUE) %>% 
    #highlightNearest = TRUE, 
    #nodesIdSelection = TRUE) %>%
    visInteraction(keyboard = TRUE,
                   dragNodes = F, 
                   dragView = F, 
                   zoomView = F) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, 
                                     scaleFactor = 0.5, type = 'arrow'))) %>%
    #visPhysics(barnesHut = list(avoidOverlap = 10))
    visPhysics(solver = "repulsion", stabilization = TRUE,  
               forceAtlas2Based = list(gravitationalConstant = -200))
  

}




visualize_away_network <- function(all_data_frames) {
  nodes_and_adges <- all_data_frames[[3]]
  make_away_network(nodes_and_adges)
  
}

make_away_network <- function (nodes_and_edges) {
  
  # generate colors for edges
  # vals <- brewer.pal(n = 9, name = "PuBu")
  # i <- 4
  # list_colors <- vector()
  # while(i < 9) {
  #     list_colors <- c(list_colors, vals[i])
  #     i <- i + 1
  # }
  # pal <- list_colors
  
  
  # load data from input
  cut_away_points <- nodes_and_edges[[3]]
  away_assists_notime <- nodes_and_edges[[4]]
  team_away <- nodes_and_edges[[6]]
  cut_away_all_stats <- nodes_and_edges[[8]]
  
  # get colors for each team by calling function
  away_colors <- generate_colors(team_away)
  
  
  # generate colors for edges
  shadesOfGrey <- colorRampPalette(c("grey60", "grey5"))
  n_colors <- max(cut_away_all_stats$assists)
  pal <- shadesOfGrey(n_colors)
  
  # set up for network
  cut_list_away_players <-   cut_away_points$name
  cut_list_away_points <- as.numeric(cut_away_points$points)
  
  away_weighted_net <- graph_from_data_frame(d= away_assists_notime, vertices=cut_list_away_players, directed=T) 
  
  
  # set graph attributes
  #V(away_weighted_net)$size <- sqrt(cut_list_away_points) * 5
  #V(away_weighted_net)$size <- cut_list_away_points * 3
  V(away_weighted_net)$size <- case_when(cut_list_away_points == 0 ~ 0,
                                         (cut_list_away_points >= 1 & cut_list_away_points < 5) ~ 5,
                                         (cut_list_away_points >= 5 & cut_list_away_points < 10) ~ 10,
                                         (cut_list_away_points >= 10 & cut_list_away_points < 20) ~ 17,
                                         (cut_list_away_points >= 20 & cut_list_away_points < 30) ~ 25,
                                         (cut_list_away_points >= 30) ~ 40)
  
  # V(away_weighted_net)$size <- case_when(cut_list_away_points > 10 ~ 15,
  #                                        cut_list_away_points > 20 ~ 25,
  #                                        cut_list_away_points > 30 ~ 35
  #                                        )
  
  V(away_weighted_net)$color <- as.character(away_colors[[1]])
  V(away_weighted_net)$frame.color <- as.character(away_colors[[1]])
  V(away_weighted_net)$label.cex <- 1.5
  V(away_weighted_net)$label.font <- 2
  V(away_weighted_net)$label.color <- as.character(away_colors[[2]])
  E(away_weighted_net)$color <- pal[E(away_weighted_net)$count]
  E(away_weighted_net)$width <- away_assists_notime$count * 3
  
  #curves = autocurve.edges2(away_weighted_net)
  
  # convert to visNetwork
  away_visnet <- toVisNetworkData(away_weighted_net, idToLabel = TRUE)
  away_nodes <-away_visnet$nodes
  away_edges <- away_visnet$edges
  
  # add labels for when you hover
  # away_label_names <- cut_list_away_players
  # 
  # for (name in away_label_names) {
  #   
  #   if(nchar(name) >= 13) {
  #     # new_name <- ""
  #     # first_part <- substr(name, 0, 10)
  #     # sec_part <- substr(name, 11, nchar(name))
  #     # new_name <- paste(first_part, "\n", sec_part)
  #     # name <- cat(new_name)
  #     name <- paste(strwrap(name,5), collapse="\n")
  # 
  #     }
  # }
  
  
  away_nodes$title <- paste0(cut_list_away_players, "<br>",
                             "Points: ", cut_away_points$points,"<br>", 
                             "Assists: ", cut_away_all_stats$assists, "</p>")
  
  away_edges$title <- paste0("Count from ", away_assists_notime$assist_f.last, "<br>",
                             " to ", away_assists_notime$scorer_f.last, ": ",
                             away_assists_notime$count, "</p>")
  
  # prevents bidirectional arrows
  away_edges$smooth <- TRUE
  
  # make labels inside nodes- min size of 3 font so they are still labeled
  away_nodes$shape <- "circle"
  #away_nodes$font.size = sqrt(as.numeric(cut_away_points$points)) * 5
  #away_nodes$font.size = as.numeric(cut_away_points$points) * 2
  away_nodes$font.size <- case_when(cut_list_away_points == 0 ~ 0,
                                    (cut_list_away_points >= 1 & cut_list_away_points < 5) ~ 5,
                                    (cut_list_away_points >= 5 & cut_list_away_points < 10) ~ 10,
                                    (cut_list_away_points >= 10 & cut_list_away_points < 20) ~ 17,
                                    (cut_list_away_points >= 20 & cut_list_away_points < 30) ~ 25,
                                    (cut_list_away_points >= 30) ~ 40)
  
  away_nodes$font.size <- case_when(nchar(away_nodes$id) >= 15 ~ away_nodes$font.size * 0.6,
                                    (nchar(away_nodes$id) < 15 & nchar(away_nodes$id) > 12) ~ away_nodes$font.size * 0.8,
                                    (nchar(away_nodes$id) <= 12  & nchar(away_nodes$id) > 6 ) ~ away_nodes$font.size,
                                    nchar(away_nodes$id) <= 6 ~ away_nodes$font.size * 1.5)
  
  away_nodes$font.color = as.character(away_colors[[2]])
  away_edges$color <- pal[away_edges$count]
  
  away_vis <- visNetwork(away_nodes, away_edges, height = "600px", width = "100%", main = paste(team_away, "Points/Assists Network")) %>%
    #visIgraphLayout(layout = "layout_nicely") %>%
    #visNodes(scaling = list(min = 10, max = 70)) %>%
    #visOptions(autoResize = TRUE) %>% 
    #highlightNearest = TRUE, 
    #nodesIdSelection = TRUE) %>%
    visInteraction(keyboard = TRUE,
                   dragNodes = F, 
                   dragView = F, 
                   zoomView = F) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, 
                                     scaleFactor = 0.5, type = 'arrow'))) %>%
    #visPhysics(barnesHut = list(avoidOverlap = 10))
    visPhysics(solver = "forceAtlas2Based", stabilization = TRUE,
               forceAtlas2Based = list(gravitationalConstant = -200))

}





make_table <- function(all_data_frames) {
  team_stats <- all_data_frames[[4]]
  nodes_and_edges <- all_data_frames[[3]]
  home_team <- nodes_and_edges[[5]]
  away_team <- nodes_and_edges[[6]]
  
  new_vars <- c("Field Goal %", "Three Point %", "Rebounds", "Total Turnovers", "Assists")
  team_stats2 <- team_stats[team_stats$category %in% new_vars, ]
  
  team_stats2$winner <- ifelse(team_stats2$category == "Total Turnovers", 
                               case_when(team_stats2$away_stats > team_stats2$home_stats ~ home_team,
                                         team_stats2$away_stats < team_stats2$home_stats ~ away_team),
                               
                               case_when(team_stats2$away_stats > team_stats2$home_stats ~ away_team,
                                         team_stats2$away_stats < team_stats2$home_stats ~ home_team,
                                         team_stats2$away_stats == team_stats2$home_stats ~ "tie"))
  
  
  team_stats2$color <- case_when(team_stats2$winner == away_team ~ generate_colors(away_team)[[1]],
                                 team_stats2$winner == home_team ~ generate_colors(home_team)[[1]],
                                 team_stats2$winner == "tie" ~ "#FFFFFF")
  
  colors_levels <- c(generate_colors(away_team)[[1]], generate_colors(home_team)[[1]])
  
  columns2hide <- c(3,4)
  
  
  
  
  DT::datatable(team_stats2, colnames = c(away_team, "Category", home_team), options=list(dom = 't', ordering = F, columnDefs = list(list(className = 'dt-body-center', visible=FALSE, targets=columns2hide)))
                   , rownames= FALSE)%>% formatStyle(
                     'category', 'winner',
                     backgroundColor = styleEqual(c(away_team, home_team), c(colors_levels[1], colors_levels[2])),
                     color = styleEqual(c(away_team, home_team, "tie"), c("#FFFFFF", "#FFFFFF", "#000000")), `text-align` = 'center') %>%
     formatStyle('away_stats', `text-align` = 'center') %>%
     formatStyle('home_stats',`text-align` = 'center')

 }






visualize_gameflow<- function(all_data_frames){
  pbp_dataset <- all_data_frames[[1]]
  home_team <- pbp_dataset$home_team[1]
  away_team <- pbp_dataset$away_team[1]
  
  pbp_dataset$home_score <- as.numeric(pbp_dataset$home_score)
  pbp_dataset$away_score <- as.numeric(pbp_dataset$away_score)
  pbp_dataset$time_min <- as.numeric(pbp_dataset$time_min)
  
  home_color <- generate_colors(home_team)[[3]]
  away_color <- generate_colors(away_team)[[3]]
  # 
  # 
  # value1 <- as.numeric(pbp_dataset$home_score[nrow(pbp_dataset)])
  # value2 <- as.numeric(pbp_dataset$away_score[nrow(pbp_dataset)])
  # 
  # Set margin parameters for plot
  m <- list(
    l = 50,
    r = 98,
    b = 60,
    t = 60,
    pad = 4
  )
  
  plot_ly(pbp_dataset, x=~time_min, y=~home_score, name=home_team, type='scatter', mode = 'lines', 
          line = list(color = home_color, width = 2))%>%
    add_trace(y = ~away_score, name = away_team, line = list(color = away_color, width = 2)) %>%
    layout(
      width = 450, height = 350, margin = m,
      legend = list(x = 0.07, y = 0.95),
      xaxis = list(title = "Time(min)",
                   zeroline = FALSE
      ),
      yaxis = list (title = "Score",
                    zeroline = FALSE
      )
    )
  
  
  
  
  
}





# this function scrapes and cleans data we will need for all visualizations
# returns list of all dataframes needed
# each visualization will have all_data_frames as parameter and then will use only the one it needs
setup <- function (season_df, date) {
  
  urls <- get_urls(season_df, date)
  pbp_url <- as.character(urls[[1]])
  boxscore_url <- as.character(urls[[2]])
  teamstats_url <- as.character(urls[[3]])
  
  
  pbp <- scrape_pbp(url = pbp_url)
  clean_pbp <- clean_pbp(pbp = pbp)
  team_stats <- scrape_teamstats(url = teamstats_url)
  
  both_all_stats <- scrape_and_clean_boxscore(url = boxscore_url)
  
  nodes_and_edges <- clean_for_network(pbp = clean_pbp, list_both_all_stats = both_all_stats)
  
  all_data_frames <- list(clean_pbp, both_all_stats, nodes_and_edges, team_stats)
  return(all_data_frames)
}












# R SHINY HELPER FUNCTIONS

# makes string for HTML selection bar that describes game matchup
logofunction <- function(dataset){
  
  logo <-c()
  
  for (i in 1:nrow(dataset)) {
    if(dataset$location[i] == 'vs'){
      logo <- append(logo,sprintf("<img src= '%s' width=30px><div class='jhr'> vs.</div></img> <img src='%s' width=30px><div class='jhr'>%s</div></img>",dataset$duke_logo_url[i],dataset$oppon_logo_url[i],dataset$date[i]))
    }
    else{logo <- append(logo,sprintf("<img src= '%s' width=30px><div class='jhr'> vs.</div></img> <img src='%s' width=30px><div class='jhr'>%s</div></img>",dataset$oppon_logo_url[i],dataset$duke_logo_url[i],dataset$date[i]))}
    
  }
  return(logo)
  
}


