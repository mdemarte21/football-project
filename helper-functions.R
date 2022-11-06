
# Load packages
library(tidyverse)
library(lme4)
library(mgcv)
library(caret)
library(xgboost)
library(tidymodels)
library(rjson)
library(cowplot)
library(jsonlite)
library(zoo)
library(nnet)

#' Helper function to load all game ids

load_game_ids <- function(){
  ids <- c(2068, 2269, 2417, 2440, 2841, 3442, 3518, 3749, 4039)
  return(ids)
}

#' Helper function to load match meta data

load_meta_data <- function(){
  df <- fromJSON("git/football-project/data/matches.json")
  df <- as.data.frame(df)
  df$home_team <- df$home_team$short_name
  df$away_team <- df$away_team$short_name
  return(df)
}

#' Helper function to load the match data
#' 
#' @param match_id what match to pull data for
#' 
#' @example for match_id = 2068
#' load_player_data(2068)

load_player_data <- function(match_id){
  df <- fromJSON(paste0("git/football-project/data/matches/", match_id,"/match_data.json")) 
  home_team <- as.data.frame(df$home_team) %>% dplyr::select(id, short_name) %>% rename(club_name = short_name)
  away_team <- as.data.frame(df$away_team) %>% dplyr::select(id, short_name) %>% rename(club_name = short_name)
  df <- as.data.frame(df$players)
  df$position_short <- df$player_role$acronym
  df$pos_id <- df$player_role$id
  df$position_name <- df$player_role$name
  df$player_role <- NULL
  df$match_id <- match_id
  df <- bind_rows(
    df %>% inner_join(home_team, by = c("team_id" = "id")),
    df %>% inner_join(away_team, by = c("team_id" = "id"))
  )
  return(df)
}

#' Helper function to load referee data for the match
#' 
#' @param match_id what match to pull data for
#' 
#' @example for match_id - 2068
#' load_referee_data(2068)

load_referee_data <- function(match_id){
  df <- fromJSON(paste0("git/football-project/data/matches/", match_id,"/match_data.json")) 
  df <- as.data.frame(df$referees)
  if(nrow(df) == 0){
    return(tibble())
  }else{
    df$match_id <- match_id
    return(df) 
  }
}

#' Helper function to load the structured match data
#' 
#' @param match_id what match to pull data for
#' 
#' @example for match_id = 2068
#' load_structured_data(2068)

load_structured_data <- function(match_id){
  df <- fromJSON(paste0("git/football-project/data/matches/", match_id,"/structured_data.json")) 
  df <- as.data.frame(df)
  df$trackable_object <- df$possession$trackable_object
  df$group <- df$possession$group
  df$possession <- NULL
  df <- df %>%
    mutate(
      filter = case_when(
        is.na(period) & is.na(time) & is.na(trackable_object) & is.na(group) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(filter == 0) %>% 
    dplyr::select(-filter)
  df <- df %>% rename(tracked_object = trackable_object) %>% unnest(data)
  df$match_id <- match_id
  return(df)
}

#' Helper function to return the data frames for all 9 games from a specific function
#' 
#' @param func which function to use
#' 
#' @example for player_data
#' load_data(player_data)

load_data <- function(func){
  ids <- load_game_ids()
  if(func == "player_data"){
    df <- bind_rows(load_player_data(ids[1]), load_player_data(ids[2]), load_player_data(ids[3]), load_player_data(ids[4]), load_player_data(ids[5]), 
                          load_player_data(ids[6]), load_player_data(ids[7]), load_player_data(ids[8]), load_player_data(ids[9]))
  }else if(func == "ref_data"){
    df <- bind_rows(load_referee_data(ids[1]), load_referee_data(ids[2]), load_referee_data(ids[3]), load_referee_data(ids[4]), load_referee_data(ids[5]), 
                    load_referee_data(ids[6]), load_referee_data(ids[7]), load_referee_data(ids[8]), load_referee_data(ids[9]))
  }else if(func == "structured_data"){
    df <- bind_rows(load_structured_data(ids[1]), load_structured_data(ids[2]), load_structured_data(ids[3]), load_structured_data(ids[4]), load_structured_data(ids[5]), 
                    load_structured_data(ids[6]), load_structured_data(ids[7]), load_structured_data(ids[8]), load_structured_data(ids[9]))
  }
    
  return(df)
}

#' Helper function to create goalie pitch plots
#' 
#' @param df data to use for the plot, specifically the goalie data
#' 
#' @param match match to use
#' 
#' @example for 2068
#' goalie_pitch_plot(goalie_data, 2068)

goalie_pitch_plot <- function(df, match){
  df <- df %>% filter(match_id == match)
  plot_grid(
    plot_pitch(theme = "dark") +
      geom_point(data = df %>% filter(period == 1) %>% mutate(x = x + 52.5, y = y + 34), aes(x = x, y = y, col = goalie)) +
      ggtitle(paste("1st Half H:", unique(df$home_team), "vs.", "A:", unique(df$away_team), unique(df$match_id))) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = .5, size = 12)
      ),
    plot_pitch(theme = "dark") +
      geom_point(data = df %>% filter(period == 2) %>% mutate(x = x + 52.5, y = y + 34), aes(x = x, y = y, col = goalie)) +
      ggtitle(paste("2nd Half H:", unique(df$home_team), "vs.", "A:", unique(df$away_team), unique(df$match_id))) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(hjust = .5, size = 12)
      ),
    ncol = 1
  )
}

#' Helper function to clean the player tracks to prepare for the model data
#' 
#' @param df data to use
#' 
#' @example for model data
#' clean_trackable_objects(model_data)

clean_trackable_objects <- function(df){
  # Get all null tracks
  null_tracks <- df %>%
    filter(is.na(trackable_object)) %>%
    dplyr::select(match_id, track_id) %>%
    distinct() 
  
  # Get all null tracks data
  null_tracks <- df %>%
    inner_join(null_tracks, by = c("match_id", "track_id"))
  
  # Remove null tracks from model data
  df <- df %>% anti_join(null_tracks, by = c("match_id", "track_id"))
  
  # Fill in null tracks
  tracks <- null_tracks %>%
    mutate(is_null = ifelse(is.na(trackable_object), 1, 0)) %>%
    group_by(match_id, track_id) %>%
    summarise(
      n = n(),
      is_null = sum(is_null)
    ) %>%
    mutate(tot = n - is_null) %>% 
    filter(tot > 0) %>%
    dplyr::select(match_id, track_id)
  
  # Remove null tracks
  null_tracks <- null_tracks %>% 
    inner_join(tracks, by = c("match_id", "track_id")) %>%
    arrange(match_id, track_id, frame)
  
  matches <- unique(null_tracks$match_id)
  final_df <- tibble()
  for(m in 1:length(matches)){
    print(paste(matches[m], Sys.time()))
    match_data <- null_tracks %>% filter(match_id == matches[m])
    tracks <- unique(match_data$track_id)
    for (t in 1:length(tracks)) {
      track_data <- match_data %>% filter(track_id == tracks[t])
      if(is.na(track_data$trackable_object[1])){
        frame_stop <- track_data %>% filter(!(is.na(trackable_object))) %>% filter(frame == min(frame)) %>% pull(frame)
        top <- track_data %>% filter(frame <= frame_stop) %>% fill(trackable_object, .direction = c("up")) %>% filter(frame != frame_stop)
        bottom <- track_data %>% filter(frame >= frame_stop) %>% fill(trackable_object)
        track_data1 <- bind_rows(top, bottom)
      }else{
        track_data1 <- track_data %>% fill(trackable_object) 
      }
      final_df <- bind_rows(final_df, track_data1)
    }
  }
  
  df <- bind_rows(df, final_df)
  return(df)
}

#' Helper function to prepare model data
#' 
#' @param df data to use
#' 
#' @example for coordinate data
#' prepare_model_data(coordinate_data)

prepare_model_data <- function(df){
  # Get the distance from the ball
  ball_data <- df %>%
    filter(trackable_object == 55) %>%
    dplyr::select(match_id, frame, x, y, period) %>%
    rename(x_ball = x, y_ball = y)
  
  df <- df %>%
    left_join(ball_data, by = c("match_id", "frame", "period")) %>%
    mutate(
      ball_dist = case_when(
        !(is.na(x_ball)) & !(is.na(x)) ~ sqrt((x_ball - x)^2 + (y_ball - y)^2)
      )
    ) %>%
    dplyr::select(-x_ball, -y_ball)
  
  # Standardize data
  df <- df %>%
    mutate(
      mod_x = case_when(
        period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) ~ x * -1,
        period == 1 & match_id %in% c(2269, 2417, 2440) ~ x * -1,
        TRUE ~ x
      ),
      mod_y = case_when(
        period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) ~ y * -1,
        period == 1 & match_id %in% c(2269, 2417, 2440) ~ y * -1,
        TRUE ~ y
      )
    )
  
  # Get distance from midfield and goal
  df <- df %>%
    mutate(
      mid_dist = sqrt(mod_x^2 + mod_y^2),
      goal_dist = case_when(
        is_home == "home" ~ sqrt((-50 - mod_x)^2 + mod_y^2),
        TRUE ~ sqrt((50 - mod_x)^2 + mod_y^2)
      )
    )
  return(df)
}

#' Helper function to aggregate data for smaller model
#' 
#' @param df data to use
#' 
#' @example for modeling_data
#' aggregate_model_data(modeling_data)

aggregate_model_data <- function(df){
  # Get initial aggregate thirds
  full_agg <- df %>% 
    group_by(match_id, trackable_object, position_short, unit) %>%
    summarise(
      n = n(),
      defensive_third = sum(defensive_third),
      middle_third = sum(middle_third),
      attacking_third = sum(attacking_third)
    ) %>%
    ungroup() %>%
    mutate(
      time = defensive_third + middle_third + attacking_third,
      defensive_third = defensive_third / time,
      middle_third = middle_third / time,
      attacking_third = attacking_third / time
    ) %>%
    dplyr::select(-time)
  
  # Rates a player spent in each third depending what third the balls was in
  rate_per_ball <- df %>%
    filter(!(is.na(ball_loc))) %>%
    group_by(match_id, trackable_object, position_short, unit, is_home, ball_loc) %>%
    summarise(
      defensive_third = sum(defensive_third),
      middle_third = sum(middle_third),
      attacking_third = sum(attacking_third)
    ) %>%
    ungroup() %>%
    mutate(
      ball_loc = case_when(
        is_home == "away" & ball_loc == "Home Defense" ~ "Offense",
        is_home == "away" & ball_loc == "Home Offense" ~ "Defense", 
        ball_loc == "Home Defense" ~ "Defense",
        ball_loc == "Home Offense" ~ "Offense",
        TRUE ~ ball_loc
      )
    ) %>%
    mutate(
      time = defensive_third + middle_third + attacking_third,
      defensive_third = defensive_third / time,
      middle_third = middle_third / time,
      attacking_third = attacking_third / time
    ) %>%
    dplyr::select(-time) %>%
    pivot_wider(names_from = ball_loc, values_from = c(defensive_third, middle_third, attacking_third)) %>%
    rename(ball_def_loc_def = defensive_third_Defense, ball_def_loc_mid = middle_third_Defense, ball_def_loc_att = attacking_third_Defense,
           ball_att_loc_def = defensive_third_Offense, ball_att_loc_mid = middle_third_Offense, ball_att_loc_att = attacking_third_Offense,
           ball_mid_loc_def = `defensive_third_Middle Third`, ball_mid_loc_mid = `middle_third_Middle Third`, ball_mid_loc_att = `attacking_third_Middle Third`) %>%
    dplyr::select(-is_home)
    
    
  # Distance from goal & midline in each third
  dist_goal <- df %>%
    filter(!(is.na(ball_loc))) %>%
    group_by(match_id, trackable_object, position_short, unit, is_home, ball_loc) %>%
    summarise(
      mid_dist = mean(mid_dist),
      goal_dist = mean(goal_dist)
    ) %>%
    ungroup() %>%
    mutate(
      ball_loc = case_when(
        is_home == "away" & ball_loc == "Home Defense" ~ "Offense",
        is_home == "away" & ball_loc == "Home Offense" ~ "Defense", 
        ball_loc == "Home Defense" ~ "Defense",
        ball_loc == "Home Offense" ~ "Offense",
        TRUE ~ ball_loc
      )
    ) %>%
    pivot_wider(names_from = ball_loc, values_from = c(mid_dist, goal_dist)) %>%
    rename(ball_def_mid_dist = mid_dist_Defense, ball_att_mid_dist = mid_dist_Offense, ball_mid_mid_dist = `mid_dist_Middle Third`,
           ball_def_goal_dist = goal_dist_Defense, ball_att_goal_dist = goal_dist_Offense, ball_mid_goal_dist = `goal_dist_Middle Third`) %>%
    dplyr::select(-is_home)
  
  final_df <- full_agg %>%
    inner_join(rate_per_ball, by = c("match_id", "trackable_object", "position_short", "unit")) %>%
    inner_join(dist_goal, by = c("match_id", "trackable_object", "position_short", "unit"))
  
  return(final_df)
    
}

#' Helper function to create graph to dig deeper on with KNN
#' 
#' @param df data to use
#' 
#' @param graph to create
#' 
#' @example for defensive_third
#' graph_to_help_understand_model(ext_data, "defensive_third")

graph_to_help_understand_model <- function(df, graph){
  if(graph == "defensive_third"){
    df$y <- df$defensive_third
  }else if(graph == "middle_third"){
    df$y <- df$middle_third
  }else if(graph == "attacking_third"){
    df$y <- df$attacking_third
  }else if(graph == "ball_att_loc_def"){
    df$y <- df$ball_att_loc_def
  }else if(graph == "ball_att_loc_mid"){
    df$y <- df$ball_att_loc_mid
  }else if(graph == "ball_att_loc_att"){
    df$y <- df$ball_att_loc_att
  }else if(graph == "ball_def_loc_def"){
    df$y <- df$ball_def_loc_def
  }else if(graph == "ball_def_loc_mid"){
    df$y <- df$ball_def_loc_mid
  }else if(graph == "ball_def_loc_att"){
    df$y <- df$ball_def_loc_att
  }else if(graph == "ball_mid_loc_def"){
    df$y <- df$ball_mid_loc_def
  }else if(graph == "ball_mid_loc_mid"){
    df$y <- df$ball_mid_loc_mid
  }else if(graph == "ball_mid_loc_att"){
    df$y <- df$ball_mid_loc_att
  }else if(graph == "ball_def_mid_dist"){
    df$y <- df$ball_def_mid_dist
  }else if(graph == "ball_mid_mid_dist"){
    df$y <- df$ball_mid_mid_dist
  }else if(graph == "ball_att_mid_dist"){
    df$y <- df$ball_att_mid_dist
  }else if(graph == "ball_def_goal_dist"){
    df$y <- df$ball_def_goal_dist
  }else if(graph == "ball_mid_goal_dist"){
    df$y <- df$ball_mid_goal_dist
  }else if(graph == "ball_att_goal_dist"){
    df$y <- df$ball_att_goal_dist
  }
  
  df <- df %>% mutate(correct = as.factor(as.character(correct)))
  
  ggplot(data = df, aes(x = unit, y = y, col = correct, size = correct)) +
    geom_point() + 
    scale_color_manual(values = c("0" = "red", "1" = "green")) +
    scale_size_manual(values = c("0" = 3, "1" = 1.5)) +
    theme_classic() +
    ggtitle(graph) +
    labs(y = graph) +
    theme(plot.title = element_text(hjust = .5, size = 12, face = "bold"))
}
