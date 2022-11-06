
# Source the helper functions
source("git/football-project/helper-functions.R")

# Prepare and clean data -------------------------------------------------------------------------------------------------------------------------
# Load data
match_data <- load_meta_data()
player_data <- load_data("player_data")
ref_data <- load_data("ref_data")
model_data <- load_data("structured_data")

# Get data for modeling
model_data <- model_data %>%
  inner_join(match_data, by = c("match_id" = "id")) %>%
  filter(!(is.na(period))) %>%
  # Remove the referee from the tracking data
  anti_join(ref_data %>% dplyr::select(trackable_object), by = c("trackable_object")) %>%
  mutate(group_name = ifelse(is.na(group_name), "no group", group_name)) %>%
  filter(group_name != "referee")

# Clean the trackable objects column to get to help with making predictions
# A helper function was created to do this
model_data <- clean_trackable_objects(model_data)

# Add in player data
model_data <- model_data %>%
  left_join(player_data %>%
              dplyr::select(trackable_object, last_name, number, position_short, match_id, club_name), 
            by = c("trackable_object", "match_id"))  %>% 
  mutate(is_home = ifelse(home_team == club_name, "home", "away")) 

# Filling missing data will be time expensive and likely cause a decrease in performance (speed) of model building, therefore, we won't
# fill in for missing data
# If we wanted to fill missing data we would approximate linear movement for players based between when we have measurements for them

# Get columns that get distance from ball, midfield, and goal line for defensive goal
# Helper function was created to do this
model_data <- prepare_model_data(model_data)

# Remove ball data from the data / create separate df to hold it
ball_data <- model_data %>% filter(trackable_object == 55)
model_data <- model_data %>% filter(trackable_object != 55)

# Add in tactical unit - make GK a separate unit
model_data <- model_data %>%
  mutate(
    unit = case_when(
      position_short == "GK" ~ "GK",
      position_short %in% c("CB", "LCB", "LWB", "RCB", "RWB") ~ "DEF",
      position_short %in% c("AM", "CM", "LM", "RM") ~ "MID",
      position_short %in% c("CF", "LF", "LW", "RF", "RW") ~ "ATT"
    ) 
  )

# Create variable for if the ball was in your defensive, middle or attacking third
ball_data <- ball_data %>%
  dplyr::select(match_id, period, frame, time, x, y) %>%
  mutate(
    ball_loc = case_when(
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 ~ "Home Defense",
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 ~ "Home Offense",
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 ~ "Home Offense",
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 ~ "Home Defense",
      period == 1 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 ~ "Home Offense",
      period == 1 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 ~ "Home Defense",
      period == 2 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 ~ "Home Defense",
      period == 2 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 ~ "Home Offense",
      TRUE ~ "Middle Third"
    )
  )

# Create modeling df
modeling_data <- model_data %>%
  dplyr::select(-track_id, -z, -tracked_object, -group, -status, -date_time, -home_team, -away_team, -last_name, -number, -club_name) %>%
  left_join(ball_data %>% dplyr::select(match_id, period, frame, ball_loc), by = c("match_id", "period", "frame"))

# Add variable for where on the field a player was
modeling_data <- modeling_data %>%
  mutate(
    defensive_third = case_when(
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 & is_home == "home" ~ 1,
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 & is_home == "away" ~ 1,
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 & is_home == "home" ~ 1,
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 & is_home == "away" ~ 1,
      period == 1 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 & is_home == "away" ~ 1,
      period == 1 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 & is_home == "home" ~ 1,
      period == 2 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 & is_home == "away" ~ 1,
      period == 2 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 & is_home == "home" ~ 1,
      TRUE ~ 0
    ),
    middle_third = case_when(
      x < 17.5 & x > -17.5 ~ 1,
      TRUE ~ 0
    ),
    attacking_third = case_when(
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 & is_home == "away" ~ 1,
      period == 1 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 & is_home == "home" ~ 1,
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x >= 17.5 & is_home == "away" ~ 1,
      period == 2 & match_id %in% c(2068, 2841, 3442, 3518, 3749, 4039) & x <= -17.5 & is_home == "home" ~ 1,
      period == 1 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 & is_home == "home" ~ 1,
      period == 1 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 & is_home == "away" ~ 1,
      period == 2 & match_id %in% c(2269, 2417, 2440) & x >= 17.5 & is_home == "home" ~ 1,
      period == 2 & match_id %in% c(2269, 2417, 2440) & x <= -17.5 & is_home == "away" ~ 1,
      TRUE ~ 0
    )
  )

# Get final modeling data
# Ths dataframe aggregates the following things
# The percentage of time a player spent in each third
# The percentage of time a player spent in each third, depending on what third of the pitch the ball was in
# A players average distance from their defensive goal, depending on what third of the pitch the ball was in
# A players average distance from the midline, depending on what third of the pitch the ball was in
# A helper function was created to do this
fin_data <- aggregate_model_data(modeling_data) %>%
  mutate(
    ball_att_loc_def = ifelse(is.na(ball_att_loc_def), 0, ball_att_loc_def),
    ball_def_loc_def = ifelse(is.na(ball_def_loc_def), 0, ball_def_loc_def),
    ball_att_loc_mid = ifelse(is.na(ball_att_loc_mid), 0, ball_att_loc_mid),
    ball_def_loc_mid = ifelse(is.na(ball_def_loc_mid), 0, ball_def_loc_mid),
    ball_att_loc_att = ifelse(is.na(ball_att_loc_att), 0, ball_att_loc_att),
    ball_def_loc_att = ifelse(is.na(ball_def_loc_att), 0, ball_def_loc_att),
    ball_att_mid_dist = ifelse(is.na(ball_att_mid_dist), 0, ball_att_mid_dist),
    ball_def_mid_dist = ifelse(is.na(ball_def_mid_dist), 0, ball_def_mid_dist),
    ball_att_goal_dist = ifelse(is.na(ball_att_goal_dist), 0, ball_att_goal_dist),
    ball_def_goal_dist = ifelse(is.na(ball_def_goal_dist), 0, ball_def_goal_dist)
  )

# Add in positions for Oxlade Chamberlin & Mahrez who are only players w/o position assigned to them
fin_data <- fin_data %>%
  mutate(
    unit = case_when(
      trackable_object == 9077 & match_id == 4039 ~ "ATT",
      trackable_object == 515 & match_id == 4039 ~ "DEF",
      TRUE ~ unit
    )
  )

# KNN Model Building -------------------------------------------------------------------------------------------------------------------------
# Process data
preProcValues <- preProcess(x = fin_data,method = c("center", "scale"))

# Partition data between training and testing
set.seed(21)
sample <- sample(nrow(fin_data), nrow(fin_data) * .8)
train <- fin_data[sample,]
test <- fin_data[-sample,]

# Build the model
ctrl <- trainControl(method="repeatedcv",repeats = 3) 
knnFit <- train(factor(unit) ~ defensive_third + middle_third + attacking_third + ball_def_loc_att + ball_def_loc_def + ball_def_loc_mid +
                  ball_mid_loc_att + ball_mid_loc_def + ball_mid_loc_mid + ball_att_loc_att + ball_att_loc_def + ball_att_loc_mid +
                  ball_def_mid_dist + ball_mid_mid_dist + ball_att_mid_dist + ball_def_goal_dist + ball_mid_goal_dist + ball_att_goal_dist,
                data = train,
                method = "knn",
                trControl = ctrl, 
                preProcess = c("center","scale"), tuneLength = 20)
plot(knnFit)

# Cross validation on test data
# Seems we want less neighbors
knnPredict <- predict(knnFit,newdata = test)
#Get the confusion matrix to see accuracy value and other parameter values
# Accuracy is about 82% for midfielders and attackers on test data
confusionMatrix(knnPredict, factor(test$unit))

# Get the confusion matrix for the full data
fin_data$knn_pred <- predict(knnFit,newdata = fin_data)
confusionMatrix(fin_data$knn_pred, factor(fin_data$unit))

# Save the model
saveRDS(knnFit, "git/football-project/tactical-model.RDS")


