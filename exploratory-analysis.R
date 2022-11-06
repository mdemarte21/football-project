
# Source the helper functions
source("git/football-project/helper-functions.R")
# Load in function to get pitch plot
source("https://raw.githubusercontent.com/larsmaurath/keine-mathematik/master/content/data/pitch_plot.R")

# Load data
ids <- load_game_ids()
match_data <- load_meta_data()
player_data <- load_data("player_data")
ref_data <- load_data("referee_data")
coordinate_data <- load_data("structured_data")

# Standardizing the coordinates so that offsense and defense are on the same scale
# Do this by understanding where the goalies mainly stood
goalie_data <- player_data %>%
  filter(position_short == "GK") %>%
  dplyr::select(trackable_object, match_id, club_name) %>%
  inner_join(coordinate_data, by = c("trackable_object", "match_id")) %>%
  inner_join(match_data, by = c("match_id" = "id")) %>% 
  mutate(
    is_home = ifelse(home_team == club_name, "home", "away"),
    goalie = paste(period, is_home)
  ) %>%
  filter(!(is.na(period)))

# For each game id
goalie_pitch_plot(goalie_data, 2068) # 1st Half home team defends left side
goalie_pitch_plot(goalie_data, 2269) # 1st Half home team defends right side
goalie_pitch_plot(goalie_data, 2417) # 1st Half home team defense right side
goalie_pitch_plot(goalie_data, 2440) # 1st Half home team defense right side
goalie_pitch_plot(goalie_data, 2841) # 1st Half home team defense left side
goalie_pitch_plot(goalie_data, 3442) # 1st Half home team defense left side
goalie_pitch_plot(goalie_data, 3518) # 1st Half home team defense left side
goalie_pitch_plot(goalie_data, 3749) # 1st Half home team defense left side
goalie_pitch_plot(goalie_data, 4039) # 1st Half home team defense left side

# Test flipping data to make sure everything linesup
test_data <- goalie_data %>%
  filter(match_id == 4039) %>%
  mutate(
    x = ifelse(period == 2, x * -1, x),
    y = ifelse(period == 2, y * -1, y) 
  )

plot_grid(
  goalie_pitch_plot(goalie_data, 4039),
  goalie_pitch_plot(test_data, 4039),
  ncol = 2
)

# Everything checks out we should multiply coordinates by -1 to put everything on the same scale
# We'll always put the home team defending the left side of the pitch and the away team on the right side of the pitch to help the model understand the team

# Look further into coordinate to see if there's anything we need to do to update it
model_data <- coordinate_data %>%
  left_join(player_data %>% dplyr::select(trackable_object, last_name, number, position_short, match_id, club_name), by = c("trackable_object", "match_id")) %>%
  inner_join(match_data, by = c("match_id" = "id")) %>% 
  mutate(is_home = ifelse(home_team == club_name, "home", "away")) %>%
  filter(!(is.na(period))) %>%
  # Remove the referee from the tracking data
  anti_join(ref_data %>% dplyr::select(trackable_object), by = c("trackable_object")) %>%
  mutate(group_name = ifelse(is.na(group_name), "no group", group_name)) %>%
  filter(group_name != "referee")

# Get columns that get distance from ball, midfield, and goal line for defensive goal
model_data <- prepare_model_data(model_data)
table(is.na(model_data$y))
table(is.na(model_data$x))
# There is no missing coordinate data in the df

# Do we have a trackable object for every play?
table(is.na(model_data$trackable_object))
# We need to fill in the trackable object for ~ 125000 frames

# Start to understand how we can fix this by isolating frames with no trackable object
untracked_data <- model_data %>% filter(is.na(trackable_object))
# Look at data from one specific track
model_data %>% 
  filter(track_id == untracked_data$track_id[1] & match_id == 2068) %>%
  dplyr::select(match_id, trackable_object, track_id, frame, x, y) %>%
  View()
# Appears we need to fill down the data to complete the trackable objects

# Test how to do this to build into helper function
test_clean_tracking <- model_data %>% 
  filter(track_id == untracked_data$track_id[1]) %>%
  dplyr::select(match_id, trackable_object, track_id, tracked_object, frame, x, y)

test_clean_tracking %>%
  fill(trackable_object) %>%
  View()
# This solution works, let's make sure it works for the entire data

# First let's get ll tracks with null trackable_objects
null_tracks <- model_data %>%
  filter(is.na(trackable_object)) %>%
  dplyr::select(match_id, track_id) %>%
  distinct() 
# Now let's make sure they all have beginnings
null_tracks <- model_data %>%
  inner_join(null_tracks, by = c("match_id", "track_id"))
table(is.na(null_tracks %>%
  group_by(match_id, track_id) %>%
  filter(frame == min(frame)) %>%
  pull(trackable_object)))
# Most tracks are completely null

null_track_summary <- null_tracks %>%
  mutate(is_null = ifelse(is.na(trackable_object), 1, 0)) %>%
  group_by(match_id, track_id) %>%
  summarise(
    n = n(),
    is_null = sum(is_null)
  ) %>%
  mutate(tot = n - is_null) 
# About half of all tracks have some sort of trackable object  

# Let's check on a totally null track
model_data %>% filter(match_id == 2068 & track_id == 156) %>% View()
# Seems like there are simply some tracked objects we will need to remove

# Using fill is kind of a messy
fill_nulls <- null_tracks %>% 
  dplyr::select(match_id, trackable_object, track_id, frame, x, y) %>%
  arrange(match_id, track_id, frame) %>%
  group_by(match_id, track_id) %>%
  fill(trackable_object) 

fill_nulls %>%
  dplyr::select(match_id, track_id, trackable_object) %>%
  distinct() %>%
  group_by(match_id, track_id) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# I figured this out in the helper functions part by filling up and down!
