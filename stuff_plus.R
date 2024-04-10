# Build my own version of Stuff Plus
require(baseballr)
require(tidyverse)
require(dplyr)
require(DBI)
require(RPostgres)
require(Rtools)
require(drat)
require(xgboost)

pw <- {
  "LbrMkt22!"
}

# Connect to pitching database
pitching_db <- DBI::dbConnect(RPostgres::Postgres(),
                              dbname = "pitching",
                              user = "postgres",
                              password = pw,
                              host = "localhost",
                              port = 5432)

base <- "C:/Users/David Wasser/Dropbox/David/Baseball/pitching/stuff-plus"
setwd(base)
set.seed(62704) 

#### Choose features
features <- c("release_speed", "release_spin_rate", "pfx_x", "pfx_z", "release_extension",
              "balls", "strikes", "stand_l", "stand_r", "throw_l", "throw_r", "spin_axis", 
              "release_pos_x", "release_pos_z")

#### Read-in data
# Easily filter to include only swing events (removing bunts because they are weird)
swing_events <- c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")

## Eliminate any swing events that are "weird"
# For some pitches, the description shows a walk even though Statcast says a swing occurred.

# Remove position players pitching and pitchers with less than 100 pitches thrown
# Do this by finding the pitchers who threw less than 100 pitches during 2017-present
# This will throw out some real pitchers, but they will have a tiny sample size anyway
batters_pitching <- tbl(pitching_db, 'statcast') %>%
  group_by(pitcher) %>% 
  summarize(pitches_flag = n()) %>%
  ungroup() %>%
  filter(pitches_flag > 100) %>%
  select(-pitches_flag)

## Remove pitchers batting (except Ohtani and Lorenzen)
# Do this by finding the players who threw at least 100 pitches during 2017-present
pitchers_batting <- tbl(pitching_db, 'statcast') %>% 
  group_by(pitcher) %>% 
  summarize(pitches = n()) %>%
  ungroup() %>%
  filter(pitches >= 100, pitcher != "660271", pitcher!= "547179") %>%
  select(pitcher)

raw_ff <- tbl(pitching_db, 'statcast') %>%
  # Remove batters pitching 
  inner_join(batters_pitching, by = "pitcher") %>%
  # Remove pitchers batting
  anti_join(pitchers_batting, by = c("batter" = "pitcher")) %>%
  # Select only desired pitch type
  filter(pitch_type == "FF") %>%
  # Drop weird observations where both a swing and a walk are recorded simultaneously
  mutate(walkflag = ifelse(grepl("walks", des), 1, 0)) %>%
  filter(walkflag == 0) %>%
  select(-walkflag) %>%
  # Drop unnecessary variables to reduce table size 
  select(-fielder_2, -fielder_2_1, -fielder_3, -fielder_4, -fielder_5, 
         -fielder_6, -fielder_7, -fielder_8, -fielder_9, -spin_rate_deprecated,
         -break_angle_deprecated, -break_length_deprecated, -hit_location, -hc_x, -hc_y,
         -batter, -home_team, -away_team, -on_3b, -on_2b, -on_1b, -tfs_deprecated, 
         -tfs_zulu_deprecated, -hit_distance_sc, -estimated_ba_using_speedangle,
         -estimated_woba_using_speedangle, -woba_value, -woba_denom, -babip_value,
         -iso_value, -home_score, -away_score, -bat_score, -fld_score, -post_away_score,
         -post_home_score, -post_fld_score, -if_fielding_alignment, -of_fielding_alignment,
         -inning, -inning_topbot, -umpire, -pitcher_1, -at_bat_number, -post_bat_score,
         -delta_home_win_exp, -fielding_team, -batting_team, -outs_when_up) %>%
  # Only keep observations where feature variables are not missing
  filter(!is.na(pitch_type), # Pitch type
         !is.na(release_speed), # Velocity
         !is.na(release_spin_rate), # Spin rate
         !is.na(plate_x), # Horizontal position over the plate,
         !is.na(plate_z), # Vertical position over the plate,
         !is.na(pfx_x), # Horizontal movement () 
         !is.na(pfx_z), # Vertical movement ()
         !is.na(release_extension), # Extension
         !is.na(balls), # Balls
         !is.na(strikes), # Strikes
         !is.na(stand), # Batter handedness
         !is.na(release_pos_x), # Horizontal release position
         !is.na(release_pos_z), # Vertical release position
         !is.na(p_throws)) %>% # Pitcher handedness
  # Construct some features
  mutate(throw_r = ifelse(p_throws == "R", 1, 0),
         throw_l = ifelse(p_throws == "L", 1, 0),
         stand_r = ifelse(stand == "R", 1, 0),
         stand_l = ifelse(stand == "L", 1, 0)) %>%
  # Construct outcome variables
  mutate(swing = ifelse(description %in% swing_events, 1, 0),
         calledstrike = ifelse(description == "called_strike", 1, 0),
         foul = ifelse(grepl("foul", description) == TRUE, 1, 0),
         ball = ifelse(description == "ball", 1, 0),
         inplay = ifelse(description == "hit_into_play", 1, 0),
         groundball = ifelse(bb_type == "ground_ball", 1, 0),
         flyball = ifelse(bb_type %in% c("fly_ball", "line_drive", "popup"), 1, 0))

# Select only the numeric vectors required for model:train on 2023 data only
ff_swing2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  select(swing, all_of(features)) %>%
  collect()

clean_ff <- raw_ff %>% 
  select(pitcher, swing, all_of(features))
  collect()

rm(pitchers_batting, batters_pitching)
gc()

#### Aim: predict specific outcomes, will then attach expected run values to those outcomes 
# Expected runs for each count: https://en.wikipedia.org/wiki/Pitch_quantification
# Expected runs for each event: https://library.fangraphs.com/principles/linear-weights/

# For now, just use cross-validated parameters from previous version of model
eta_choice <- 0.2
max_depth_choice <- 15
max_leaves_choice <- 63
subsample_choice <- 1.0
colsample_choice <- 0.6

#### Swing Model ####
# Create training data size
trainSize <- round(0.80*nrow(ff_swing2023))

# Create train and test datasets (randomly select rows from cleaned data)
trainIndex <- sample(nrow(ff_swing2023), trainSize)
trainDF <- ff_swing2023 %>% dplyr::slice(trainIndex)
testDF <- ff_swing2023 %>% dplyr::slice(-trainIndex)

# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -swing)), 
                      label = trainDF$swing)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -swing)), 
                     label = testDF$swing)

# Tune parameters ####
# # Start by choosing eta from a list of candidate values
# paramDF <- tibble(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3))
# 
# # Next convert data frame to a list of lists
# paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
# 
# # Now write a loop to perform a cross validation using each value of paramList
# bestResults <- tibble() # Collect best results here
# 
# pb <- txtProgressBar(style = 3) 
# for(i in seq(length(paramList))) {
#   rwCV <- xgb.cv(params = paramList[[i]], 
#                  data = dtrain, 
#                  nrounds = 200, 
#                  nfold = 10,
#                  early_stopping_rounds = 10,
#                  verbose = TRUE)
#   bestResults <- bestResults %>% 
#     bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
#   gc() # Free unused memory after each loop iteration
#   setTxtProgressBar(pb, i/length(paramList))
# }
# close(pb) # done with the progress bar
# 
# ## View results
# etasearch <- bind_cols(paramDF, bestResults)
# View(etasearch)
# 
# eta_choice <- 0.2
# 
# # Now find optimal depth and leaves at same time
# paramDF <- expand.grid(
#   max_depth = seq(15, 29, by = 2),
#   max_leaves = c(63, 127, 255, 511, 1023, 2047, 4095),
#   eta = eta_choice)
# 
# paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
# bestResults <- tibble()
# 
# 
# pb <- txtProgressBar(style = 3)
# for(i in seq(length(paramList))) {
#   rwCV <- xgb.cv(params = paramList[[i]],
#                  data = dtrain, 
#                  nrounds = 200, 
#                  nfold = 10,
#                  early_stopping_rounds = 10,
#                  verbose = FALSE)
#   bestResults <- bestResults %>% 
#     bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
#   gc() 
#   setTxtProgressBar(pb, i/length(paramList))
# }
# close(pb)
# 
# depth_leaves <- bind_cols(paramDF, bestResults)
# View(depth_leaves) 
# 
# max_depth_choice <- 15
# max_leaves_choice <- 63
# 
# source(paste0(base,"/func/cvGridSearch.R"))
# 
# gc()
# 
# paramDF <- expand.grid(
#   subsample = seq(0.6, 1, by = 0.1),
#   colsample_bytree = seq(0.6, 1, by = 0.1),
#   max_depth = max_depth_choice,
#   max_leaves = max_leaves_choice,
#   eta = eta_choice)
# 
# randsubsets <- GridSearch(paramDF, dtrain)
# 
# subsample_choice <- 1.0
# colsample_choice <- 0.6
# 
# ### Final check using the testing dataset ####
ff_swing_model <- xgb.train(data = dtrain, verbose = 0,
                            watchlist = list(train = dtrain, test = dtest),
                            nrounds = 10000,
                            early_stopping_rounds = 50,
                            max_depth = max_depth_choice,
                            max_leaves = max_leaves_choice,
                            subsample = subsample_choice,
                            colsample_bytree = colsample_choice,
                            eta = eta_choice)

# ff_model$evaluation_log %>% 
#   pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
#   ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()

#### Run the model on the full dataset ####
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff, -c(swing, pitcher))), 
                              label = clean_ff$swing)

# Need to confirm that this is the correct way to make the prediction
clean_ff$ff_stuff <- predict(ff_swing_model, ff_data_matrix)


### Convert to "plus" scale and re-attach pitcher IDs
pitcher_ids <- tbl(pitching_db, 'statcast') %>%
  select(pitcher, player_name) %>%
  distinct() %>%
  collect()

full_data <- tbl(pitching_db, 'statcast') %>%
  filter(description %in% swing_events) %>%
  mutate(walkflag = ifelse(grepl("walks", des), 1, 0)) %>%
  filter(walkflag == 0) %>%
  select(-walkflag) %>%
  collect() %>%
  inner_join(ff_data, by = c("pitcher", "strikes", "balls", "delta_run_exp",
                             "release_speed", "release_spin_rate", "pfx_x", "pfx_z"))


pitcher_level2023 <- full_data %>%
  mutate(year = substr(game_date, 1, 4)) %>%
  filter(year == "2023") %>%
  group_by(pitcher) %>%
  # Count number of pitches
  mutate(n_ff = n()) %>%
  # Multiply stuff by -1 to re-orient for ordinal ranking where higher == better
  mutate(ff_stuff = -1*ff_stuff) %>%
  summarize(avg_ff_pred = mean(ff_stuff),
            n_ff = max(n_ff)) %>%
  ungroup() %>%
  filter(n_ff >= 500) %>%
  mutate(avg_ffstuff_all = mean(avg_ff_pred),
         ff_stuff_plus = round(100*(avg_ff_pred/avg_ffstuff_all))) %>%
  inner_join(pitcher_ids, by = "pitcher") %>%
  arrange(-ff_stuff_plus)

