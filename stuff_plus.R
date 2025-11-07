# Build my own version of Stuff Plus
require(baseballr)
require(caret)
require(DBI)
require(dplyr)
require(drat)
require(RPostgres)
require(tidyverse)
require(xgboost)

pw <- X

# Connect to pitching database
pitching_db <- DBI::dbConnect(RPostgres::Postgres(),
                              dbname = "pitching",
                              user = "postgres",
                              password = pw,
                              host = "localhost",
                              port = 5432)

base <- "/stuff-plus"
setwd(base)
set.seed(62704) 

#### Read-in user-created functions
source("func/cvGridSearch.R")


#### Choose features
features <- c("release_speed", "release_spin_rate", "pfx_x", "pfx_z", "release_extension",
              "balls", "strikes", "stand_l", "stand_r", "throw_l", "throw_r", "spin_axis", 
              "release_pos_x", "release_pos_z")

#### Read-in data
swing_events <- c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")
no_swing_events <- c("called_strike", "ball", "hit_by_pitch")
contact_events <- c("foul", "hit_into_play", "foul_tip", "foul_bunt", "bunt_foul_tip")
all_outcomes <- c("swing", "noswingevents", "contact", "foul", "bip_events")
bip_list <- c("ground_ball", "line_drive", "fly_ball", "popup")



# Remove position players pitching and pitchers with less than 100 pitches thrown
# Do this by finding the pitchers who threw less than 100 pitches during 2017-present
# This will throw out some real pitchers, but they will have a tiny sample size anyway
batters_pitching <- tbl(pitching_db, 'statcast') %>%
  group_by(pitcher) %>% 
  summarize(pitches_flag = n()) %>%
  ungroup() %>%
  filter(pitches_flag > 100) %>%
  select(-pitches_flag)

raw_ff <- tbl(pitching_db, 'statcast') %>%
  # Remove batters pitching 
  inner_join(batters_pitching, by = "pitcher") %>%
  # Select only desired pitch type
  filter(pitch_type == "FF") %>%
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
         linedrive = ifelse(bb_type == "line_drive", 1, 0),
         flyball = ifelse(bb_type %in% c("fly_ball", "popup"), 1, 0),
         hbp = ifelse(description == "hit_by_pitch", 1, 0),
         contact = ifelse(description %in% contact_events, 1, 0)) %>%
  mutate(noswingevents = ifelse(swing == 1, NaN, ifelse(calledstrike == 1, 0, 
                                                 ifelse(ball == 1, 1, 2))),
         bip_events = NaN)


# Select only the numeric vectors required for model:
# Use only 2023 for training, and split it within season (then 2024 once I have it)
ff_swing2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  select(swing, all_of(features)) %>%
  collect()

ff_noswing2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  filter(description %in% no_swing_events) %>%
  select(noswingevents, all_of(features)) %>%
  collect()

ff_contact2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  filter(description %in% swing_events) %>%
  select(contact, all_of(features)) %>%
  collect()

ff_foul2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  filter(contact == 1) %>%
  select(foul, all_of(features)) %>%
  collect()

ff_bip2023 <- raw_ff %>% 
  filter(game_year == 2023) %>%
  filter(contact == 1) %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  #filter(bb_type %in% bip_list) %>%
  select(bip_events, all_of(features), launch_angle, launch_speed) %>%
  collect()

# Round exit velocities to nearest integer
ff_bip2023$launch_speed <- round(ff_bip2023$launch_speed)

# Ground balls
ff_bip2023$bip_events[ff_bip2023$launch_angle < 10 & ff_bip2023$launch_speed < 90] <- 0
ff_bip2023$bip_events[ff_bip2023$launch_angle < 10 & between(ff_bip2023$launch_speed, 90, 95)] <- 1
ff_bip2023$bip_events[ff_bip2023$launch_angle < 10 & between(ff_bip2023$launch_speed, 96, 100)] <- 2
ff_bip2023$bip_events[ff_bip2023$launch_angle < 10 & between(ff_bip2023$launch_speed, 101, 105)] <- 3
ff_bip2023$bip_events[ff_bip2023$launch_angle < 10 & ff_bip2023$launch_speed >= 106] <- 4

# Line drives
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 10, 24) & ff_bip2023$launch_speed < 90] <- 5
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 10, 24) & between(ff_bip2023$launch_speed, 90, 95)] <- 6
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 10, 24) & between(ff_bip2023$launch_speed, 96, 100)] <- 7
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 10, 24) & between(ff_bip2023$launch_speed, 101, 105)] <- 8
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 10, 24) & ff_bip2023$launch_speed >= 106] <- 9

# Fly balls
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 25, 50) & ff_bip2023$launch_speed < 90] <- 10
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 25, 50) & between(ff_bip2023$launch_speed, 90, 95)] <- 11
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 25, 50) & between(ff_bip2023$launch_speed, 96, 100)] <- 12
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 25, 50) & between(ff_bip2023$launch_speed, 101, 105)] <- 13
ff_bip2023$bip_events[between(ff_bip2023$launch_angle, 25, 50) & ff_bip2023$launch_speed >= 106] <- 14

# Pop ups
ff_bip2023$bip_events[ff_bip2023$launch_angle > 50] <- 15

ff_bip2023 <- ff_bip2023 %>% select(-launch_angle, -launch_speed)



clean_ff <- raw_ff %>% 
  select(pitcher, all_of(all_outcomes), all_of(features), launch_angle, launch_speed) %>%
  mutate(launch_speed = round(launch_speed)) %>%
  collect()

# Ground balls
clean_ff$bip_events[clean_ff$launch_angle < 10 & clean_ff$launch_speed < 90] <- 0
clean_ff$bip_events[clean_ff$launch_angle < 10 & between(clean_ff$launch_speed, 90, 95)] <- 1
clean_ff$bip_events[clean_ff$launch_angle < 10 & between(clean_ff$launch_speed, 96, 100)] <- 2
clean_ff$bip_events[clean_ff$launch_angle < 10 & between(clean_ff$launch_speed, 101, 105)] <- 3
clean_ff$bip_events[clean_ff$launch_angle < 10 & clean_ff$launch_speed >= 106] <- 4

# Line drives
clean_ff$bip_events[between(clean_ff$launch_angle, 10, 24) & clean_ff$launch_speed < 90] <- 5
clean_ff$bip_events[between(clean_ff$launch_angle, 10, 24) & between(clean_ff$launch_speed, 90, 95)] <- 6
clean_ff$bip_events[between(clean_ff$launch_angle, 10, 24) & between(clean_ff$launch_speed, 96, 100)] <- 7
clean_ff$bip_events[between(clean_ff$launch_angle, 10, 24) & between(clean_ff$launch_speed, 101, 105)] <- 8
clean_ff$bip_events[between(clean_ff$launch_angle, 10, 24) & clean_ff$launch_speed >= 106] <- 9

# Fly balls
clean_ff$bip_events[between(clean_ff$launch_angle, 25, 50) & clean_ff$launch_speed < 90] <- 10
clean_ff$bip_events[between(clean_ff$launch_angle, 25, 50) & between(clean_ff$launch_speed, 90, 95)] <- 11
clean_ff$bip_events[between(clean_ff$launch_angle, 25, 50) & between(clean_ff$launch_speed, 96, 100)] <- 12
clean_ff$bip_events[between(clean_ff$launch_angle, 25, 50) & between(clean_ff$launch_speed, 101, 105)] <- 13
clean_ff$bip_events[between(clean_ff$launch_angle, 25, 50) & clean_ff$launch_speed >= 106] <- 14

# Pop ups
clean_ff$bip_events[clean_ff$launch_angle > 50] <- 15

clean_ff <- clean_ff %>% select(-launch_angle, -launch_speed) %>%
  mutate(bip_events = as.numeric(bip_events))


rm(pitchers_batting, batters_pitching)
gc()




#### Eventually create a toggle for whether I want to tune the parameters
# For now, just use cross-validated parameters from previous version of model
max_depth_choice <- 15
subsample_choice <- 1.0
colsample_choice <- 0.6


#### Swing Model ###
source("swing_model.R")
clean_ff$xswing <- predict(ff_swing_model, ff_data_matrix)


#### No swing model ###
source("noswing_model.R")
pred_noswing <- predict(ff_noswing_model, ff_data_matrix)
noswing_prediction <- matrix(pred_noswing, nrow = numberOfClasses, ncol = length(pred_noswing)/numberOfClasses) %>% 
  t() %>% 
  data.frame() %>%
  rename("xCalledstrike" = X1, 
         "xBall" = X2, 
         "xHBP" = X3)

clean_ff_noswings <- cbind(clean_ff_noswings, noswing_prediction)


#### Contact model ###
source("contact_model.R")
clean_ff$xcontact <- predict(ff_contact_model, ff_data_matrix)


#### Foul Ball model ###
source("foulball_model.R")
clean_ff_contactonly$xfoul <- predict(ff_foul_model, ff_data_matrix)


#### Ball in play (BIP) model ###
n_battedball_bins <- 16
ff_bip2023$bip_events <- as.numeric(ff_bip2023$bip_events)
source("bip_model.R")

pred_bip <- predict(ff_bip_model, ff_data_matrix)
bip_prediction <- matrix(pred_bip, nrow = numberOfClasses, ncol = length(pred_bip)/numberOfClasses) %>% 
  t() %>% 
  data.frame() %>%
  mutate(xGB = X1 + X2 + X3 + X4 + X5,
         xLD = X6 + X7 + X8 + X9 + X10,
         xFB = X11 + X12 + X13 + X14 + X15 + X16)

clean_ff_bip <- cbind(clean_ff_bip, bip_prediction)


#######################################################~
#### Combine all predictions
all_predictions1 <- left_join(clean_ff, clean_ff_noswings, by = intersect(names(clean_ff), names(clean_ff_noswings)))
all_predictions2 <- left_join(all_predictions1, clean_ff_contactonly, by = intersect(names(all_predictions1), names(clean_ff_contactonly)))
all_predictions <- left_join(all_predictions2, clean_ff_bip, by = intersect(names(all_predictions2), names(clean_ff_bip)))

rm(all_predictions1, all_predictions2)
gc()


#######################################################~
#### Construct xRV using linear weights
# For now, LWTS taken from here: https://library.fangraphs.com/pitching/pitchingbot-pitch-modeling-primer/
# RV for bip: calculated by battedball_run_values.R and are based on 2023 only (to account for ban on shifts)
lwt_foul <- 0
lwt_ball <- 0.127
lwt_hbp <- 0.57
lwt_strike <- -0.235
lwt_gb <- -0.055
lwt_ld <- 0.277
lwt_fb <- 0.103

lwt_X1  <- -0.148  
lwt_X2  <- -0.0656 
lwt_X3  <-  0.00144
lwt_X4  <-  0.0729 
lwt_X5  <-  0.133  
lwt_X6  <-  0.231  
lwt_X7  <-  0.209  
lwt_X8  <-  0.242  
lwt_X9  <-  0.420  
lwt_X10 <-  0.657  
lwt_X11 <- -0.123  
lwt_X12 <- -0.171  
lwt_X13 <-  0.106  
lwt_X14 <-  0.690  
lwt_X15 <-  1.22   
lwt_X16 <- -0.271
  

all_predictions <- all_predictions %>%
  mutate(xCalledstrike = ifelse(is.na(xCalledstrike), 0, xCalledstrike),
         xBall = ifelse(is.na(xBall), 0, xBall),
         xfoul = ifelse(is.na(xfoul), 0, xfoul),
         xHBP = ifelse(is.na(xHBP), 0, xHBP),
         xcontact = ifelse(is.na(xcontact), 0, xcontact),
         xGB = ifelse(is.na(xGB), 0, xGB),
         xLD = ifelse(is.na(xLD), 0, xLD),
         xFB = ifelse(is.na(xFB), 0, xFB),
         X1 = ifelse(is.na(X1), 0, X1),
         X2 = ifelse(is.na(X2), 0, X2),
         X3 = ifelse(is.na(X3), 0, X3),
         X4 = ifelse(is.na(X4), 0, X4),
         X5 = ifelse(is.na(X5), 0, X5),
         X6 = ifelse(is.na(X6), 0, X6),
         X7 = ifelse(is.na(X7), 0, X7),
         X8 = ifelse(is.na(X8), 0, X8),
         X9 = ifelse(is.na(X9), 0, X9),
         X10 = ifelse(is.na(X10), 0, X10),
         X11 = ifelse(is.na(X11), 0, X11),
         X12 = ifelse(is.na(X12), 0, X12),
         X13 = ifelse(is.na(X13), 0, X13),
         X14 = ifelse(is.na(X14), 0, X14),
         X15 = ifelse(is.na(X15), 0, X15),
         X16 = ifelse(is.na(X16), 0, X16)) %>%
  mutate(xRV_all = xfoul*lwt_foul + xBall*lwt_ball + xHBP*lwt_hbp + xCalledstrike*lwt_strike + (1-xcontact)*lwt_strike + xGB*lwt_gb + xLD*lwt_ld + xFB*lwt_fb,
         # Follow Fangraphs and only use models that predict swing events in order to not just predict zone rate
         xRV_swings = xfoul*lwt_foul + (1-xcontact)*lwt_strike + X1*lwt_X1 + X2*lwt_X2 + X3*lwt_X3 + X4*lwt_X4 + X5*lwt_X5 + X6*lwt_X6 + X7*lwt_X7 + X8*lwt_X8 + X9*lwt_X9 + X10*lwt_X10 + X11*lwt_X11 + X12*lwt_X12 + X13*lwt_X13 + X14*lwt_X14 + X15*lwt_X15 + X6*lwt_X16)

rm(bip_prediction, noswing_prediction)

#######################################################~
### Convert to "plus" scale and re-attach pitcher IDs
pitcher_ids <- tbl(pitching_db, 'statcast') %>%
  select(pitcher, player_name) %>%
  distinct() %>%
  collect()

full_data <- tbl(pitching_db, 'statcast') %>%
  # Select only desired pitch type
  filter(pitch_type == "FF") %>%
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
  # Construct some features
  mutate(throw_r = ifelse(p_throws == "R", 1, 0),
         throw_l = ifelse(p_throws == "L", 1, 0),
         stand_r = ifelse(stand == "R", 1, 0),
         stand_l = ifelse(stand == "L", 1, 0)) %>%
  collect()

full_data <- inner_join(full_data, all_predictions, by = intersect(names(full_data), names(all_predictions)))


pitcher_level <- full_data %>% select(game_year, player_name, pitcher, xRV_swings) %>%
  filter(!is.na(xRV_swings)) %>%
  filter(game_year == 2024) %>%
  filter(player_name != "") %>%
  group_by(pitcher) %>%
  # Count number of pitches
  mutate(n_ff = n()) %>%
  ungroup() %>%
  filter(n_ff >= 100) %>%
  # Winsorize values at 1% and 99% levels
  mutate(xRV_swings = ifelse(xRV_swings >= quantile(xRV_swings, c(0.99)), quantile(xRV_swings, c(0.99)), xRV_swings),
         xRV_swings = ifelse(xRV_swings <= quantile(xRV_swings, c(0.01)), quantile(xRV_swings, c(0.01)), xRV_swings)) %>%
  # Multiply stuff by -1 to re-orient for ordinal ranking where higher == better
  mutate(ff_stuff = -1*xRV_swings) %>%
  mutate(avg_ffstuff_all = mean(ff_stuff),
         ff_stuff_plus = 100*(ff_stuff/avg_ffstuff_all)) %>%
  group_by(pitcher) %>%
  summarize(ff_stuff_plus = round(mean(ff_stuff_plus)),
            n_ff = max(n_ff)) %>%
  ungroup() %>%
  inner_join(pitcher_ids, by = "pitcher") %>%
  arrange(-ff_stuff_plus)

