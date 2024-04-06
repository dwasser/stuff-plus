# Build my own version of Stuff Plus

require(baseballr)
require(tidyverse)
require(dplyr)
require(DBI)
require(RPostgres)
require(Rtools)
require(drat)

#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
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

#### Read-in data
# Only keep swing events (prevent model from focusing on zone rate when no swing occurs)
# Easily filter to include only swing events (removing bunts because they are weird)
swing_events <- c("foul", "hit_into_play", "swinging_strike", "swinging_strike_blocked", "foul_tip")

## Eliminate any swing events that are "weird"
# For some pitches, the description shows a walk even though Statcast says a swing occurred.

rawdata <- tbl(pitching_db, 'statcast') %>%
  filter(description %in% swing_events) %>%
  mutate(walkflag = ifelse(grepl("walks", des), 1, 0)) %>%
  filter(walkflag == 0) %>%
  select(-walkflag) %>%
  collect()


## Remove pitchers batting (except Ohtani and Lorenzen)
# Do this by finding the players who threw at least 100 pitches during 2017-present
pitchers_batting <- rawdata %>% group_by(pitcher, player_name) %>% 
  summarize(pitches = n()) %>%
  ungroup() %>%
  filter(pitches >= 100, pitcher != "660271", pitcher!= "547179")

clean <- rawdata %>% anti_join(pitchers_batting, by = c("batter" = "pitcher"))

## Remove position players pitching
# Do this by finding the pitchers who threw less than 100 pitches during 2017-present
# This will throw out some real pitchers, but they will have a tiny sample size anyway
batters_pitching <- clean %>% group_by(pitcher, player_name) %>% 
  summarize(pitches = n()) %>%
  ungroup() %>%
  filter(pitches < 100)

clean <- clean %>% anti_join(batters_pitching, by = c("pitcher" = "pitcher"))

rm(batters_pitching, pitchers_batting, rawdata)
gc()

## Clean up pitch types
# Remove rare pitch types like knuckleballs
clean <- clean %>% filter(!(pitch_type %in% c("AB", "AS", "EP", "IN", "KN", "NP", "PO", "UN", "SC", "")))

# Drop generic fastballs (pitch_type FA) because there are so few observations
clean <- clean %>% filter(!(pitch_type == "FA"))

# Drop generic forkballs (pitch_type FO) because there are few observations
clean <- clean %>% filter(!(pitch_type == "FO"))

# For now, also drop pitch type == CS because I cannot tell what it is
clean <- clean %>% filter(!(pitch_type == "CS"))

# Group knucklecurve with curve, Group two-seam and sinker
clean <- clean %>% mutate(pitch_type = ifelse(pitch_type == "KC", "CU", pitch_type))
clean <- clean %>% mutate(pitch_type = ifelse(pitch_type == "FT", "SI", pitch_type))

## Drop observations with missing data for input variables
clean <- clean %>% filter(!is.na(pitch_type), # Pitch type
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
                          !is.na(p_throws)) # Pitcher handedness

# Drop some variables that are definitely not needed
clean <- clean %>% select(-fielder_2, -fielder_2_1, -fielder_3, -fielder_4, -fielder_5, 
                          -fielder_6, -fielder_7, -fielder_8, -fielder_9, -spin_rate_deprecated,
                          -break_angle_deprecated, -break_length_deprecated, -hit_location, -hc_x, -hc_y)
gc() 


#### Add linear weights ####
# For now, just use the delta_run_exp variable provided in the Statcast data


# Add a count variable and fix a couple errors in balls/strikes
clean <- clean %>% mutate(count = paste(balls, strikes, sep = "-")) %>%
  mutate(count = ifelse(count == "4-2", "3-2", count),
         count = ifelse(count == "4-1", "3-1", count),
         count = ifelse(count == "1-3", "1-2", count))

clean %>% group_by(count) %>% summarize(mean(delta_run_exp, na.rm = TRUE))

# Select only the numeric vectors required for model
clean <- clean %>% 
  select(pitch_type, release_speed, release_spin_rate,
         plate_x, plate_z, pfx_x, pfx_z, release_extension,
         balls, strikes, stand, p_throws, pitcher, delta_run_exp, 
         balls, strikes, spin_axis) %>%
  mutate(p_throws = ifelse(p_throws == "R", 1, 2),
         stand = ifelse(stand == "R", 1, 2))

# Re-code the events variable: do this later when calculating linear weights
#clean <- clean %>% mutate()



#### Aim: predict the expected run difference between before and after pitch is thrown
# Expected runs for each count: https://en.wikipedia.org/wiki/Pitch_quantification
# Expected runs for each event: https://library.fangraphs.com/principles/linear-weights/

# For now, just use off the shelf numbers
exp_runs_count <- data.frame(matrix(ncol = 2, nrow = 12))
colnames(exp_runs_count) <- c('count', 'exp_runs_count')

exp_runs_event <- data.frame(matrix(ncol = 2, nrow = 6))
colnames(exp_runs_count) <- c('events', 'exp_runs_count')

# For now, use "delta_run_exp" as outcome variable 

## Create training data size
set.seed(62704) 

# Start with just four-seam fastballs
ff_data <- clean %>% 
  filter(pitch_type == "FF") %>%
  filter(!is.na(delta_run_exp)) %>%
  select(-pitch_type, -plate_x, -plate_z, -spin_axis, -release_extension)

trainSize <- round(0.80*nrow(ff_data))

# Create train and test datasets (randomly select rows from cleaned data)
trainIndex <- sample(nrow(ff_data), trainSize)
trainDF <- ff_data %>% dplyr::slice(trainIndex) %>% select(-pitcher)
testDF <- ff_data %>% dplyr::slice(-trainIndex) %>% select(-pitcher)

# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -delta_run_exp)), label = trainDF$delta_run_exp)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -delta_run_exp)), label = testDF$delta_run_exp)

# Tune parameters
# Start by choosing eta from a list of candidate values
paramDF <- tibble(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3))

# Next convert data frame to a list of lists
paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)

# Now write a loop to perform a cross validation using each value of paramList
bestResults <- tibble() # Collect best results here

pb <- txtProgressBar(style = 3) 
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]], 
                 data = dtrain, 
                 nrounds = 200, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = TRUE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() # Free unused memory after each loop iteration
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb) # done with the progress bar

## View results
etasearch <- bind_cols(paramDF, bestResults)
View(etasearch)

eta_choice <- 0.2

# Now find optimal depth and leaves at same time
paramDF <- expand.grid(
  max_depth = seq(15, 29, by = 2),
  max_leaves = c(63, 127, 255, 511, 1023, 2047, 4095),
  eta = eta_choice)

paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
bestResults <- tibble()


pb <- txtProgressBar(style = 3)
for(i in seq(length(paramList))) {
  rwCV <- xgb.cv(params = paramList[[i]],
                 data = dtrain, 
                 nrounds = 200, 
                 nfold = 10,
                 early_stopping_rounds = 10,
                 verbose = FALSE)
  bestResults <- bestResults %>% 
    bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
  gc() 
  setTxtProgressBar(pb, i/length(paramList))
}
close(pb)

depth_leaves <- bind_cols(paramDF, bestResults)
View(depth_leaves) 

max_depth_choice <- 15
max_leaves_choice <- 63

GridSearch <- function(paramDF, dtrain) {
  paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
  bestResults <- tibble()
  pb <- txtProgressBar(style = 3)
  for(i in seq(length(paramList))) {
    rwCV <- xgb.cv(params = paramList[[i]],
                   data = dtrain, 
                   nrounds = 200, 
                   nfold = 10,
                   early_stopping_rounds = 10,
                   verbose = FALSE)
    bestResults <- bestResults %>% 
      bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
    gc() 
    setTxtProgressBar(pb, i/length(paramList))
  }
  close(pb)
  return(bind_cols(paramDF, bestResults) %>% arrange(test_rmse_mean))
}

gc()

paramDF <- expand.grid(
  subsample = seq(0.6, 1, by = 0.1),
  colsample_bytree = seq(0.6, 1, by = 0.1),
  max_depth = max_depth_choice,
  max_leaves = max_leaves_choice,
  eta = eta_choice)

randsubsets <- GridSearch(paramDF, dtrain)

subsample_choice <- 1.0
colsample_choice <- 0.6

### Final check using the testing dataset
ff_model <- xgb.train(data = dtrain, verbose = 0,
                      watchlist = list(train = dtrain, test = dtest),
                      nrounds = 10000,
                      early_stopping_rounds = 50,
                      max_depth = max_depth_choice,
                      max_leaves = max_leaves_choice,
                      subsample = subsample_choice,
                      colsample_bytree = colsample_choice,
                      eta = eta_choice)

ff_model$evaluation_log %>% 
  pivot_longer(cols = c(train_rmse, test_rmse), names_to = "RMSE") %>% 
  ggplot(aes(x = iter, y = value, color = RMSE)) + geom_line()


#### Run the model on the full dataset
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(ff_data, -c(delta_run_exp, pitcher))), label = ff_data$delta_run_exp)

# Need to confirm that this is the correct way to make the prediction
ff_data$ff_stuff <- predict(ff_model, ff_data_matrix)


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

