#### No swing model
# Create train and test datasets (randomly select rows from cleaned data)
trainDF <- ff_noswing2022
testDF <- ff_noswing2023

# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -noswingevents)), 
                      label = trainDF$noswingevents)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -noswingevents)), 
                     label = testDF$noswingevents)

test_label <- testDF$noswingevents

## Skip parameter tuning for now

# Train model
numberOfClasses <- length(no_swing_events)
ff_noswing_model <- xgb.train(data = dtrain, verbose = 0,
                              watchlist = list(train = dtrain, test = dtest),
                              nrounds = 10000,
                              early_stopping_rounds = 50,
                              max_depth = max_depth_choice,
                              max_leaves = max_leaves_choice,
                              subsample = subsample_choice,
                              colsample_bytree = colsample_choice,
                              eta = eta_choice,
                              objective = "multi:softprob",
                              eval_metric = "mlogloss",
                              num_class = numberOfClasses)


ff_noswing_model$evaluation_log %>%
  pivot_longer(cols = c(train_mlogloss, test_mlogloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()

## HERE, NEED TO EVALUATE BASED ON CONFUSION MATRIX 
## SEE TUTORIAL

#### Run the model on the full dataset
clean_ff_noswings <- clean_ff %>% filter(!is.na(noswingevents))
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff_noswings, -c(all_of(all_outcomes), pitcher))), 
                              label = clean_ff_noswings$noswingevents)

ff_data_labels <- clean_ff_noswings$noswingevents
