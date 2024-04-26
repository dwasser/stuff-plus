#### Ball in play model
# Create train and test datasets (randomly select rows from cleaned data)
trainDF <- ff_bip2022
testDF <- ff_bip2023

# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -bip_events)), 
                      label = trainDF$bip_events)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -bip_events)), 
                     label = testDF$bip_events)

## Skip parameter tuning for now

# Train model
numberOfClasses <- 3
ff_bip_model <- xgb.train(data = dtrain, verbose = 0,
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


ff_bip_model$evaluation_log %>%
  pivot_longer(cols = c(train_mlogloss, test_mlogloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()

## HERE, NEED TO EVALUATE BASED ON CONFUSION MATRIX AND ALSO MAKE SPECIFIC PREDICTED CLASS
## SEE TUTORIAL

#### Run the model on the full dataset 
clean_ff_bip <- clean_ff %>% filter(!is.na(bip_events))
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff_bip,  -c(all_of(all_outcomes), xswing, xcontact, pitcher))), 
                              label = clean_ff_bip$bip_events)

ff_data_labels <- clean_ff_bip$bip_events