#### No swing model
# Create train and test datasets (randomly select rows from cleaned data)
trainsplit <- 0.75
train_index <- sample(1:nrow(ff_noswing2023), nrow(ff_noswing2023)*trainsplit)

trainDF <- ff_noswing2023[train_index,]
testDF <- ff_noswing2023[-train_index,]


# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -noswingevents)), 
                      label = trainDF$noswingevents)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -noswingevents)), 
                     label = testDF$noswingevents)

test_label <- testDF$noswingevents


## Parameter tuning
numberOfClasses <- length(no_swing_events)
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

# Cross-validation parameters
nround <- 200 # Number of XGBoost rounds
cv.nfold <- 10

# Fit cv.nfold*cv.nround XGB models and save the out of fold (OOF) predictions
cv_noswing_model <- xgb.cv(params = xgb_params,
                       data = dtrain,
                       nrounds = nround,
                       nfold = cv.nfold,
                       prediction = TRUE,
                       verbose = FALSE)

OOF_prediction <- data.frame(cv_noswing_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = trainDF$noswingevents + 1)
head(OOF_prediction)

# Confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")


#### Tune hyperparameters
### Eta
candidates <- tibble(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3))
etasearch <- cvGridSearch(candidates, dtrain = dtrain, cvnfolds = cv.nfold, cvnrounds = nround)

eta_choice <- etasearch$eta[etasearch$test_rmse_mean == min(etasearch$test_rmse_mean)]
rm(etasearch, candidates)
print(paste0("Eta choice: ", eta_choice))

### Maximum Depth
candidates <- expand.grid(max_depth = seq(15, 29, by = 2),
                          eta = eta_choice)
# depth_search <- cvGridSearch(candidates, dtrain = dtrain, cvnfolds = cv.nfold, cvnrounds = nround)
# 
# depth_choice <- depth_search$max_depth[depth_search$test_rmse_mean == min(depth_search$test_rmse_mean)]
# rm(depth_search, candidates)
# 
# print(paste0("Max depth choice: ", depth_choice))
depth_choice <- 15

# Train model
ff_noswing_model <- xgb.train(data = dtrain, 
                              verbose = 0,
                              watchlist = list(train = dtrain, test = dtest),
                              nrounds = nround,
                              early_stopping_rounds = 50,
                              eta = eta_choice,
                              max_depth = depth_choice,
                              #subsample = subsample_choice,
                              #colsample_bytree = colsample_choice,
                              params = xgb_params)


ff_noswing_model$evaluation_log %>%
  pivot_longer(cols = c(train_mlogloss, test_mlogloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()


#### Run the model on the full dataset
clean_ff_noswings <- clean_ff %>% filter(!is.na(noswingevents))
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff_noswings, -c(all_of(all_outcomes), xswing, pitcher))), 
                              label = clean_ff_noswings$noswingevents)

ff_data_labels <- clean_ff_noswings$noswingevents
