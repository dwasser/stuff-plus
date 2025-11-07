#### Ball in play model
# Create train and test datasets (randomly select rows from cleaned data)
trainsplit <- 0.75
train_index <- sample(1:nrow(ff_bip2023), nrow(ff_bip2023)*trainsplit)

trainDF <- ff_bip2023[train_index,]
testDF <- ff_bip2023[-train_index,]


# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -bip_events)), 
                      label = trainDF$bip_events)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -bip_events)), 
                     label = testDF$bip_events)

## Parameter tuning
numberOfClasses <- n_battedball_bins
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)

# Cross-validation parameters
nround <- 200 # Number of XGBoost rounds
cv.nfold <- 10

# Fit cv.nfold*cv.nround XGB models and save the out of fold (OOF) predictions
cv_bip_model <- xgb.cv(params = xgb_params,
                       data = dtrain,
                       nrounds = nround,
                       nfold = cv.nfold,
                       prediction = TRUE,
                       verbose = FALSE)

OOF_prediction <- data.frame(cv_bip_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = trainDF$bip_events)
head(OOF_prediction)

# Confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label + 1),
                mode = "everything")


#### Tune hyperparameters
### Eta
# candidates <- tibble(eta = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3))
# etasearch <- cvGridSearch(candidates, dtrain = dtrain, cvnfolds = cv.nfold, cvnrounds = nround)
# 
# eta_choice <- etasearch$eta[etasearch$test_rmse_mean == min(etasearch$test_rmse_mean)]
# rm(etasearch, candidates)
# print(paste0("Eta choice: ", eta_choice))
eta_choice <- 0.05

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


### Train model
ff_bip_model <- xgb.train(data = dtrain, 
                          verbose = 0,
                          watchlist = list(train = dtrain, test = dtest),
                          nrounds = nround,
                          early_stopping_rounds = 50,
                          eta = eta_choice,
                          max_depth = depth_choice,
                          #subsample = subsample_choice,
                          #colsample_bytree = colsample_choice,
                          params = xgb_params)

# Predict hold-out test set
test_pred <- predict(ff_bip_model, newdata = dtest)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = testDF$bip_events,
         max_prob = max.col(., "last"))

# Confusion matrix of test set
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label + 1),
                mode = "everything")

## Variable importance
# Compute feature importance matrix
names <-  colnames(ff_bip2023[,-1])
importance_matrix = xgb.importance(feature_names = names, model = ff_bip_model)
head(importance_matrix)

# Plot
#gp = xgb.ggplot.importance(importance_matrix) + ggtitle("ff_bip_model")
#print(gp) 


ff_bip_model$evaluation_log %>%
  pivot_longer(cols = c(train_mlogloss, test_mlogloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()



#### Run the model on the full dataset 
clean_ff_bip <- clean_ff %>% filter(!is.na(bip_events))
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff_bip,  -c(all_of(all_outcomes), xswing, xcontact, pitcher))), 
                              label = clean_ff_bip$bip_events)

ff_data_labels <- clean_ff_bip$bip_events