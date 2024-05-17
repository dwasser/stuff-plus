#### Contact model
# Create train and test datasets (randomly select rows from cleaned data)
trainsplit <- 0.75
train_index <- sample(1:nrow(ff_contact2023), nrow(ff_contact2023)*trainsplit)

trainDF <- ff_contact2023[train_index,]
testDF <- ff_contact2023[-train_index,]


# Format data for xgboost
dtrain <- xgb.DMatrix(data = as.matrix(select(trainDF, -contact)), 
                      label = trainDF$contact)
dtest <- xgb.DMatrix(data = as.matrix(select(testDF, -contact)), 
                     label = testDF$contact)


## Parameter tuning
xgb_params <- list("objective" = "binary:logistic")

# Cross-validation parameters
nround <- 200 # Number of XGBoost rounds
cv.nfold <- 10


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


# ### Final check using the testing dataset 
ff_contact_model <- xgb.train(data = dtrain, 
                              verbose = 0,
                              watchlist = list(train = dtrain, test = dtest),
                              nrounds = nround,
                              early_stopping_rounds = 50,
                              eta = eta_choice,
                              max_depth = max_depth_choice,
                              #subsample = subsample_choice,
                              #colsample_bytree = colsample_choice,
                              params = xgb_params)

ff_contact_model$evaluation_log %>%
  pivot_longer(cols = c(train_logloss, test_logloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()

#### Run the model on the full dataset 
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff, -c(all_of(all_outcomes), xswing, pitcher))), 
                              label = clean_ff$contact)
