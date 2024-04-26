#### Swing Model
# Create train and test datasets (randomly select rows from cleaned data)
trainDF <- ff_swing2022
testDF <- ff_swing2023

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
# ### Final check using the testing dataset 
ff_swing_model <- xgb.train(data = dtrain, verbose = 0,
                            watchlist = list(train = dtrain, test = dtest),
                            nrounds = 10000,
                            early_stopping_rounds = 50,
                            max_depth = max_depth_choice,
                            max_leaves = max_leaves_choice,
                            subsample = subsample_choice,
                            colsample_bytree = colsample_choice,
                            eta = eta_choice,
                            objective = "binary:logistic")

ff_swing_model$evaluation_log %>%
  pivot_longer(cols = c(train_logloss, test_logloss), names_to = "LogLoss") %>%
  ggplot(aes(x = iter, y = value, color = LogLoss)) + geom_line()

#### Run the model on the full dataset 
ff_data_matrix <- xgb.DMatrix(data = as.matrix(select(clean_ff, -c(all_of(all_outcomes), pitcher))), 
                              label = clean_ff$swing)
