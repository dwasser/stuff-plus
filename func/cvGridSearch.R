# Function to implement parameter grid search for xgboost cross validation (cv)
cvGridSearch <- function(paramDF, dtrain) {
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