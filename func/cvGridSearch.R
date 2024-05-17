# Function to implement parameter grid search for xgboost cross validation (cv)
cvGridSearch <- function(paramDF, dtrain, cvnfolds, cvnrounds) {
  paramList <- lapply(split(paramDF, 1:nrow(paramDF)), as.list)
  bestResults <- tibble()
  for(i in seq(length(paramList))) {
    rwCV <- xgb.cv(params = paramList[[i]],
                   data = dtrain, 
                   nrounds = cvnrounds, 
                   nfold = cvnfolds,
                   early_stopping_rounds = 10,
                   verbose = FALSE)
    bestResults <- bestResults %>% 
      bind_rows(rwCV$evaluation_log[rwCV$best_iteration])
    gc() 
  }
  rm(rwCV)
  return(bind_cols(paramDF, bestResults) %>% arrange(test_rmse_mean))
}