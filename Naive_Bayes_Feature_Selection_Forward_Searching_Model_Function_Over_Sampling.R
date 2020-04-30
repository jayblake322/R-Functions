
nBWrapper <- function(df, laplace){
  # This function applies feature selection to find the best predictors of a dataframe applying the
  # Note: ASSUMES DEPENDENT VARIABLE IS THE FIRST COLUMN OF DF 
  ## Naive Bayes Model
  
  set.seed(0)
  # Initialise variables and vectors to record results and run the model
  
  no_obs <- dim(df)[1] 
  no_preds <- dim(df)[2] - 1
  y <- 2 # index counter for best predictors vector
  best_preds <- 1 # vector index of best predictors |  accounts for dependent variable 
  pred_names <- names(df[, -1]) # predictor variable names vector
  min_error_vec <- c() # records error rate of model with new predictor
  best_preds_vec <- c() # records names of best predictors 
  n_its <- no_preds -1 # sets outer loop iterations at one less the number of predictors
  evaluation <- list() # initialise list to store algorithmn results
  
  #### OUTER LOOP 
  for(iterate in 1:n_its){ 
    
    x <- 0 # initialise/reset index counter for middle loop interations
    error <- 0 # initialise error variable
    errors <- c() # initialise vector to record all errors from middle loop iterations
    
    # MIDDLE LOOP START| Controls dataframes
    for (i in df[, -best_preds]){ # iterates for the number of predictors not in best model 
      
      x <- x + 1 # record number of iterations 
      remain_pred_names <- names(df[, -best_preds]) # remaining predictor names
      df_train <- data.frame(dependent = df[, 1]) # initialise dataframe with the dependent variable
      df_train[, 1:length(best_preds)] <- df[, best_preds] # add best predictors including dependent
      df_train$add_pred <- i # add the new predictor to try
      names(df_train)[names(df_train) == "add_pred"] <- remain_pred_names[x] # give original name 
      
      for(z in 1:10){ # INNER LOOP | Runs Model
        
        test_index <- sample(no_obs, size = as.integer(no_obs * 0.3), 
                             prob = ifelse(df$Severity == "Serious/Fatal", 0.8, 0.2), replace = TRUE) # test index
        training_index = - test_index # training index
        
        NaiveBayesModel <- naive_bayes(dependent ~ ., data = df_train[training_index, ], laplace = laplace) # train model
        Predictions<- predict(NaiveBayesModel, newdata = df[test_index, 2:no_preds]) # apply model
        
        confusion_table <- table(Predictions, df[test_index, "V1"]) # create contingency table
        accuracy <- sum(diag(confusion_table))/sum(confusion_table) # calculate accuracy
        error <- error + (1 - accuracy) # sum errors
        
      } 
      
      # MIDDLE LOOP END| Evaluates Model Best Predictor
      errors[x] <- error/10 # average errors
      error <- 0 # reset errors for next iteration
      
    }
    
    # OUTER LOOP END | records best predictor names and indices 
    low_error_ind <- which.min(errors) # indexes the best predictor from error vector
    best_pred_name <- pred_names[low_error_ind] # get best variable name
    best_preds[y] <- which(colnames(df) == best_pred_name) # Adds best predictor to index vector
    pred_names <- pred_names[-low_error_ind] # remove best predictor from name vector
    min_error_vec[y - 1] <- min(errors) # record error rate for best model 
    best_preds_vec[y - 1] <- best_pred_name # add new best predictor name to vector
    y <- y + 1 
    
    print(best_preds_vec) # prints best predictors each time one is added
    
    
  }
  
  # Store and return results
  evaluation[[1]] <- best_preds_vec
  evaluation[[2]] <- min_error_vec
  evaluation[[3]] <- pred_names
  
  return(evaluation)
}



