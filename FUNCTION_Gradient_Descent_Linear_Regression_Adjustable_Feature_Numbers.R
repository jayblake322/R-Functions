linear_gradient_descent <- function(x, y, beginning_coefs, learning_rate, n_epochs, min_step){
  
  coef_storage <- matrix(0, nrow = n_epochs, ncol = length(beginning_coefs))  # coefficient storage matrix
  mse_storage <- matrix(0, nrow = n_epochs, ncol = 1)  # mse storage matrix
  gradient <- matrix(0, nrow = n_epochs, ncol = length(beginning_coefs)) # gradient storage matrix
  coef_storage[1, ] <- beginning_coefs # set first coefficients in storage matrix
  n_features <- length(beginning_coefs) - 1 # calculate the number of features in x
  n_coefs <- length(beginning_coefs)
  
  for(i in 1:(n_epochs - 1)){
    
    # Calculate yhat with current estimated coeffiecients
    # Linear Equation --- b0 + b1*x1 ......+ bn*xn
    # construct yhat in parts to account for different numbers of features
    yhat <- 0
    
    # add the contribution from each feature and its coefficient
    for (e in 1:n_features){
      
      feature_contribution <- coef_storage[i, e + 1] * x[, e] 
      yhat <- yhat + feature_contribution
      
    }
    
    yhat <- yhat + coef_storage[i, 1] # add b0 coefficient
    
    # calculate mse 
    
    mse_storage[i] <- mean((y - yhat) ^ 2)
    
    # calculate gradients for intercept
    
    gradient[i, 1] <- -2 * mean(y - yhat)
    
    # calculate gradients of each feature
    for (z in 1:n_features){
      
      gradient[i, z + 1] <- -2 * mean(x[, z] * (y - yhat))
      
    }
    
    # calculate new estimated coefficients for b0
    coef_storage[i + 1, 1] <- coef_storage[i, 1] - gradient[i, 1] * learning_rate
    
    # calculate new estimated coefficients for features
    for (c in 2:(n_coefs)){
      
      coef_storage[i + 1, c] <- coef_storage[i, c] - gradient[i, c] * learning_rate
      
    }
    
    # check stopping limit of algorithmn (min step size)
    if(all(abs(gradient[i, ]) < min_step)){
      
      i = i - 1
      break;
      
    }
  }
  
  return(list("iteration" = 1:i, "coefs" = coef_storage, "gradient" = gradient, "cost" = mse_storage))
  
}
