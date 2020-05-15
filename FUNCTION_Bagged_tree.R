## Bagged Tree Function

bagging_function <- function(data, r){
  
  MSE <- matrix(-(r-1), r, 1) 
  
  #sample with replacement  
  
  for (i in 1:r){
    
    ss <- sample(nrow(data), 200, replace = TRUE)
    train_sample <- data[ss,]
    test <- data[-ss,]
    M1 <- rpart(train_sample$medv ~., train_sample)
    pre <- predict(M1, test)
    MSE[i] <- mean((test$medv - pre) ^ 2)
  }
  
  return(list("Mean SE"= mean(MSE), "Lower CI" = quantile(MSE, p = 0.05), "Upper CI" = quantile(MSE, p = 0.95)))
  
}
