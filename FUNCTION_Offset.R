
offset_column <- function(df, col, offset){
# this function moves every value in a df column to the right index by the number of spaces indicated by offset. Initial NAs. 
  
  offset_vec <- vector()
  n <- nrow(df)
  loop_start <- offset + 1 
  
  for (x in loop_start:n){
    
    y = x - offset
    offset_vec[x] = df[y, col]
    
  } 
  
  return(offset_vec)
  
}

