strsplits <- function(x, splits, ...){

  # this function splits a string on multiple criteria and returns all results in a vector. 
  for (split in splits)
  {
    x <- unlist(strsplit(x, split, ...))
  }
  return(x[!x == ""]) # Remove empty values
}

# examplex 
 # strsplits(strings, c("-R2", "/", "-1")) multiple criteria.