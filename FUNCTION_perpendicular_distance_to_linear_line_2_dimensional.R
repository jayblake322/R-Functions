# Function for the shortest "perpendicular" distance to a linear equation line

# 2 dimensional
distance_to_line_2m <- function(coords, slope, intercept) {
  
  # coords as concatenated input
  a = c(1, intercept + slope)
  b = c(-intercept/slope, 0)       
  x1 <- a - b 
  x2 <- coords - a
  x3 <- cbind(x1, x2)
  x4 <- sqrt(sum(x1 * x1))
  x5 <- abs(det(x3))
  perp_distance <- x5/x4
  return(perp_distance)
}