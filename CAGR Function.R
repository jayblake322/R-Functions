CAGR <- function(PV, FV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}


