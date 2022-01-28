# This script contains all support functions for the simulation.
#


gini = function (x) { # adapted from ineq::Gini 
  if (any(is.na(x))) {
    x <- na.omit(x)
    warning("NAs are ignored in the calculation of the Gini coefficient.")
  }
  x <- x |> as.numeric() |> sort()
  if (length(x) <= 1) return(NA) ###
  if (all(x == 0)) return(0) ###
  n <- length(x)
  G <- sum(x * 1L:n)
  G <- 2 * G / sum(x) - (n + 1L)
  return(G / (n - 1L))
}


truncate <- function(x, min = 0, max = 1){
  if (length(x) > 1) {return(sapply(x, truncate, min = min, max = max))}
  ifelse(
    x < min,
    return(min),
    ifelse(
      x > max,
      return(max),
      return(x)
    )
  )
}