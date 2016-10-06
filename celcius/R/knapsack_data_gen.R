#' @title Knapsack data generator
#' 
#' @description A data generator for creating knapsack elements
#' 
#' @param n number of observation to be created
#' @param seed The random generator seed
#' 
#' @return A data frame with two columns, 'v' for value and 'w' for weight.  
#' 
#' 
#' 
#' @export
knapsack_data_gen <- function(n = 2000, seed = 42){
  set.seed(seed)
  data <-   data.frame(w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
  return(data)
}