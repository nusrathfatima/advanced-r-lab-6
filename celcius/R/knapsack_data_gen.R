#' @title Knapsack data generator
#' 
#' @description A data generator
#' 
#' @export
knapsack_data_gen <- function(n = 2000, seed = 42){
  set.seed(seed)
  data <-   data.frame(w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
  return(data)
}