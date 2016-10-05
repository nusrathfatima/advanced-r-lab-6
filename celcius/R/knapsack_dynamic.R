#' knapsack_dynamic
#'
#' @description If the weights are actually discrete values, knapsack_dynamic search is
#'  used to solve the knapsack problem but scaling much better
#' over all possible values of w.
#'
#' @param x A data.frame cx with two variables v and w
#' @param W Weight restriction for x
#' @return it returns the maximum knapsack value and which elements. 
#' 
#' 
#' @example knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @example knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' @example knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' @example knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' 
#' 
#' @seealso see also similar functions:
#' \code{\link{brute_force_knapsack}}
#' \code{\link{greedy_knapsack}}
#'  
#' 
#'
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#'
#' @export



knapsack_dynamic <- function(x, W){
  
  m <- matrix(0,ncol = W, nrow = nrow(x))
  
  for(i in 2:(nrow(x))){
    for(j in 1:W){
      if(x[i,"w"] > j){
        m[i,j] <- m[i-1,j]
      } else {
        m[i,j] <- max(m[i-1,j], m[i-1,j - x[i,"w"]] + x[i,"v"] )
      }
    }
  }
  
  res <- list(
    "value" = m[nrow(x),W]
  )
  return(res)
}