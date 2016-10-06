#' greedy_knapsack
#'
#' @description greedy_knapsack search is to use the a heuristic or approximation for the problem. This algorithm will not give
#' an exact result (but it can be shown that it will return at least 50% of the true maximum value), but it
#' will reduce the computational complexity considerably (actually to O(n log n) due to the sorting part of
#' the algorithm)
#'
#' @param x A data.frame cx with two variables v and w
#' @param W Weight restriction for x
#' @return it returns the maximum knapsack value and which elements. 
#' 
#' 
#' @example greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' 
#' 
#' @seealso see also similar functions:
#' \code{\link{brute_force_knapsack}}
#' \code{\link{knapsack_dynamic}}
#'  
#' 
#'
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @export


greedy_knapsack <- function(x, W){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  
  #Creating the a fractional variable and sorting the
  # row according to the new variable
  x$frac <- x$v/x$w
  x <- x[order(x$frac, decreasing = TRUE), ]
  
  #Creating two vectors for holding the result 
  value <- vector("numeric")
  elements <- vector("numeric")
  
  #Initilazing values
  weight <- 0
  value <- 0
  
  
  i <- 1
  while(weight + x[i,"w"] < W){
    value <- value + x[i,"v"] 
    weight <- weight + x[i,"w"]
    elements[i] <- rownames(x[i,])
    i <- i + 1
  }
  
  res <- list(
    "value" = round(value,digits = 0),
    "elements" = elements
  )
  
  return(res)
  
}