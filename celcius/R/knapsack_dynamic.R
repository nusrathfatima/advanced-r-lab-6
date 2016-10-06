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
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' @examples knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' @examples knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' @examples knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
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
  
  m <- matrix(0,ncol = (W), nrow = (nrow(x)))
  
  for(i in 2:(nrow(x))){
    for(j in 1:(W )){
      if(x[i,"w"] > j){
        m[i,j] <- m[i-1,j]
      } else {
        m[i,j] <- max(m[i-1,j], m[i-1,j - x[i,"w"]] + x[i,"v"] )
      }
    }
  }
  
  
  res <- list(
    "value" = round(m[(nrow(x) ),(W )], digits = 0),
    "elements" = vector("numeric")
  )
  
  elements <- vector("numeric")
  
  m <- rbind(rep(0, W), m)
  x <- rbind(rep(0, ncol(x)),x)
  
  i <- nrow(m)
  k <- W
  
 
  while(i > 1 && k > 0 ){
    if(m[i,k] != m[(i-1),k]){
      elements <- cbind(elements, (i-1))
      k <- k - x[i,"w"]
      i <- i - 1
    } else {
      i <- i - 1
    }
  }
  
  names(elements) <- NULL
  
  res[["elements"]] <- as.vector(sort(elements))


  return(res)
}