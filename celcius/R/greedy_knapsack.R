


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
    "value" = value,
    "elements" = elements
  )
  
  return(res)
  
}