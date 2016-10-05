


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