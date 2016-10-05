set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


######################################
## Dynamic

knapsack_dynamic <- function(x, W){
  #x<-x[order(x$w),]##ordering x by weight
  m <- matrix(0,ncol = W, nrow = nrow(x))
  b<-list()
  for(i in 2:(nrow(x))){
    for(j in 1:W){
      if(x[i,"w"] > j){
        m[i,j] <- m[i-1,j]
      } else {
        m[i,j] <- max(m[i-1,j], m[i-1,j - x[i,"w"]] + x[i,"v"] )
        b <- which(m==max(m[i-1,j], m[i-1,j - x[i,"w"]] + x[i,"v"],arr.ind = TRUE ),
      }
    }
  }
  
  
  res <- list(
    "value" = m[nrow(x),W]
    ,"elements"= b
  )
  return(res)
}
#basic<- data.frame(w=c(1,3,4,5), v=c(1,4,5,7))
#knapsack_dynamic(x = basic , W = 7)

knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


vector = c( 100, 200, 300 )
which(vector == max(vector)

