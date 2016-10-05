set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
)


brute_force_knapsack <- function(x,W){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  
  keep <- matrix(0, nrow = nrow(x), ncol = nrow(x))
  values <- matrix(0, nrow = nrow(x), ncol = nrow(x))
  best_value <- 0
  res <- list(
    "value" = vector("numeric"),
    "elements" = vector("numeric")
  )
  
  for(rows in 1:nrow(x)){
    for(cols in 1:nrow(x)){
      if(sum(x[rows,"v"],x[cols,"v"]) > best_value & sum(x[rows,"w"],x[cols,"w"]) <= W){
        if(rows == cols){
          next
        } else {
          best_value <- sum(x[rows,"v"],x[cols,"v"])
          res[["elements"]] <- sort(c(rows,cols))
          #keep[rows,cols] <- 1
          #values[rows,cols] <- sum(x[rows,"v"],x[cols,"v"])
        }
      } else {
        next
      }
    }
    print(paste((rows/nrow(x))*100, "%"))
  }
  
  res[["value"]] <- round(best_value,digits = 0)
  
  return(res)
}

intToBits(knapsack_objects$v)

packBits(intToBits(knapsack_objects$v[1]), type = "integer")

# Testing
debugonce(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack2(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:1000,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

################# 
# Bruteforce 2

brute_force_knapsack2 <- function(x,W){
  require(combinat, quietly = TRUE)
  
  comb <- combinat::combn(rownames(x), m = 2, simplify = FALSE, fun = as.numeric)
  
  best_value <- 0

  
  best_comb <- function(orig_data = data, comb = comb, W = W){
    res <- list(
      "value" = vector("numeric"),
      "elements" = vector("numeric")
    )
    
    if(sum(orig_data[comb,"v"]) > best_value & sum(orig_data[comb,"v"]) <=W){
      assign("best_value",sum(orig_data[comb,"v"]), envir = as.environment(parent.frame()))
      res[["value"]] <- sum(orig_data[comb,"v"])
      res[["elements"]] <- comb
    } else {
      next
    }
    
    return(res)
  }
  
  res <- lapply(comb, best_comb,orig_data = x, comb = comb, W = W)
  
  return(res)
}



######################################
## Dynamic

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


knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


######################################
## Greedy knapsack

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

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

## Help files
intToBits(matrix(rep(c(0,1),9), ncol = 3))
intToBits()

packBits(intToBits(matrix(rep(c(0,1),9), ncol = 3)), type = "integer")


rownames(knapsack_objects)

tail(combn2(rownames(knapsack_objects), 2))

combinat::combn2()
