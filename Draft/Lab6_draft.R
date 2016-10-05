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
  }
  
  res[["value"]] <- round(best_value,digits = 0)
  
  return(res)
}

intToBits(knapsack_objects$v)

packBits(intToBits(knapsack_objects$v[1]), type = "integer")

# Testing
debugonce(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500))
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:1000,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

################# 
# Bruteforce 2

brute_force_knapsack2 <- function(x,W, parallel = FALSE){
  require(combinat, quietly = TRUE)
  require(parallel, quietly = TRUE)
  

  
  if(parallel){
    if(Sys.info()["sysname"] == "Windows"){
      stop("The parallel function can not be used in a Windows system")
    } else {
     
      cores = parallel::detectCores()
      
      comb <- unlist(parallel::mclapply(1:nrow(x), 
                            function(i){
                              combinat::combn(rownames(x), 
                                              m = i, 
                                              simplify = FALSE, 
                                              fun = as.numeric)},
                            mc.cores = cores),
                     recursive = FALSE)
      
      values <- parallel::mclapply(comb, 
                                   function(comb){ifelse(sum(x[comb,"w"]) <=W,
                                                         sum(x[comb,"v"]),
                                                         0)},
                                   mc.cores = cores)
       
    }
  } else {
    
    comb <- unlist(lapply(1:nrow(x), 
                          function(i){
                            combinat::combn(rownames(x), 
                                            m = i, 
                                            simplify = FALSE, 
                                            fun = as.numeric)}),
                   recursive = FALSE)
    
    values <- lapply(comb, function(comb){ifelse(sum(x[comb,"w"]) <=W,
                                                 sum(x[comb,"v"]),
                                                 0)})
  }
  

  
  return(list(
    "value" =  values[[which.max(values)]],
    "elements" = comb[[which.max(values)]]
    ))
}

library(microbenchmark)

microbenchmark(
  brute_force_knapsack(x = knapsack_objects[1:50,], W = 3500),
  brute_force_knapsack2(x = knapsack_objects[1:10,], W = 3500)
)


microbenchmark(
  "normal" = brute_force_knapsack2(x = knapsack_objects[1:20,], W = 3500),
  "parallel" = brute_force_knapsack2(x = knapsack_objects[1:20,], W = 3500, parallel = TRUE)
)
brute_force_knapsack2(x = knapsack_objects[1:200,], W = 3500, parallel = TRUE)

brute_force_knapsack2(x = knapsack_objects[1:8,], W = 3500)


test <- combinat::combn(rownames(knapsack_objects[1:8,]), m = 2, simplify = FALSE, fun = as.numeric)
unlist(lapply(test, function(comb){ifelse(sum(x[comb,"w"]) <=W,
                                   sum(x[comb,"v"]), 
                                   0)}))


(lapply(combinat::combn(rownames(knapsack_objects[1:8,]), m = 2, simplify = FALSE, fun = as.numeric), as.character))

temp_resi <- data.frame(
  "comb" = combinat::combn(rownames(knapsack_objects[1:8,]), m = 2, simplify = FALSE)
)
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
