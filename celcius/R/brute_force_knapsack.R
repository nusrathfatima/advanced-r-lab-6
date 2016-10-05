

brute_force_knapsack <- function(x,W, parallel = FALSE){
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