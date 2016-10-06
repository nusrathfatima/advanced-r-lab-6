#' Brute_force_knapsack
#'
#' @description Brute-force search (i.e. going through all possible alternatives and return the maximum value
#' found) knapsack is an algorythm approach of complexity O(2n) since all possible combinations 2n needs to be evaluated.
#'
#' @param x A data.frame cx with two variables v and w
#' @param W Weight restriction for x
#' @return it returns the maximum knapsack value and which elements. 
#' 
#' @example brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' @example brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' @example brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' @example brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' 
#' 
#' 
#' 
#' @seealso see also similar functions:
#' \code{\link{greedy_knapsack}}
#' \code{\link{knapsack_dynamic}}
#'  
#' 
#'
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem}
#'
#' @export
data(knapsack_objects)

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
    "value" =  round(values[[which.max(values)]],digits = 0),
    "elements" = comb[[which.max(values)]]
  ))
}