#' @title Brute force knapsack
#'
#' @description A function that solve the knapsack problem through brute force. Brute-force search 
#' is an algorithm approach of complexity O(2n) since all possible combinations 2n needs to be evaluated.
#'
#' @param x A data.frame cx with two variables v and w
#' @param W The weight capacity of the knapsack
#' @param parallel If true, the function will use multiple cores. Works only on Mac/Linux system.
#' @return The maximum knapsack value and corresponding elements that contributes to the value. 
#' 
#' @examples knapsack_objects <- knapsack_data_gen(n = 2000, seed = 42)
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' @examples brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' @examples brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' @examples brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
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
brute_force_knapsack <- function(x,W, parallel = FALSE){
  require(combinat, quietly = TRUE)
  require(parallel, quietly = TRUE)
  
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  
  if(ncol(x) != 2){
    stop("x should only have 2 columns, a value columns and a weight column")
  }
  
  if(!all(names(x) %in% c("v","w"))){
    stop("Names of columns in x should only be 'v' for value and 'w' for weight")
  }
  
  if(length(W) >1 | !is.numeric(W)){
    stop("W must be numeric with length 1 ")
  }
  
  
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