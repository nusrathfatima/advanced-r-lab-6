

devtools::install_github("hadley/lineprof")
library(lineprof)


# Example data

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )


# Brute_force -----------------------------

source(paste(getwd(),"celcius","R","brute_force_knapsack.R", sep = "/"))

brute_normal <- lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500))
shine(brute_normal)

brute_par <- lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500, parallel = TRUE))
shine(brute_par)


# knapsack_dynamic --------------------------

source(paste(getwd(),"celcius","R","knapsack_dynamic.R", sep = "/"))

dynamic_prof <- lineprof(knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500))
shine(dynamic_prof)

# greedy_knapsack ----------------------------

source(paste(getwd(),"celcius","R","greedy_knapsack.R", sep = "/"))
greedy_prof <- lineprof(greedy_knapsack(x = knapsack_objects[1:1500,], W = 3500))
shine(greedy_prof)
