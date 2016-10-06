## ------------------------------------------------------------------------
library(celcius)
knapsack_objects <- knapsack_data_gen(n = 2000, seed = 42)
head(knapsack_objects, n = 5)

## ---- warning= FALSE, message= FALSE-------------------------------------
brute_force_knapsack(knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
knapsack_dynamic(knapsack_objects[1:8,], W = 3500)

## ------------------------------------------------------------------------
greedy_knapsack(knapsack_objects[1:8,], W = 3500)

