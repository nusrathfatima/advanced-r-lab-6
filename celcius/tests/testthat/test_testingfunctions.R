library(celcius)
library(testthat)
install.packages("combinat", repos = "http://cran.rstudio.com/")
install.packages("microbenchmark", repos = "http://cran.rstudio.com/")

library(microbenchmark)
library(combinat)

######################################DATA FOR TESTING###############################

context("knapsack")

set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

#####################################Brute_force_knapsack###############################

test_that("Brute_force_knapsack", {
  
  expect_that(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500),
              equals(list(value = 121186, elements = c(6,8))))
  
  expect_that(brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000),
              equals(list(value = 16922, elements = c(8,9))))
  
  expect_that(brute_force_knapsack(x = c(1:3), W = 2000),
              throws_error("Data must be a data.frame"))
  expect_that(brute_force_knapsack(x = c(1:3), W = 2000:2001),
              throws_error("W must be of length 1"))
  
  expect_that(colnames(knapsack_objects[1:8,])[1],
              equals("w"))
  expect_that(colnames(knapsack_objects[1:8,])[2],
              equals("v"))
}
 
#####################################greedy_knapsack###############################

test_that("greedy_knapsack", {
  
  expect_that(greedy_knapsack(x = knapsack_objects[1:8,], W = 3500),
              equals(list(value = 121186, elements = c(6,8))))
  
  expect_that(greedy_knapsack(x = knapsack_objects[1:12,], W = 2000),
              equals(list(value = 16922, elements = c(8,9))))
  
  expect_that(greedy_knapsack(x = c(1:3), W = 2000),
              throws_error("Data must be a data.frame"))
  expect_that(greedy_knapsack(x = c(1:3), W = 2000:2001),
              throws_error("W must be of length 1"))
  
  expect_that(colnames(knapsack_objects2[1:8,])[1],
              equals("w"))
  expect_that(colnames(knapsack_objects2[1:8,])[2],
              equals("v"))
}



#####################################knapsack_dynamic###############################

test_that("knapsack_dynamic", {
  
  expect_that(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500),
              equals(list(value = 121186, elements = c(6,8))))
  
  expect_that(knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000),
              equals(list(value = 16922, elements = c(8,9))))
  
  expect_that(knapsack_dynamic(x = c(1:3), W = 2000),
              throws_error("Data must be a data.frame"))
  expect_that(knapsack_dynamic(x = c(1:3), W = 2000:2001),
              throws_error("W must be of length 1"))
  
  expect_that(colnames(knapsack_objects2[1:8,])[1],
              equals("w"))
  expect_that(colnames(knapsack_objects2[1:8,])[2],
              equals("v"))
}

