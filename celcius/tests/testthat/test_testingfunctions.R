library(celcius)
library(testthat)
install.packages("combinat", repos = "http://cran.rstudio.com/")
install.packages("microbenchmark", repos = "http://cran.rstudio.com/")

library(microbenchmark)
library(combinat)
knapsack_objects <- knapsack_data_gen()

######################################DATA FOR TESTING###############################

context("knapsack")



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
})
 
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
  
  expect_that(colnames(knapsack_objects[1:8,])[1],
              equals("w"))
  expect_that(colnames(knapsack_objects[1:8,])[2],
              equals("v"))
}
)


#####################################knapsack_dynamic###############################

test_that("knapsack_dynamic", {
  
  expect_that(knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500),
              equals(list(value= 192647, 
                          elements= c(92, 574, 472, 80, 110, 537, 332, 117, 37, 
                                      776, 577, 288, 234, 255, 500, 794, 55,
                                      290, 436, 346, 282, 764, 599, 303, 345, 
                                      300, 243, 43, 747, 35, 77, 229, 719, 564)
                          )
                     )
              )
  
  expect_that(knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000),
              equals(list(value = 16922, elements = c(8,9))))
  
  expect_that(knapsack_dynamic(x = c(1:3), W = 2000),
              throws_error("Data must be a data.frame"))
  expect_that(knapsack_dynamic(x = c(1:3), W = 2000:2001),
              throws_error("W must be of length 1"))
  
  expect_that(colnames(knapsack_objects[1:8,])[1],
              equals("w"))
  expect_that(colnames(knapsack_objects[1:8,])[2],
              equals("v"))
})


