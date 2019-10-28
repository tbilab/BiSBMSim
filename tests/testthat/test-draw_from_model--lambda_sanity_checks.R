library(dplyr)

N_a <- 125  # Number of nodes of the a type
N_b <- 125  # Number of nodes of the b type
K_a <- 4    # How many blocks of a type nodes are there
K_b <- 3    # How many blocks of the b type nodes are there
a_node_groups <- assign_group_membership(N = N_a, K = K_a)
b_node_groups <- assign_group_membership(N = N_b, K = K_b)


test_that("When in binary_connection mode no lambda values over 1 are accepted.", {
  bad_lambda_generator <- function(n){
    runif(n, min = 1, max = 10)
  }

  bad_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b, random_generator = bad_lambda_generator)

  expect_error(
    draw_from_model(
      b_a    = a_node_groups,
      b_b    = b_node_groups,
      Lambda = bad_lambda,
      binary_connections = TRUE
    ),
    "In binary connection mode you can't have a lambda greater than one. Either adjust your Lambdas or set binary_connection = FALSE.",
    fixed = TRUE
  )
})

test_that("Negative Lambda values are turned away", {
  bad_lambda_generator <- function(n){
    runif(n, min = -2, max = 0)
  }

  bad_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b, random_generator = bad_lambda_generator)

  expect_error(
    draw_from_model(
      b_a    = a_node_groups,
      b_b    = b_node_groups,
      Lambda = bad_lambda,
      binary_connections = TRUE
    ),
    "Lambda contains negative value.",
    fixed = TRUE
  )
})
