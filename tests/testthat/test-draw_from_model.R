library(dplyr)
context("Draw results from model")

N_a <- 125  # Number of nodes of the a type
N_b <- 125  # Number of nodes of the b type
K_a <- 4    # How many blocks of a type nodes are there
K_b <- 3    # How many blocks of the b type nodes are there

a_node_groups <- assign_group_membership(N = N_a, K = K_a)
b_node_groups <- assign_group_membership(N = N_b, K = K_b)
Lambda <- generate_random_lambda(K_a = K_a, K_b = K_b) %>% arrange(a,b)

all_node_pairs <- draw_from_model(
  b_a    = a_node_groups,
  b_b    = b_node_groups,
  Lambda = Lambda
)

test_that("Group pairs have approximately the correct average number of connections", {
  drawn_group_pair_avgs <- all_node_pairs %>%
    group_by(a_group, b_group) %>%
    summarise(average_num_edges = mean(num_edges)) %>%
    arrange(a_group, b_group) %>% ungroup()

  expect_equal(
    drawn_group_pair_avgs$average_num_edges,
    Lambda$avg_num_cons,
    tolerance = 0.05
  )
})

test_that("Custom node names are respected", {
  model_draw <- draw_from_model(
    b_a    = a_node_groups,
    b_b    = b_node_groups,
    Lambda = Lambda,
    a_name = "my_a_node",
    b_name = "my_b_node"
  )

  expect_equal(
    colnames(model_draw)[1], "my_a_node"
  )

  expect_equal(
    colnames(model_draw)[2], "my_b_node"
  )
})

test_that("When in binary_connection mode no lambda values over 1 are accepted.", {
  big_lambda_generator <- function(n){
    runif(n, min = 1, max = 10)
  }

  bad_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b, random_generator = big_lambda_generator)

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
