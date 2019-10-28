library(dplyr)

N_a <- 125  # Number of nodes of the a type
N_b <- 125  # Number of nodes of the b type
K_a <- 4    # How many blocks of a type nodes are there
K_b <- 3    # How many blocks of the b type nodes are there

a_node_groups <- assign_group_membership(N = N_a, K = K_a)
b_node_groups <- assign_group_membership(N = N_b, K = K_b)
Lambda <- generate_random_lambda(K_a = K_a, K_b = K_b) %>% arrange(a,b)

test_that("Group pairs have approximately the correct average number of connections", {
  drawn_group_pair_avgs <-  draw_from_model(
      b_a    = a_node_groups,
      b_b    = b_node_groups,
      Lambda = Lambda
    ) %>%
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


test_that("Binary connection mode only ever returns 0 and 1 for connections values", {

  beta_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b, random_generator = function(n){rbeta(n, shape1 = 2, shape2 = 2)})

  drawn_data <- draw_from_model(
    b_a    = a_node_groups,
    b_b    = b_node_groups,
    Lambda = beta_lambda,
    binary_connections = TRUE
  )

  expect_true(
    all(drawn_data$num_edges %in% c(0,1))
  )
})
