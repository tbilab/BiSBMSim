context("Generating random Lambda matrices")

test_that("Returns a tibble of the right size", {
  K_a <- 5
  K_b <- 4

  random_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b)

  expect_equal(dim(random_lambda), c(K_a*K_b, 3))
})

test_that("Alternative generating functions work", {
  K_a <- 10
  K_b <- 15
  poisson_avg <- 15
  poisson_generating_function <- function(n) rpois(n, lambda = poisson_avg)

  random_lambda <- generate_random_lambda(K_a = K_a, K_b = K_b, random_generator = poisson_generating_function)

  mean(random_lambda$avg_num_cons) %>%
    expect_equal(poisson_avg, tolerance = 3) # Central limit theorem don't let me down!
})

