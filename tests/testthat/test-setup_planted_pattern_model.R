library(dplyr)

test_that("Lambda is the right size", {
  model_specs <- tribble(
    ~b1,  ~b2,  ~b3,  ~b4,  ~size,
    1,    1,    0,    0,     10,
    1,    1,    1,    0,     15,
    0,    0,    0,    1,     13,
    0,    0,    1,    1,     24
  ) %>%
    setup_planted_pattern_model(num_noise_nodes = 15)

  expect_equal(
    nrow(model_specs$Lambda),
    4*(4 + 1)
  )
})

test_that("Noise code lambdas are properly reflected", {
  model_specs <- tribble(
    ~b1,  ~b2,  ~b3,  ~b4,  ~size,
    1,    1,    0,    0,     10,
    1,    1,    1,    0,     15,
    0,    0,    0,    1,     13,
    0,    0,    1,    1,     24
  ) %>%
    setup_planted_pattern_model(num_noise_nodes = 15, noise_p = 0.83)

  noise_lambdas <- model_specs$Lambda %>%
    filter(b == 5) %>%
    pull(avg_num_cons)

  expect_true(
    all(noise_lambdas == 0.83)
  )
})

test_that("Makes on and off lambdas are what they should be", {
  model_specs <- tribble(
    ~b1,  ~b2,  ~b3,  ~b4,  ~size,
    1,    1,    0,    0,     10,
    1,    1,    1,    0,     15
  ) %>%
    setup_planted_pattern_model(planted_p_on = 0.92, planted_p_off = 0.24)

  # "On" lambdas
  model_specs$Lambda %>%
    filter(a == 1, b %in% c(1,2)) %>%
    pull(avg_num_cons) %>%
    magrittr::equals(0.92) %>%
    all() %>%
    expect_true()

  # "Off" lambdas
  model_specs$Lambda %>%
    filter(a == 1, b %in% c(3,4)) %>%
    pull(avg_num_cons) %>%
    magrittr::equals(0.24) %>%
    all() %>%
    expect_true()
})

test_that("Makes sure size column is provided", {
  expect_error(
    tribble(
      ~b1,  ~b2,  ~b3,  ~b4,
      1,    1,    1,    0,
      1,    1,    1,    0,
      0,    0,    0,    1
    ) %>%
      setup_planted_pattern_model(),
  "Requested patterns dataframe is missing size column.")
})

test_that("Catches when identical patterns are supplied", {
  expect_error(
    tribble(
      ~b1,  ~b2,  ~b3,  ~b4,  ~size,
      1,    1,    1,    0,     10,
      1,    1,    1,    0,     15,
      0,    0,    0,    1,     13
    ) %>%
      setup_planted_pattern_model(),
    "Two or more requested patterns are the same."
  )
})

test_that("Makes sure sizes are reasonable", {
  expect_error(
    tribble(
      ~b1,  ~b2,  ~b3,  ~b4,  ~size,
      1,    1,    1,    0,     -1,
      1,    1,    0,    0,     12.3,
      0,    0,    0,    1,     13
    ) %>%
      setup_planted_pattern_model(),
    "Requested pattern size(s) of -1, 12.3 not valid.",
    fixed = TRUE
  )
})

