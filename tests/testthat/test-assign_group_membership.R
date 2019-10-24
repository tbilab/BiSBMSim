context("Group membership assignment")


has_all_groups <- function(N, K, place_randomly, ensure_all_groups_seen){
  num_unique_memberships <- assign_group_membership(N, K, place_randomly = place_randomly, ensure_all_groups_seen = ensure_all_groups_seen) %>%
    unique() %>%
    length()

  num_unique_memberships == K
}

#' assign_group_membership(10, 3)
#' assign_group_membership(10, 4, place_randomly = TRUE)
#' assign_group_membership(10, 4, place_randomly = TRUE, ensure_all_groups_seen = FALSE)
test_that("Correct number of memberships are returned", {
  N <- 15
  K <- 5
  memberships <-

  assign_group_membership(N, K) %>%
    length() %>%
    expect_equal(N)

  assign_group_membership(N, K, place_randomly = TRUE) %>%
    length() %>%
    expect_equal(N)

  assign_group_membership(N, K, place_randomly = TRUE, ensure_all_groups_seen = FALSE) %>%
    length() %>%
    expect_equal(N)
})

test_that("Basic sequential membership works", {
  N <- 15
  K <- 5
  memberships <- assign_group_membership(N, K)

  expect_equal(memberships[1:K], 1:K)
})

test_that("All groups are seen when requested", {
  N <- 10
  K <- 9
  repetitions <- 10

  1:repetitions %>%
    purrr::map_lgl(~has_all_groups(N = N, K = K, place_randomly = TRUE, ensure_all_groups_seen = TRUE)) %>%
    all() %>%
    expect_true()
})

test_that("Not all groups are sometimes seen when allowed", {
  N <- 10
  K <- 9
  repetitions <- 10

  1:repetitions %>%
    purrr::map_lgl(~has_all_groups(N = N, K = K, place_randomly = TRUE, ensure_all_groups_seen = FALSE)) %>%
    all() %>%
    expect_false()
})

test_that("Cant have more groups than nodes", {
  expect_error(assign_group_membership(N = 5, K = 6))
})

test_that("Cant have more groups than nodes", {
  expect_error(assign_group_membership(N = 5, K = 6))
})
