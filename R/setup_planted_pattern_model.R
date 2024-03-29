#' Setup a planted pattern model
#'
#' Supplied with a dataframe of planted patterns of b-nodes, will return all the
#' neccesary components to draw from SBM model with set planted structure using
#' \code{\link{draw_from_model()}}.
#'
#' @param planted_patterns Tibble containing planted patterns for node a groups
#'   by activating b nodes with `1` or `0` in columns prefixed with pattern
#'   `b1`, `b2`, .... Additionally a column `size` is used to control how many a
#'   nodes have the planted pattern.
#' @param num_noise_nodes How many additional b nodes with no pattern are added
#'   to model?
#' @param noise_p The individual lambda for connections between the noise
#'   b-nodes and any a-node.
#' @param planted_p_on The lambda for a planted pattern b-node that has been
#'   turned on in a pattern.
#' @param planted_p_off The lambda for a planted pattern b-node that has been
#'   turned off in pattern.
#'
#' @return List with group membership vectors for both node types (`b_a, b_b`),
#'   and the Lambda matrix for edge behavior (`Lambda`).
#' @export
#'
#' @examples
#' my_patterns <- dplyr::tribble(
#'   ~b1,  ~b2,  ~b3,  ~b4,  ~size,
#'     1,    1,    0,    0,     10,
#'     1,    1,    1,    0,     15,
#'     0,    0,    0,    1,     13,
#'     0,    0,    1,    1,     24
#' )
#' setup_planted_pattern_model(my_patterns, num_noise_nodes = 15)
#'
setup_planted_pattern_model <- function(planted_patterns, num_noise_nodes = 10, noise_p = 0.2, planted_p_on = 0.95, planted_p_off = 0.05){
  # Tests to make sure provided patterns tibble is in right format

  # Test if size column present
  missing_size_col <- !('size' %in% colnames(planted_patterns))
  if(missing_size_col){
    stop("Requested patterns dataframe is missing size column.")
  }

  # Test for redundent patterns
  all_patterns <- planted_patterns %>%
    dplyr::select(-size) %>%
    tidyr::unite('pattern') %>%
    dplyr::pull('pattern')

  has_non_unique_patterns <- length(all_patterns) != length(unique(all_patterns))
  if(has_non_unique_patterns){
    stop("Two or more requested patterns are the same.")
  }

  # Make sure sizes are positive integers
  requested_sizes <- planted_patterns$size

  not_positive <- requested_sizes <= 0
  not_integer  <- requested_sizes %% 1 != 0
  non_valid_size <- not_positive | not_integer

  if(any(non_valid_size)){
    bad_sizes <- requested_sizes[which(non_valid_size)]
    stop(paste("Requested pattern size(s) of", paste(bad_sizes, collapse = ", "),"not valid."))
  }

  # Total number of nodes of type a
  N_a <- sum(planted_patterns$size)

  # Total number of nodes of type b
  N_planted <- ncol(planted_patterns) - 1
  N_b <- N_planted + num_noise_nodes

  # Number of clusters for a nodes
  K_a <- nrow(planted_patterns)

  # Group membership for a-nodes
  b_a <- rep(1:K_a, times = planted_patterns$size)

  # Group membership for b-nodes
  b_b <- c(1:N_planted, rep(N_planted + 1, times = num_noise_nodes))

  # Build out a uniform lambda chunk of these noise nodes connection probs
  noise_patterns <- dplyr::tibble(
    a = 1:K_a,
    b = paste0("b", N_planted + 1),
    avg_num_cons = noise_p
  )

  # Build full lambda for planted patterns and also the noise codes for feeding into simulator
  Lambda <- planted_patterns %>%
    dplyr::select(-size) %>%
    dplyr::mutate_all(~ifelse(. == 1, planted_p_on, planted_p_off)) %>%
    dplyr::mutate(a = 1:dplyr::n()) %>%
    tidyr::gather(
      key = "b", value = "avg_num_cons", -a
    ) %>%
    dplyr::bind_rows(noise_patterns) %>%
    dplyr::mutate(
      b = as.integer(gsub("b", "", b))
    )

  list(
    b_a = b_a,
    b_b = b_b,
    Lambda = Lambda
  )
}
