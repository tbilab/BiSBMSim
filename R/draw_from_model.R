#' Draw data from model
#'
#' Based on a fully specified model of node membership vectors and Lambda matrix
#' for average group pair connections, draws data from a bipartite SBM model and
#' returns that data as a datafrom of each pair and the total number of
#' connection seen for that pair.
#'
#' @param b_a Group membership vector for the a nodes. See
#'   \code{\link{assign_group_membership()}} for more details on format.
#' @param b_b Group membership vectof for the b nodes
#' @param Lambda Tibble containing average number of connections for each unique
#'   pair of a and b node groups. See \code{\link{generate_random_lambda()}} for
#'   more details.
#' @param binary_connections Is data simple a yes or no connection? If `FALSE`,
#'   function defaults to treating `Lambda` matrix values as mean for Poisson
#'   distribution, otherwise `Lambda` values are interpreted as the probability
#'   of connection using a bernouli distribution. Defaults to `FALSE`.
#' @param a_name Name for the type a nodes in the output.
#' @param b_name Name for the type b nodes in the output.
#'
#' @return A tibble with colums for ids of each node pair (`a, b`), the groups
#'   those node are in (`a_group, b_group`), (`avg_num_cons`) for the expected
#'   number of connections from the `Lambda` matrix and the number of edges
#'   between those two pairs from model draw (`num_edges`).
#' @export
#'
#' @examples
draw_from_model <- function(b_a, b_b, Lambda, binary_connections = FALSE, a_name = "a", b_name = "b"){
  # If were in a binary mode then make sure none of our lambda values are greater than 1.
  if(binary_connections){
    if(any(Lambda$avg_num_cons > 1)){
      stop("In binary connection mode you can't have a lambda greater than one. Either adjust your Lambdas or set binary_connection = FALSE.")
    }
  }

  # Check to make sure we don't have any negative lambas for whatever reason
  if(any(Lambda$avg_num_cons < 0)){
    stop("Lambda contains negative value.")
  }

  node_pairs <- expand.grid(
    a = 1:length(b_a),
    b = 1:length(b_b)
  )

  n_pairs <- nrow(node_pairs)
  node_pairs$a_group <- b_a[node_pairs$a]
  node_pairs$b_group <- b_b[node_pairs$b]

  # Grab the group membership for each node and get avg connections for each
  # pair by joining with Lambda matrix
  node_pairs <- dplyr::inner_join(
    node_pairs,
    Lambda,
    by = c('a_group' = 'a', 'b_group' = 'b')
  )

  # Draw connections from desired distribution!
  if(binary_connections){
    # Draw connections from bernouli
    node_pairs$num_edges <- rbinom(n = n_pairs, size = 1, prob = node_pairs$avg_num_cons)
  } else {
    # Draw from Poison distribution with given average for each pair
    node_pairs$num_edges <- rpois(n = n_pairs, lambda = node_pairs$avg_num_cons)
  }

  as_tibble(node_pairs) %>%
    rename(
      !!rlang::sym(a_name) := a,
      !!rlang::sym(b_name) := b
    )
}
