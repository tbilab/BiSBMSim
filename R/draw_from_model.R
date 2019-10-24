#' Draw data from model
#'
#' Based on a fully specified model of node membership vectors and Lambda matrix
#' for average group pair connections, draws data from a bipartite SBM model and
#' returns that data as a datafrom of each pair and the total number of
#' connection seen for that pair.
#'
#' @param b_a Group membership vector for the a nodes. See \code{\link{assign_group_membership()}} for more details on format.
#' @param b_b Group membership vectof for the b nodes
#' @param Lambda Tibble containing average number of connections for each unique pair of a and b node groups. See \code{\link{generate_random_lambda()}} for more details.
#'
#' @return A tibble with colums for ids of each node pair (`a, b`), the groups those node are in (`a_group, b_group`) and the number of edges between those two pairs from model draw (`num_edges`).
#' @export
#'
#' @examples
draw_from_model <- function(b_a, b_b, Lambda){

  node_pairs <- expand.grid(
    a = 1:length(b_a),
    b = 1:length(b_b)
  )

  n_pairs <- nrow(node_pairs)
  node_pairs$a_group <- b_a[node_pairs$a]
  node_pairs$b_group <- b_b[node_pairs$b]

  # Grab the group membership for each node and get avg connections for each
  # pair by joining with Lambda matrix
  pair_groups <- dplyr::inner_join(
    node_pairs[c("a_group", "b_group")],
    Lambda,
    by = c('a_group' = 'a', 'b_group' = 'b')
  )

  # Draw from Poison distribution with given average for each pair
  node_pairs$num_edges <- rpois(n_pairs, lambda = pair_groups$avg_num_cons)
  as_tibble(node_pairs)
}
