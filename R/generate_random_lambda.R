#' Generate a random lambda matrix
#'
#' Given a `K_a` type a clusters, and `K_b` type b clusters and a random
#' generator for average number of connections between nodes, generates a tibble
#' that encodes all pairs of cluster types and the average number of connections
#' between them.
#'
#' @param K_a Number of clusters for node type a
#' @param K_b Number of clusters for node type b
#' @param random_generator Function used to generate average connection values. Defalts to just `runif`, needs to take a single argument of number of values to generate.
#'
#' @return Tibble with three columns: `a` for id of a node cluster, `b` for id of b node cluster, and `avg_num_cons` for the average number of connection between two nodes in cluster a and b respectively.
#' @export
#'
#' @examples
#' generate_random_lambda(K_a = 3, K_b = 2, random_generator = function(n) rpois(n, 5))
generate_random_lambda <- function(K_a, K_b, random_generator = runif){
  dplyr::mutate(
    expand.grid(
      a = 1:K_a,
      b = 1:K_b
    ),
    avg_num_cons = random_generator(dplyr::n())
  )
}
