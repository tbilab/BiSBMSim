#' Assign group membership to nodes
#'
#' @param N Total number of nodes
#' @param K Total number of groups or clusters
#' @param place_randomly Do we want to randomly sample group membership for nodes? Defaults to `FALSE` so all groups are evenly (or as close to as possible) represented.
#' @param ensure_all_groups_seen If `place_randomly = TRUE`, do we make sure that at least one node is placed in every group?
#'
#' @return An array of memberships of length `N` made up of `K` groups as encoded with integers
#' @export
#'
#' @examples
#' assign_group_membership(10, 3)
#' assign_group_membership(10, 4, place_randomly = TRUE)
#' assign_group_membership(10, 4, place_randomly = TRUE, ensure_all_groups_seen = FALSE)
assign_group_membership <- function(N, K, place_randomly = FALSE, ensure_all_groups_seen = TRUE){
  if(K > N) {
    stop("Can't have more groups than nodes. Did you accidentally flip K and N?")
  }

  all_groups <- 1:K

  if(place_randomly){
    if(ensure_all_groups_seen){
      # Cutoff first K elements of vector and place every group in them
      assignments <- c(
        all_groups,
        sample(x = all_groups, size = N - K, replace = TRUE)
      )
    } else {
      assignments <- sample(x = all_groups, size = N, replace = TRUE)
    }
  } else {
    assignments <- rep(all_groups, length.out = N)
  }

  assignments
}
