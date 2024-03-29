% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw_from_model.R
\name{draw_from_model}
\alias{draw_from_model}
\title{Draw data from model}
\usage{
draw_from_model(b_a, b_b, Lambda, binary_connections = FALSE,
  a_name = "a", b_name = "b")
}
\arguments{
\item{b_a}{Group membership vector for the a nodes. See
\code{\link{assign_group_membership()}} for more details on format.}

\item{b_b}{Group membership vectof for the b nodes}

\item{Lambda}{Tibble containing average number of connections for each unique
pair of a and b node groups. See \code{\link{generate_random_lambda()}} for
more details.}

\item{binary_connections}{Is data simple a yes or no connection? If \code{FALSE},
function defaults to treating \code{Lambda} matrix values as mean for Poisson
distribution, otherwise \code{Lambda} values are interpreted as the probability
of connection using a bernouli distribution. Defaults to \code{FALSE}.}

\item{a_name}{Name for the type a nodes in the output.}

\item{b_name}{Name for the type b nodes in the output.}
}
\value{
A tibble with colums for ids of each node pair (\code{a, b}), the groups
those node are in (\code{a_group, b_group}), (\code{avg_num_cons}) for the expected
number of connections from the \code{Lambda} matrix and the number of edges
between those two pairs from model draw (\code{num_edges}).
}
\description{
Based on a fully specified model of node membership vectors and Lambda matrix
for average group pair connections, draws data from a bipartite SBM model and
returns that data as a datafrom of each pair and the total number of
connection seen for that pair.
}
\examples{
N_a <- 15  # Number of nodes of the a type
N_b <- 10  # Number of nodes of the b type
K_a <- 4    # How many blocks of a type nodes are there
K_b <- 3    # How many blocks of the b type nodes are there

draw_from_model(
  assign_group_membership(N = N_a, K = K_a),
  assign_group_membership(N = N_b, K = K_b),
  Lambda = generate_random_lambda(K_a = K_a, K_b = K_b)
)

}
