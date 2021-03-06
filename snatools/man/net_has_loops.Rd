% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata.R
\name{net_has_loops}
\alias{net_has_loops}
\alias{net_has_loops.igraph}
\alias{net_has_loops.network}
\alias{net_has_loops.tbl_graph}
\title{Does a Graph Have Loops?}
\usage{
net_has_loops(x)

\method{net_has_loops}{igraph}(x)

\method{net_has_loops}{network}(x)

\method{net_has_loops}{tbl_graph}(x)
}
\arguments{
\item{x}{An \code{igraph}, \code{network}, or \code{tbl_graph}.}
}
\value{
\code{logical} indicating whether \code{x} is contains loop edges.
}
\description{
Does a Graph Have Loops?
}
\seealso{
\code{\link[igraph:which_loop]{igraph::which_loop()}}

\code{\link[network:has.loops]{network::has.loops()}}

\code{\link[tidygraph:edge_is_loop]{tidygraph::edge_is_loop()}}
}
\author{
Brendan Knapp \email{brendan.g.knapp@gmail.com}
}
