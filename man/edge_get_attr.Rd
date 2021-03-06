% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edges.R
\name{edge_get_attr}
\alias{edge_get_attr}
\alias{edge_get_attr.igraph}
\alias{edge_get_attr.network}
\title{Extract an edge attribute.}
\usage{
edge_get_attr(.net, .edge_attr, .edge_index = edge_seq(.net))

\method{edge_get_attr}{igraph}(.net, .edge_attr,
  .edge_index = edge_seq(.net))

\method{edge_get_attr}{network}(.net, .edge_attr,
  .edge_index = edge_seq(.net))
}
\arguments{
\item{.net}{network of class \code{igraph} or \code{network}.}

\item{.edge_attr}{\code{character} scalar corresponding to the name of a edge attribute in \code{.net}.}

\item{.edge_index}{\code{numeric} or \code{logical} vector indicating which edges to target.}
}
\value{
\code{vector} (typically)
}
\description{
Extract an edge attribute.
}
\examples{
edge_get_attr(.net = florence_business_ig, .edge_attr = "edge_type")

florence_marriage_nw \%>\% 
  edge_get_attr(.edge_attr = "edge_type")

florence_marriage_nw \%>\% 
  edge_get_attr(.edge_attr = "edge_type", .edge_index = 1:5)

}
\author{
Brendan Knapp \email{brendan.knapp@nps.edu}
}
