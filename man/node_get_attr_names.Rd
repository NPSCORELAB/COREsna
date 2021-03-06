% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nodes.R
\name{node_get_attr_names}
\alias{node_get_attr_names}
\alias{node_get_attr_names.igraph}
\alias{node_get_attr_names.network}
\title{Get the names of a network's node attributes.}
\usage{
node_get_attr_names(.net)

\method{node_get_attr_names}{igraph}(.net)

\method{node_get_attr_names}{network}(.net)
}
\arguments{
\item{.net}{network of class \code{igraph} or \code{network}.}
}
\value{
\code{character} \code{vector}
}
\description{
Get the names of a network's node attributes.
}
\examples{
node_get_attr_names(.net = florence_business_ig)

florence_business_nw \%>\% 
  node_get_attr_names()

}
\author{
Brendan Knapp \email{brendan.knapp@nps.edu}
}
