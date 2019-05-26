net_is_directed <- function(.net) {
  UseMethod("net_is_directed")
}

#' @importFrom igraph is_directed
net_is_directed.igraph <- function(.net) {
  is_directed(.net)
}

net_is_directed.network <- function(.net) {
  isTRUE(.net[["gal"]][["directed"]])
}

net_is_weighted <- function(.net) {
  "weight" %in% edge_get_attr_names(.net)
}

net_is_bipartite <- function(.net) {
  UseMethod("net_is_bipartite")
}

#' @importFrom igraph is_bipartite
net_is_bipartite.igraph <- function(.net) {
  is_bipartite(.net)
}

net_is_bipartite.network <- function(.net) {
  is.numeric(.net[["gal"]][["bipartite"]])
}

net_is_multiplex <- function(.net) {
  UseMethod("net_is_multiplex")
}

#' @importFrom igraph any_multiple
net_is_multiplex.igraph <- function(.net) {
  any_multiple(.net)
}

net_is_multiplex.network <- function(.net) {
  el <- .fetch_edgelist(.net)
  if (!net_is_directed(.net)) {
    el <- cbind(
      pmin.int(el[, 1L], el[, 2L]), 
      pmax.int(el[, 1L], el[, 2L])
    )
  }
  any(duplicated.matrix(el))
}

net_has_isolates <- function(.net) {
  UseMethod("net_has_isolates")
}

#' @importFrom igraph degree
net_has_isolates.igraph <- function(.net) {
  any(degree(.net) == 0)
}

#' @importFrom network as.matrix.network.edgelist
net_has_isolates.network <- function(.net) {
  el <- as.matrix.network.edgelist(.net)
  any(!seq_len(attr(el, "n")) %in% el)
}

net_has_loops <- function(.net) {
  UseMethod("net_has_loops")
}

#' @importFrom igraph which_loop
net_has_loops.igraph <- function(.net) {
  any(which_loop(.net))
}

net_has_loops.network <- function(.net) {
  el <- .fetch_edgelist(.net)
  any(el[ , 1L] == el[ , 2L])
}



