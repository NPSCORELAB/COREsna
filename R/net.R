#' Is a network directed?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_is_directed(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_is_directed()
#' 
#' @export
net_is_directed <- function(.net) {
  UseMethod("net_is_directed")
}

#' @rdname net_is_directed
#' @importFrom igraph is_directed
#' @export
net_is_directed.igraph <- function(.net) {
  is_directed(.net)
}

#' @rdname net_is_directed
#' @export
net_is_directed.network <- function(.net) {
  isTRUE(.net[["gal"]][["directed"]])
}

#' Are a network's edges weighted?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_is_weighted(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_is_weighted()
#' 
#' @export
net_is_weighted <- function(.net) {
  .validate_net_arg(.net)
  "weight" %in% edge_get_attr_names(.net)
}

#' Is a network bipartite?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_is_bipartite(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_is_bipartite()
#' 
#' @export
net_is_bipartite <- function(.net) {
  UseMethod("net_is_bipartite")
}

#' @rdname net_is_bipartite
#' @importFrom igraph is_bipartite
#' @export
net_is_bipartite.igraph <- function(.net) {
  is_bipartite(.net)
}

#' @rdname net_is_bipartite
#' @export
net_is_bipartite.network <- function(.net) {
  is.numeric(.net[["gal"]][["bipartite"]])
}

#' Is a network multiplex?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_is_multiplex(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_is_multiplex()
#' 
#' @export
net_is_multiplex <- function(.net) {
  UseMethod("net_is_multiplex")
}

#' @rdname net_is_multiplex
#' @importFrom igraph any_multiple
#' @export
net_is_multiplex.igraph <- function(.net) {
  any_multiple(.net)
}

#' @rdname net_is_multiplex
#' @export
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


#' Does a network have any isolate nodes?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_has_isolates(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_has_isolates()
#' 
#' @export
net_has_isolates <- function(.net) {
  UseMethod("net_has_isolates")
}

#' @rdname net_has_isolates
#' @importFrom igraph degree
#' @export
net_has_isolates.igraph <- function(.net) {
  any(degree(.net, mode = "total") == 0)
}

#' @rdname net_has_isolates
#' @importFrom network as.matrix.network.edgelist
#' @export
net_has_isolates.network <- function(.net) {
  el <- as.matrix.network.edgelist(.net)
  any(!seq_len(attr(el, "n")) %in% el)
}

#' Does a network have any loop edges?
#'
#' @template param-net
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples
#' net_has_loops(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   net_has_loops()
#' 
#' @export
net_has_loops <- function(.net) {
  UseMethod("net_has_loops")
}

#' @rdname net_has_loops
#' @importFrom igraph which_loop
#' @export
net_has_loops.igraph <- function(.net) {
  any(which_loop(.net))
}

#' @rdname net_has_loops
#' @export
net_has_loops.network <- function(.net) {
  el <- .fetch_edgelist(.net)
  any(el[ , 1L] == el[ , 2L])
}



