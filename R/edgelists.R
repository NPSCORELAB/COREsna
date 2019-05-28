fetch_edgelist <- function(.net, .node_attr = node_name_attr(.net), ...) {
  .validate_net_arg(.net)

  if (!is.null(.node_attr)) {
    fill <- node_get_attr(.net, .node_attr)
  } else {
    fill <- NULL
  }

  out <- .fetch_edgelist(.net)
  if (is.null(fill)) {
    return(out)
  }
  out[] <- fill[out]

  out
}



.fetch_edgelist <- function(.net) {
  UseMethod(".fetch_edgelist")
}

#' @importFrom igraph as_edgelist
.fetch_edgelist.igraph <- function(.net) {
  as_edgelist(graph = .net, names = FALSE)
  
  # unclassed <- unclass(.net)
  # 
  # if (net_is_directed(.net)) {
  #   cbind(unclassed[[3L]], unclassed[[4L]]) + 1  
  # } else {
  #   cbind(unclassed[[4L]], unclassed[[3L]]) + 1
  # }
}

#' @importFrom network as.matrix.network.edgelist
.fetch_edgelist.network <- function(.net) {
  out <- network::as.matrix.network.edgelist(.net)
  attr(out, "n") <- NULL
  attr(out, "vnames") <- NULL
  attr(out, "bipartite") <- NULL
  
  out
  
  # out <- cbind(
  #   .map_num(.net[["mel"]], "outl"),
  #   .map_num(.net[["mel"]], "inl")
  # )
  # na_attr <- .flatten_lgl(
  #   .map(.map(.net[["mel"]], `[[`, "atl"), `[[`, "na")
  # )
  # out <- out[!na_attr, , drop = FALSE]
  # 
  # out
}

