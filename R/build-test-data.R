build_test_mat <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                         .diag = FALSE, .storage = c("int", "dbl")) {
  .storage <- switch(
    match.arg(.storage, c("int", "dbl"), several.ok = FALSE),
    int = "integer", 
    dbl = "double"
  )
  
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
    
    out <- structure(
      c(1L, 1L, 0L, 1L, 0L, 1L,
        1L, 0L, 2L, 0L, 0L, 1L, 
        0L, 1L, 1L, 1L, 0L, 0L, 
        1L, 0L, 1L, 0L, 2L, 0L, 
        0L, 0L, 0L, 1L, 1L, 1L, 
        0L, 2L, 0L, 0L, 1L, 1L),
      .Dim = c(6L, 6L)
    )

    # return(out)

  } else {
    out <- structure(
      c(1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L,
        1L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 1L, 0L,
        0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L,
        0L, 0L, 1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L,
        0L, 0L, 0L, 1L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L,
        1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L,
        0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 1L, 0L, 0L,
        0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 1L, 0L, 2L,
        0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 0L, 1L, 0L,
        0L, 0L, 2L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L,
        0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L),
      .Dim = c(12L, 12L)
    )

  }
  
  if (!.diag) {
    diag(out) <- 0L
  }
  
  if (!.weighted) {
    out[out == 2L] <- 1L
  }
  
  if (!.directed) {
    # out[] <- 0.5 * (out + t(out))
    out[] <- (out + t(out)) %/% 2
    out[lower.tri(out)] <- 0L
  }

  if (typeof(out) != .storage) {
    storage.mode(out) <- .storage
  }
  
  out
}

el_from_adj_mat <- function(.adj_mat, .weighted = FALSE, ...) {
  n_cols <- ifelse(.weighted, 3L, 2L)
  
  melted <- matrix(c(row(.adj_mat), col(.adj_mat), as.vector(.adj_mat)), ncol = 3L)
  
  melted[melted[, 3L] != 0, seq_len(n_cols)]
}

build_test_el <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
                        .diag = FALSE, .multiplex = FALSE, .storage = c("int", "dbl"), ...) {
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
  }

  adj_mat <- build_test_mat(.directed = .directed, .bipartite = .bipartite,
                          .weighted = .weighted, .diag = .diag)

  out <- el_from_adj_mat(adj_mat, .weighted = .weighted, ...)

  out[ , 1L:2L] <- cbind(
    pmin.int(out[, 1L], out[, 2L]), pmax.int(out[, 1L], out[, 2L])
  )

  if (!.multiplex) {
    out <- unique(out)
  }

  out
}




build_test_ig <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                        .diag = FALSE, .isolates = FALSE, ...) {
  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
  }
  
  el <- build_test_el(.directed = .directed, .bipartite = .bipartite, 
                    .weighted = .weighted, .diag = .diag)
  n_nodes <- length(unique(as.vector(el[ , 1L:2L])))
  
  out <- igraph::make_empty_graph(n = n_nodes, directed = .directed)
  
  out <- igraph::add_edges(out, edges = t(el[ , 1L:2L]))
  
  if (.weighted) {
    igraph::E(out)$weight <- el[ , 3L]
  } 
  
  if (.bipartite) {
    igraph::V(out)$type <- c(rep(TRUE, n_nodes %/% 2), rep(FALSE, n_nodes %/%  2))
  }
  
  igraph::V(out)$node_attr <- letters[1:n_nodes]
  
  if (.isolates) {
    out <- igraph::add_vertices(out, 1L)
  }
  
  out
}

build_test_nw <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, 
                          .diag = FALSE, .isolates = FALSE, ...) {
  el <- build_test_el(.directed = .directed, .bipartite = .bipartite, 
                      .weighted = .weighted, .diag = .diag)
  
  n_nodes <- length(unique(as.vector(el[ , 1L:2L])))
  
  if (.isolates) {
    n_nodes <- n_nodes + 1
  }
  

  if (.bipartite) {
    .diag <- FALSE
    .directed <- FALSE
    .bipartite <- n_nodes %/% 2
  } else {
    .bipartite <- FALSE
  }

  out <- network::network.initialize(
    n = n_nodes,  directed = .directed, loops = .diag,
    hyper = FALSE, multiple = FALSE, bipartite = .bipartite
  )
  
  network::network.edgelist(
    x = el, g = out
  )
  
  network::set.vertex.attribute(out, attrname = "node_attr", letters[1:n_nodes])

  # network::add.edges.network(
  #   out, head = as.list(el[ , 1L]), tail = as.list(el[ , 2L])
  # )
  # 
  if (.weighted) {
    network::set.edge.attribute(out, attrname = "weight", value = el[ , 3L])
  }

  out
}




# .sort_edge_indices <- function(.x) {
#   cbind(
#     pmin(.x[, 1L], .x[, 2L]), pmax(.x[, 1L], .x[, 2L])
#   )
# }




# build_test_el_ig <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
#                            .diag = FALSE, .storage = c("int", "dbl")) {
#   .fetch_edgelist(build_test_ig(.directed = .directed, .bipartite = .bipartite,
#                               .weighted = .weighted, .diag = .diag))
# }
# 
# build_test_el_nw <- function(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE,
#                            .diag = FALSE, .storage = c("int", "dbl")) {
#   g <- build_test_nw(.directed = .directed, .bipartite = .bipartite,
#                    .weighted = .weighted, .diag = .diag)
#   out <- network::as.matrix.network.edgelist(g)
#   
#   attr(out, "n") <- NULL
#   attr(out, "vnames") <- NULL
#   
#   out
# }

build_test_args <- function() {
  init <- list(
    list(.directed = TRUE, .bipartite = FALSE, .weighted = FALSE, .diag = FALSE), 
    list(.directed = FALSE, .bipartite = TRUE, .weighted = TRUE, .diag = TRUE)
  )
  
  out <- expand.grid(
    do.call(rbind.data.frame, init)
  )
  
  apply(out, 1L, as.list)
}

run_edgelist_tests <- function() {
  test_args <- build_test_args()
  
  for (i in seq_along(test_args)) {
    target_el <- do.call(build_test_el, test_args[[i]])
    test_ig <- .fetch_edgelist(do.call(build_test_ig, test_args[[i]]))
    test_nw <- .fetch_edgelist(do.call(build_test_nw, test_args[[i]]))
    
    res <- .all_equal(
      target_el[ , 1L:2L], test_ig, test_nw
    )
    
    
    if (!res) {
      print("test_args: "); print(test_args[[i]])
      print("test_ig: "); print(test_ig)
      print("test_nw: "); print(test_nw)
      return(FALSE)
    }
  }
  TRUE
}

run_node_attr_tests <- function() {
  test_args <- build_test_args()
  
  for (i in seq_along(test_args)) {
    if (test_args[[i]]$.bipartite) {
      target <- c("a", "b", "c", "d", "e", "f")
    } else {
      target <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
    }
    
    test_ig <- node_get_attr(do.call(build_test_ig, test_args[[i]]), "node_attr")
    test_nw <- node_get_attr(do.call(build_test_nw, test_args[[i]]), "node_attr")
    
    res <- .all_equal(
      target, test_ig, test_nw
    )

    if (!res) {
      print("test_args: "); print(test_args[[i]])
      print("test_ig: "); print(test_ig)
      print("test_nw: "); print(test_nw)
      return(FALSE)
    }
  }
  TRUE
}
