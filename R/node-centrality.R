#' Degree Centrality
#' 
#' @param net net
#' @param direction direction of ties
#' @param loops whether to include loops
#' @param node_index index of target nodes
#' @param allow_bipartite allow bipartite
#' @param ... named arguments passed on to lower-level functions
#' 
#' @return `numeric` `vector`
#' 
#' @template author-bk
#' 
#' @export
cent_degree <- function(net, direction = c("in", "out", "all"), 
                        loops = net_has_loops(net),
                        use_edge_weights = FALSE,
                        node_index = node_seq(net),
                        allow_bipartite = FALSE, ...) {
  UseMethod("cent_degree")
}

# .cent_degree <- function(.net, .direction, .loops, .node_index, ...) {
#   UseMethod(".cent_degree")
# }

#' @rdname cent_degree
#' @export
cent_degree.igraph <- function(net, direction = c("in", "out", "all"), 
                               loops = net_has_loops(net),
                               use_edge_weights = FALSE,
                               node_index = node_seq(net),
                               allow_bipartite = FALSE, ...) {
  direction <- .match_arg(direction, c("in", "out", "all"))
  use_edge_weights <- use_edge_weights && net_is_weighted(net)
  
  if (use_edge_weights) {
    igraph::strength(graph = net, vids = node_index, mode = direction, loops = loops)
  } else {
    igraph::degree(graph = net, v = node_index, mode = direction, loops = loops,
                   normalized = FALSE)
  }
}

#' @rdname cent_degree
#' @export
cent_degree.network <- function(net, direction = c("in", "out", "all"), 
                                loops = net_has_loops(net),
                                use_edge_weights = FALSE,
                                node_index = node_seq(net),
                                allow_bipartite = FALSE, ...) {
  direction <- switch(.match_arg(direction, c("in", "out", "all")),
                      "in" = "indegree", 
                      "out" = "outdegree",
                      "all" = "freeman")
  gmode_arg <- if (net_is_directed(net)) "digraph" else "graph"
  use_edge_weights <- use_edge_weights && net_is_weighted(net)
  
  sna::degree(dat = net, g = 1L, nodes = node_index, gmode = gmode_arg, 
              diag = loops, cmode = direction, rescale = FALSE, 
              ignore.eval = !use_edge_weights)
}

# 
# .fetch_args <- function() {
#   args <- match.call(sys.function(which = -1L), 
#                      sys.call(which = -1L),
#                      expand.dots = FALSE)$..
#   c(as.list(parent.frame()), args)
# }
# 
# .fetch_args <- function() {
#   dots <- match.call(sys.function(which = -1L), 
#                      sys.call(which = -1L), 
#                      expand.dots = FALSE)$..
#   c(as.list(parent.frame()), dots)
# }
# 
# face_off <- function(face = "off", off = c("a", "b")) {
#   # off <- match.arg(off, c("b", "a"), several.ok = FALSE)
#   
#   .fetch_args()
# }







