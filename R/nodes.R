#' Count the number of nodes in a network.
#'
#' @template param-net
#' 
#' @return `integer` scalar
#' 
#' @template author-bk
#' 
#' @examples 
#' node_count(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   node_count()
#' 
#' @export
node_count <- function(.net) {
  UseMethod("node_count")
}

#' @rdname node_count
#' 
#' @importFrom igraph vcount
#' 
#' @export
node_count.igraph <- function(.net) {
  vcount(.net)
  # unclass(.net)[[1L]]
}

#' @rdname node_count
#' 
#' @export
node_count.network <- function(.net) {
  .net[["gal"]][["n"]]
}

node_seq <- function(.net) {
  .validate_net(.net)

  seq_len(node_count(.net))
}

#' Get the names of a network's node attributes.
#'
#' @template param-net
#' 
#' @return `character` `vector`
#' 
#' @template author-bk
#' 
#' @examples 
#' node_get_attr_names(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   node_get_attr_names()
#' 
#' @export
node_get_attr_names <- function(.net) {
  UseMethod("node_get_attr_names")
}

#' @rdname node_get_attr_names
#' 
#' @importFrom igraph vertex_attr_names
#' 
#' @export
node_get_attr_names.igraph <- function(.net) {
  vertex_attr_names(.net)
}

#' @rdname node_get_attr_names
#' 
#' @export
node_get_attr_names.network <- function(.net) {
  out <- .flatten_chr(
    unique(
      .map(.net[["val"]], names)
    )
  )

  out[out != "na"]
}


#' Does a node attribute exist?
#'
#' @template param-net
#' @template param-node_attr
#' 
#' @return `logical` scalar
#' 
#' @template author-bk
#' 
#' @examples 
#' node_attr_exists(.net = florence_business_ig, .node_attr = "wealth")
#' 
#' florence_business_nw %>% 
#'   node_attr_exists(.node_attr = "priors")
#'   
#' florence_business_ig %>% 
#'   node_attr_exists(.node_attr = "fake attr")
#' 
#' @export
node_attr_exists <- function(.net, .node_attr) {
  .validate_net(.net)

  .node_attr %in% node_get_attr_names(.net)
}


#' Extract a node attribute.
#'
#' @template param-net
#' @template param-node_attr
#' @template param-node_index
#' 
#' @return `vector` (typically)
#' 
#' @template author-bk
#' 
#' @examples 
#' node_get_attr(.net = florence_business_ig, .node_attr = "wealth")
#' 
#' florence_business_nw %>% 
#'   node_get_attr(.node_attr = "wealth")
#' 
#' @export
node_get_attr <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  .validate_net(.net)

  if (!.is_scalar_chr(.node_attr)) {
    .stop("`.node_attr` must be a scalar character.")
  }
  if (!node_attr_exists(.net, .node_attr)) {
    .net <- deparse(substitute(.net))
    .stop("`{.node_attr}` is not a valid node attribute for `{.net}`.")
  }
  if (.is_empty(.node_index)) {
    .warning("`.node_index` is empty.")
    return(NULL)
  }
  if (!is.numeric(.node_index) && !is.logical(.node_index)) {
    .stop("`.node_index` must be a `numeric` or `logical` `vector`")
  }
  if (length(.node_index) != node_count(.net)) {
    compare_msg <- ifelse(length(.node_index) > node_count(.net), "longer", "shorter")
    .net <- deparse(substitute(.net))
    
    .stop("`.node_index` is {compare_msg} than the number of nodes in `{.net}`")
  }

  UseMethod("node_get_attr")
}

#' @rdname node_get_attr
#' 
#' @importFrom igraph vertex_attr
#' 
#' @export
node_get_attr.igraph <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  vertex_attr(graph = .net, name = .node_attr, index = .node_index)
}

#' @rdname node_get_attr
#' 
#' @export
node_get_attr.network <- function(.net, .node_attr, .node_index = node_seq(.net)) {
  .flatten(
    .map(.net[["val"]][.node_index], 
         function(x) x[[.node_attr]] %||% NA)
  )
}

#' Extract node names.
#'
#' @template param-net
#' 
#' @return 
#'  * `node_get_names()`
#'    + `character` `vector`
#'      
#' @details 
#'  * `node_get_names()`
#'    + Uses `node_name_attr()` to determine node names.
#'
#' @template author-bk
#' 
#' @examples 
#' node_get_names(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   node_get_names()
#' 
#' @export
node_get_names <- function(.net) {
  .validate_net(.net)

  out <- node_get_attr(.net, node_name_attr(.net)) %{error}% node_seq(.net)
  if (is.character(out)) {
    return(out)
  }
  as.character(out)
}

#' @rdname node_get_names
#' 
#' @return 
#' * `node_name_attr()`:
#'   + `character` scalar
#'   
#' @details 
#' * `node_name_attr()`
#'   + If `.net` is an `igraph` object, `node_get_names()` returns the node (vertex) 
#'   attribute `"name"`.
#'     + If `"name"` is not a node attribute, `node_get_names()` returns `node_seq(.net)`.
#'   + If `.net` is a `network` object, `node_get_names()` returns the node (vertex)
#'     attribute `"vertex.names"`.
#' 
#' @examples 
#' node_name_attr(.net = florence_business_ig)
#' 
#' florence_business_nw %>% 
#'   node_name_attr()
#' 
#' @export
node_name_attr <- function(.net) {
  .validate_net(.net)

  if (inherits(.net, "igraph")) {
    return("name")
  }
  if (inherits(.net, "network")) {
    return("vertex.names")
  }
}




