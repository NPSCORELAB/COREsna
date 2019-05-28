# "default"ers =========================================================================

# @name defaulters
# 
# @title Conditionally replace objects with default values.
# 
# @param .lhs Object to test.
# @param .rhs Object to return if test returns `FALSE` for `.lhs`.
# 
# @return `.lhs` or `.rhs`
# 
# @template author-bk
# 
# @details 
# * If `.lhs` is...
#   + `%||%`: `NULL`
#   + `%{}%`: empty (`length(.lhs) == 0L`)
#   + `%{NA}%`: `NA`
#   + `%{T}%`: `TRUE`
#   + `%{F}%`: `FALSE`
# * ... returns `.rhs`.
#   + Returns `.lhs` otherwise.
# 
# @examples 
# NULL %||% 1
# 1 %||% NULL
# 
# integer(length = 0L) %{}% NA
# 1 %{}% NA
# 
# NA %{NA}% 1
# 1 %{NA}% NA
# 
# TRUE %{T}% FALSE
# FALSE %{T}% TRUE
# 
# TRUE %{F}% FALSE
# FALSE %{F}% TRUE

# @rdname defaulters
# 
# @export
`%||%` <- function(.lhs, .rhs) {
  if (is.null(.lhs)) .rhs else .lhs
}

# @rdname defaulters
# 
# @export
`%{}%` <- function(.lhs, .rhs) {
  if (length(.lhs) == 0L) .rhs else .lhs
}

# @rdname defaulters
# 
# @export
`%{NA}%` <- function(.lhs, .rhs) {
  if (isTRUE(is.na(.lhs))) .rhs else .lhs
}

# @rdname defaulters
# 
# @export
`%{T}%` <- function(.lhs, .rhs) {
  if (isTRUE(.lhs)) .rhs else .lhs
}

# @rdname defaulters
# 
# @export
`%{F}%` <- function(.lhs, .rhs) {
  if (.isFALSE(.lhs)) .rhs else .lhs
}

`%{error}%` <- function(.lhs, .rhs, .envir = parent.frame()) {
  tryCatch(.lhs, error = function(e) .rhs)
}


# `is_*()`/`all_*()` ====================================================================
.isFALSE <- function(x) { 
  # `isFALSE()` apparely didn't show up until R 3.5
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}

# `%==%` <- function(.lhs, .rhs) {
#   isTRUE(all.equal(.lhs, .rhs))
# }
# 
# `%!=%` <- function(.lhs, .rhs) {
#   !isTRUE(all.equal(.lhs, .rhs))
# }

.all_equal <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)
  
  if (length(objs) == 2L) {
    return(isTRUE(all.equal(objs[[1L]], objs[[2L]])))
  }
  
  for (i in seq_along(objs[2L:length(objs)])) {
    if (!isTRUE(all.equal(objs[[1L]], objs[[i]]))) {
      return(FALSE)
    }
  }
  TRUE
}

.all_identical <- function(...) {
  objs <- list(...)
  stopifnot(length(objs) > 1L)
  
  if (length(objs) == 2L) {
    return(identical(objs[[1L]], objs[[2L]]))
  }
  
  for (i in seq_along(objs[2L:length(objs)])) {
    if (!identical(objs[[1L]], objs[[i]])) {
      return(FALSE)
    }
  }
  TRUE
}

# matrices ==============================================================================
.tri_upper <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  .x[upper.tri(.x, diag = .diag)]
}

.tri_lower <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  .x[lower.tri(.x, diag = .diag)]
}

.is_empty <- function(.x) {
  length(.x) == 0L
}

.is_not_empty <- function(.x) {
  length(.x) > 0L
}

.is_scalar <- function(.x) {
  length(.x) == 1L
}

.is_scalar_chr <- function(.x) {
  .is_scalar(.x) && is.character(.x)
}

.is_symmetric <- function(.x, .diag = FALSE) {
  stopifnot(is.matrix(.x))
  all(
    .tri_upper(.x, .diag = .diag) == .tri_lower(.x, .diag = .diag)
  )
}

# mappers ===============================================================================
.as_mapper <- function(.f, .default) {
  if (!class(.f) %in% c("character", "numeric", "integer")) return(.f)

  force(.default)

  function(.x) {
    .x[[.f]] %{error}% .default
  }
}
.map <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  lapply(.x, .f, ...)
}

.map_num <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  vapply(X = .x, FUN = .f, FUN.VALUE = numeric(1L), ...)
}

.map_int <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  vapply(X = .x, FUN = .f, FUN.VALUE = integer(1L), ...)
}

.map_dbl <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  vapply(X = .x, FUN = .f, FUN.VALUE = double(1L), ...)
}

.map_chr <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  vapply(X = .x, FUN = .f, FUN.VALUE = character(1L), ...)
}

.map_lgl <- function(.x, .f, ..., .default = NULL) {
  .f <- .as_mapper(.f, .default = .default)
  vapply(X = .x, FUN = .f, FUN.VALUE = logical(1L), ...)
}

.map_which <- function(.x, .f, ...) {
  which(.map_lgl(.x, .f, ...))
}


# .map_dbl <- function(.x, .f, ...) {
#   vapply(X = .x, FUN = .f, FUN.VALUE = double(1L), ...)
# }
# 
# .map_int <- function(.x, .f, ...) {
#   vapply(X = .x, FUN = .f, FUN.VALUE = integer(1L), ...)
# }
# 

.flatten <- function(.x) {
  unlist(.x, use.names = TRUE, recursive = FALSE)
}

.flatten_chr <- function(.x) {
  list_indices <- .map_which(.x, is.list)
  
  if (!.is_empty(list_indices)) {
    list_indices <- .txt_flatten(list_indices, .collapse = ", ")
    .stop("Can't coerce the following elements from `list` to `character`: {list_indices}")
  }

  out <- .flatten(.x)
  
  as.vector(out, mode = "character")
}

.flatten_lgl <- function(.x) {
  list_indices <- .map_which(.x, is.list)
  
  if (!.is_empty(list_indices)) {
    list_indices <- .txt_flatten(list_indices, .collapse = ", ")
    .stop("Can't coerce the following elements from `list` to `logical`: {list_indices}")
  }
  
  out <- .flatten(.x)
  
  as.vector(out, mode = "logical")
}

# misc =================================================================================
.compact <- function(.x) {
  Filter(.is_not_empty, .x)
}


# throwers =============================================================================

#' @importFrom glue glue
.stop <- function(..., .envir = parent.frame()) {
  stop(glue(..., .envir = .envir), call. = FALSE)
}

#' @importFrom glue glue
.message <- function(..., .envir = parent.frame()) {
  message(glue(..., .envir = .envir), call. = FALSE)
}

#' @importFrom glue glue
.warning <- function(..., .envir = parent.frame()) {
  warning(glue(..., .envir = .envir), call. = FALSE)
}

# .txt_* ===============================================================================
.txt_flatten <- function(..., .collapse = "") {
  paste0(..., collapse = .collapse)
}


# validators ===========================================================================
.match_arg <- function(.arg, .options) {
  stopifnot(.arg %in% .options)
  match.arg(.arg, choices = .options, several.ok = FALSE)
}

.net_is_valid <- function(.net) {
  inherits(.net, "igraph") || inherits(.net, "network")
}

.validate_net_arg <- function(.net_arg, .allow_bipartite = FALSE) {
  if (!.net_is_valid(.net_arg)) {
    .stop("`{.net_arg}` is not a valid object. COREsna currently only supports `igraph`
          and `network` objects.")
  }

  if (net_is_bipartite(.net_arg) && .allow_bipartite) {
    .net_arg <- deparse(substitute(.net_arg))
    .stop("{.net_arg} is bipartite")
  }
}

# .validate_node_index_arg <- function(.net_arg, .node_index_arg) {
#   if (.is_empty(.node_index_arg)) {
#     .stop("`node_index` is empty.")
#   }
# 
#   if (!is.numeric(.node_index_arg) && !is.logical(.node_index_arg)) {
#     bad_class <- .txt_flatten(class(.node_index_arg), .collapse = ", ")
#     .stop("`node_index` must be a `numeric` or `logical` `vector`, not {bad_class}")
#   }
# 
#   if (is.logical(.node_index_arg) && length(.node_index_arg) != node_count(.net_arg)) {
#     compare_msg <- ifelse(
#       length(.node_index_arg) > node_count(.net_arg), "longer", "shorter"
#     )
#     .net_arg <- deparse(substitute(.net_arg))
#     .stop(
#       "`node_index` is `logical`, but {compare_msg} than the number of nodes in
#       `{.net_arg}`."
#     )
#   }
# }
# 
# .validate_loops_arg <- function(.loops_arg) {
#   if (!is.logical(.loops_arg)) {
#     bad_class <- .txt_flatten(class(.loops_arg), .collapse = ", ")
#     .stop("`loops` must be logical, not {bad_class}.")
#   }
# }
# 
# .validate_direction_arg <- function(.direction_arg) {
#   if (!.direction_arg %in% c("in", "out", "all")) {
#     .direction_arg <- deparse(substitute(.direction_arg))
#     .stop(
#       '`direction` must be one of `"in"`, `"out"`, or `"all"`, not {.direction_arg}.'
#     )
#   }
# }
# 
# .args_to_match <- function() {
#   c("direction", "node_index", "edge_index", "loops", "allow_bipartite")
# }
# 
# .valid_args <- function() {
#   
# }
# 
# .validate_args <- function(.args, .which_args = c("net", "direction", "node_index",
#                                                   "edge_index", "loops",
#                                                   "allow_bipartite")) {
#   .which_args <- .which_args[.which_args %in% names(.args)]
#   
#   if ("net" %in% .which_args) {
#     net_arg <- .args[["net"]]
#     
#     if (!.net_is_valid(net_arg)) {
#       bad_class <- .txt_flatten(class(net_arg), .collapse = ", ")
#       .stop("`{net_arg}` is not a valid object. COREsna currently only supports `igraph`
#           and `network` objects, not {net_arg}.")
#     }
#     
#     if (net_is_bipartite(net_arg) && .args[["allow_biparite"]]) {
#       .net_arg <- deparse(substitute(.net_arg))
#       .stop("{net_arg} is bipartite.")
#     }
#   }
#   
#   
#   if ("direction" %in% .which_args) {
#     if (.all_equal(.args[["direction"]], c("in", "out", "all"))) {
#       .args[["direction"]] <- "in"
#     }
#     if (!.is_scalar_chr(.args[["direction"]])) {
#       bad_arg <- .args[["direction"]]
#       bad_arg <- deparse(substitute(bad_arg))
#       .stop(
#         '`direction` must be a scalar `character`, not {bad_arg}.'
#       )
#     }
#     if ( !.args[["direction"]] %in% c("in", "out", "all")) {
#       bad_arg <- .args[["direction"]]
#       bad_arg <- deparse(substitute(bad_arg))
#       .stop(
#         '`direction` must be one of `"in"`, `"out"`, or `"all"`, not {bad_arg}.'
#       )
#     }
#   }
#   
#   if ("loops" %in% .which_args) {
#     if (!is.logical(.args[["loops"]])) {
#       bad_class <- .txt_flatten(class(.args[["loops"]]), .collapse = ", ")
#       .stop("`loops` must be logical, not {bad_class}.")
#     }
#   }
#   
#   if ("node_index" %in% .which_args) {
#     node_index_arg <- .args[["node_index"]]
#     if (.is_empty(node_index_arg)) {
#       .stop("`node_index` is empty.")
#     }
#     
#     if (!is.numeric(node_index_arg) && !is.logical(node_index_arg)) {
#       bad_class <- .txt_flatten(class(node_index_arg), .collapse = ", ")
#       .stop("`node_index` must be a `numeric` or `logical` `vector`, not `{bad_class}`")
#     }
#     
#     if (is.logical(node_index_arg) && length(node_index_arg) != node_count(net_arg)) {
#       if (length(node_index_arg) > node_count(net_arg)) {
#         compare_msg <- "longer"
#       } else {
#         compare_msg <- "shorter"
#       }
#       net_arg <- deparse(substitute(net_arg))
#       .stop(
#         "`node_index` is `logical`, but {compare_msg} than the number of nodes in`{net_arg}`."
#       )
#     }
#   }
#   
#   if ("edge_index" %in% .which_args) {
#     edge_index_arg <- .args[["edge_index"]]
#     if (.is_empty(edge_index_arg)) {
#       .stop("`edge_index` is empty.")
#     }
#     
#     if (!is.numeric(edge_index_arg) && !is.logical(edge_index_arg)) {
#       bad_class <- .txt_flatten(class(edge_index_arg), .collapse = ", ")
#       .stop("`edge_index` must be a `numeric` or `logical` `vector`, not {bad_class}")
#     }
#     
#     if (is.logical(edge_index_arg) && length(edge_index_arg) != edge_count(net_arg)) {
#       if (length(edge_index_arg) > node_count(net_arg)) {
#         compare_msg <- "longer"
#       } else {
#         compare_msg <- "shorter"
#       }
#       net_arg <- deparse(substitute(net_arg))
#       .stop(
#         "`edge_index` is `logical`, but {compare_msg} than the number of nodes in`{net_arg}`."
#       )
#     }
#   }
# }
# 
# 
# .collect_args <- function(.envir = parent.frame()) {
#   dots <- match.call(sys.function(which = -1L),
#                      sys.call(which = -1L),
#                      expand.dots = FALSE)$..
#   
#   c(as.list(parent.frame()), dots)
# }

get_graph_engine <- function() {
  getOption(x = "graph_engine", default = "native")
}



# test_foo <- function(direction = "in", node_index) {
#   # return(direction)
#   # dont_exist <- .map_lgl(.args[["which_args"]], exists)
#   # return(match.call())
#   print(.collect_args())
#   
#   .validate_args(.args = .collect_args())
#   # return(direction)
# }

























