.onLoad <- function(libname, pkgname) {
  # options("graph_engine" = "native")
  # if ("package:igraph" %in% search()) {
  #   assign("edge_attr_names", value = COREsna::edge_attr_names,
  #          envir = globalenv())
  # }
}
 
.onAttach <- function(libname, pkgname) {
  # options("graph_engine" = "native")
  # setHook(packageEvent("igraph", "attach"), function(...) {
  #   assign("edge_attr_names", value = COREsna::edge_attr_names,
  #          envir = globalenv())
  # })
}