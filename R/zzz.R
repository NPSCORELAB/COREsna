# .onLoad <- function(libname, pkgname) {
#   if ("package:igraph" %in% search()) {
#     assign("edge_attr_names", value = COREsna::edge_attr_names, 
#            envir = globalenv())
#   }
# }
# 
# .onAttach <- function(libname, pkgname) {
#   setHook(packageEvent("igraph", "attach"), function(...) {
#     assign("edge_attr_names", value = COREsna::edge_attr_names, 
#            envir = globalenv())
#   })
# }