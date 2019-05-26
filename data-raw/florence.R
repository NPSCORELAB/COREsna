# pajek files...
# igraph fails at the C level
# network silently ignores vertex attributes
# ... le sigh

# target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/WaFa/Padgett.paj"

dest_file <- "inst/external-data/Padgett.paj"

# download.file(target_url, destfile = dest_file)

library(stringr)
library(purrr)
library(dplyr)


init_dat <- readr::read_lines(dest_file) %>% 
  str_squish() %>% 
  discard(~ nchar(.x) == 0L) %>% 
  str_split("^(?=\\*)") %>% 
  map(~ discard(.x, ~ nchar(.x) == 0L)) %>% 
  str_flatten(collapse = " ") %>% 
  str_split(".(?=\\*)") %>% 
  unlist()

get_net_name <- function(.x) {
  out <- str_extract(.x, "(?<=\\*Network ).*$")
  out[!is.na(out)]
}

get_nodes <- function(.x) {
  node_num <- str_extract(.x, "(?<=\\*Vertices )\\d+(?= )") %>% 
    discard(is.na)
  
  nodes <- .x %>% 
    str_extract(glue::glue("(?<=\\*Vertices {node_num} ).*$")) %>% 
    discard(is.na)
  
  node_names <- nodes %>%
    str_extract_all('(?<=(\\s|^)").*?(?=")') %>% 
    unlist() %>% 
    tibble(node_names = .)
  
  node_attr_name_lines <- str_which(.x, "^\\*Vector")
  node_attr_names <- .x[node_attr_name_lines] %>% 
    str_remove("^\\*Vector ")
  
  node_attrs <- .x[node_attr_name_lines + 1L] %>% 
    str_remove("^\\*Vertices ") %>% 
    str_split(" ") %>% 
    map(as.integer) %>% 
    map(~ .x[-1L]) %>% 
    set_names(node_attr_names)
  
  bind_cols(node_names, node_attrs) %>% 
    rename_all(~ str_to_lower(str_remove_all(., "[:punct:]"))) %>% 
    rename(node_name = nodenames) %>% 
    select(-ties) %>%                                           # meaningless as is... it's a multiplex network
    mutate(priors = if_else(priors == -1, NA_integer_, priors)) # missing data looks to be coded as `-1`
}

get_edges <- function(.x) {
  edge_lines <- str_which(.x, "\\*Arcs")
  
  edge_types <- .x[edge_lines] %>% 
    str_extract('(?<=(\\s|^)").*?(?=")')
  
  .x[edge_lines] %>% 
    str_remove('^.*?".*?"(?= \\d)') %>% 
    str_trim() %>% 
    str_split(" ") %>% 
    map(as.integer) %>% 
    map(matrix, ncol = 3, byrow = TRUE) %>% 
    map(`colnames<-`, c("source", "target", "edge_type")) %>% 
    map(as_tibble) %>% 
    map2_dfr(edge_types, ~ mutate(.x, edge_type = str_to_lower(.y)))
}

get_direction <- function(.x) {
  any(str_detect(.x, "Arcs"))
}

net_primitive <- list(
  metadata = list(
    directed = get_direction(init_dat),
    name = get_net_name(init_dat)
  ),
  nodes = get_nodes(init_dat),
  edges = get_edges(init_dat) %>% 
    mutate_at(vars(source, target), ~ recode(., !!!get_nodes(init_dat)$node_name))
)


network_from_data_frame <- function(.edges, .nodes, .directed, .multiplex = FALSE) {
  out <- network::network.initialize(
    n = nrow(.nodes), 
    directed = .directed,
    loops = FALSE,
    bipartite = FALSE, 
    multiple = .multiplex
  )
  
  # seriously?
  edge_attr_names <- names(.edges)[!names(.edges) %in% c("source", "target")] %>% 
    replicate(n = nrow(.edges), expr = ., simplify = FALSE)
  
  # seriously???
  edge_attr_vals <- .edges[ , !names(.edges) %in% c("source", "target"), drop = FALSE ] %>% 
    as.list() %>% 
    map(map, list) %>% 
    flatten()

  sources <- as.integer(
    factor(.edges$source, levels = forcats::as_factor(.nodes$node_name))
  )
  targets <- as.integer(
    factor(.edges$target, levels = forcats::as_factor(.nodes$node_name))
  )

  network::add.edges(
    x = out, 
    tail = sources,
    head = targets,
    names.eval = edge_attr_names,
    vals.eval = edge_attr_vals
  )

  .nodes <- .nodes %>% 
    rename(vertex.names = node_name) %>% 
    as.list()

  network::set.vertex.attribute(
    x = out, 
    attrname = names(.nodes), 
    value = .nodes
  )
  
  out
}


florence_marriage_nw <- network_from_data_frame(
  .edges = net_primitive$edges %>% filter(edge_type == "marital"),
  .nodes = net_primitive$nodes,
  .directed = net_primitive$metadata$directed
)

florence_business_nw <- network_from_data_frame(
  .edges = net_primitive$edges %>% filter(edge_type == "business"),
  .nodes = net_primitive$nodes,
  .directed = net_primitive$metadata$directed
)

florence_combo_nw <- network_from_data_frame(
  .edges = net_primitive$edges,
  .nodes = net_primitive$nodes,
  .directed = net_primitive$metadata$directed,
  .multiplex = TRUE
)


florence_marriage_ig <- igraph::graph_from_data_frame(
  d = net_primitive$edges %>% filter(edge_type == "marital"),
  directed = net_primitive$metadata$directed,
  vertices = net_primitive$nodes
)

florence_business_ig <- igraph::graph_from_data_frame(
  d = net_primitive$edges %>% filter(edge_type == "business"),
  directed = net_primitive$metadata$directed,
  vertices = net_primitive$nodes
)

florence_combo_ig <- igraph::graph_from_data_frame(
  d = net_primitive$edges,
  directed = net_primitive$metadata$directed,
  vertices = net_primitive$nodes
)

stopifnot(
  COREsna:::.all_equal(
    COREsna:::.fetch_edgelist.igraph(florence_combo_ig),
    COREsna:::.fetch_edgelist.network(florence_combo_nw)
  )
)

stopifnot(
  COREsna:::.all_equal(
    COREsna:::.fetch_edgelist.igraph(florence_marriage_ig),
    COREsna:::.fetch_edgelist.network(florence_marriage_nw)
  )
)

stopifnot(
  COREsna:::.all_equal(
    COREsna:::.fetch_edgelist.igraph(florence_business_ig),
    COREsna:::.fetch_edgelist.network(florence_business_nw)
  )
)

usethis::use_data(
  florence_business_ig, florence_marriage_ig, florence_combo_ig,
  florence_business_nw, florence_marriage_nw, florence_combo_nw
)














# nw <- network::read.paj(dest_file, verbose = TRUE, debug = TRUE)
# 
# library(dplyr)
# library(purrr)
# 
# 
# tibble::as_tibble(nw$networks$Business) %>% 
#   select(-.eid) %>% 
#   rename(source = .tail, target = .head)
# tibble::as_tibble(nw$networks$Marital)
# 
# prep_edges <- function(.g, .relation) {
#   .g %>% 
#     as_tibble() %>% 
#     select(-.eid) %>% 
#     rename(source = .tail, target = .head) %>% 
#     mutate(relation = .relation)
# }
# 
# prep_nodes <- function(.g, .relation) {
#   .g %>% 
#     as_tibble(unit = "vertices") %>% 
#     select(vertex.names) %>% 
#     rename(node_name = vertex.names) %>% 
#     mutate(network = .relation)
# }
# 
# net_primitive <- list(
#   metadata = list(
#     directed = unique(map_lgl(nw$networks, network::is.directed)),
#     n_nodes = unique(map_dbl(nw$networks, network::network.size)) %>%as.integer()
#   ),
#   edges = imap(nw$networks, prep_edges) %>% 
#     map_dfr(mutate, relation = tolower(relation))
#   ,
#   nodes = imap_dfr(nw$networks, prep_nodes) %>%
#     select(-network) %>% distinct()
# )
# 
# 
# florence_marriage_nw <- nw$networks$Marital 
# network::delete.edge.attribute(
#   florence_marriage_nw, attrname = "Marital"
# )
# network::set.edge.attribute(
#   florence_marriage_nw, attrname = "edge_type", value = "marital"
# )
# 
# 
# florence_business_nw <- nw$networks$Business
# network::delete.edge.attribute(
#   florence_business_nw, attrname = "Business"
# )
# network::set.edge.attribute(
#   florence_business_nw, attrname = "edge_type", value = "business"
# )
# 
# florence_combo_nw <- network::network.initialize(
#   n = network::network.size(florence_marriage_nw),
#   directed = network::is.directed(florence_marriage_nw),
#   loops = network::has.loops(florence_business_nw),
#   hyper = FALSE,
#   multiple = TRUE,
#   bipartite = FALSE
# )
# 
# network::add.edges(
#   florence_combo_nw, 
#   tail = net_primitive$edges$source,
#   head = net_primitive$edges$target,
#   names.eval = rep(list("edge_type"), nrow(net_primitive$edges)),
#   vals.eval = as.list(net_primitive$edges$relation)
# )
# 
# network::set.vertex.attribute(
#   florence_combo_nw, attrname = "vertex.names", 
#   value = net_primitive$nodes$node_name
# )
# 
# 
# florence_business_ig <- igraph::graph_from_data_frame(
#   d = net_primitive$edges %>% 
#     filter(relation == "business") %>% 
#     mutate_at(vars(source, target), ~ recode(., !!!net_primitive$nodes$node_name)),
#   directed = TRUE,
#   vertices = net_primitive$nodes
# )
# 
# stopifnot(
#   COREsna:::.all_equal(
#     COREsna:::.fetch_edgelist.igraph(florence_business_ig),
#     COREsna:::.fetch_edgelist.network(florence_business_nw)
#   )
# )
# 
# florence_marriage_ig <- igraph::graph_from_data_frame(
#   d = net_primitive$edges %>% 
#     filter(relation == "marital") %>% 
#     mutate_at(vars(source, target), ~ recode(., !!!net_primitive$nodes$node_name)),
#   directed = TRUE,
#   vertices = net_primitive$nodes
# )
# 
# stopifnot(
#   COREsna:::.all_equal(
#     COREsna:::.fetch_edgelist.igraph(florence_marriage_ig),
#     COREsna:::.fetch_edgelist.network(florence_marriage_nw)
#   )
# )
# 
# florence_combo_ig <- igraph::graph_from_data_frame(
#   d = net_primitive$edges %>% 
#     filter(relation == "marital") %>% 
#     mutate_at(vars(source, target), ~ recode(., !!!net_primitive$nodes$node_name)),
#   directed = TRUE,
#   vertices = net_primitive$nodes
# )
# 
# 
# 
# 
# 
# 
# 
# florence_business_nw
# florence_marriage_nw
# florence_combo_nw