library(tidyverse)

# Network is supposed to be undirected, but is not symmetric and which triangle is
# correct is not noted; Using both

target_url <- "https://sites.google.com/site/ucinetsoftware/datasets/covert-networks/jemaahislamiyahkoschade/Jemaah%20Islamiyah%20Koschade%20CSV.zip?attredirects=0&d=1"

temp_file <- tempfile()
temp_dir <- tempdir()
ex_dir <- "inst/external-data"

download.file(target_url, destfile = temp_file, mode = "wb")

unzip(temp_file, exdir = temp_dir)

init <- dir(temp_dir, pattern = "CSV", full.names = TRUE) %>% 
  dir(full.names = TRUE) %>% 
  read_lines() %>% 
  str_split(",")

col_names <- init[[1]] %>% 
  discard(~ .x == '""')

row_names <- map_chr(init, `[[`, 1) %>% 
  map_chr(1) %>% 
  discard(~ .x == '""')

stopifnot(isTRUE(all.equal(col_names, row_names)))

adj_mat <- init %>% 
  map(~ discard(.x, ~ .x %in% c(row_names, '""'))) %>% 
  flatten_chr() %>% 
  as.integer() %>% 
  matrix(nrow = length(row_names), ncol = length(col_names), byrow = TRUE) %>% 
  `dimnames<-`(list(row_names, col_names))
  
adj_mat %>% 
  Matrix::isSymmetric() #> FALSE ... le_sigh

# sym_adj_mat <- adj_mat
# sym_adj_mat[] <- (sym_adj_mat + t(sym_adj_mat)) %/% 2

# sym_adj_mat %>% Matrix::isSymmetric() #> TRUE


jemaah_islamiyah_ig <- igraph::graph_from_adjacency_matrix(
  adjmatrix = adj_mat,
  mode = "upper",
  weighted = TRUE
)

el <- jemaah_islamiyah_ig %>% 
  igraph::as_edgelist(names = FALSE) %>% 
  cbind(igraph::edge_attr(jemaah_islamiyah_ig, "weight"))

jemaah_islamiyah_nw <- network::as.network.matrix(
  x = el,
  matrix.type = "edgelist",
  directed = FALSE, 
  hyper = FALSE,
  loops = FALSE,
  ignore.eval = FALSE,
  names.eval = "weight",
  multiple = FALSE,
  bipartite = FALSE
)

network::set.vertex.attribute(
  x = jemaah_islamiyah_nw, 
  attrname = "vertex.names",
  value = igraph::vertex_attr(jemaah_islamiyah_ig, name = "name")
)

stopifnot(
  COREsna:::.all_equal(
    COREsna:::edge_get_attr(jemaah_islamiyah_ig, "weight"),
    COREsna:::edge_get_attr(jemaah_islamiyah_nw, "weight")
  )
)

stopifnot(
  COREsna:::.all_equal(
    COREsna:::.fetch_edgelist.igraph(jemaah_islamiyah_ig),
    COREsna:::.fetch_edgelist.network(jemaah_islamiyah_nw)
  )
)

stopifnot(
  COREsna:::.all_equal(
    COREsna::node_get_names(jemaah_islamiyah_ig),
    COREsna::node_get_names(jemaah_islamiyah_nw)
  )
)


usethis::use_data(jemaah_islamiyah_ig, jemaah_islamiyah_nw)
