# library(tidyverse)
# 
# target_url <- "http://vlado.fmf.uni-lj.si/pub/networks/data/WaFa/Padgett.paj"
# 
# dest_file <- "inst/external-data/Padgett.paj"
# 
# download.file(target_url, destfile = dest_file)
# 
# raw <- read_lines(dest_file)
# 
# # vertex attributes
# vert_names_start <- str_which(raw, "\\*Vertices\\s+16")[[1]] + 1
# vert_names_end <- str_which(raw, '\\*Arcs :1 "Business"') - 1
# 
# vert_names <- raw[vert_names_start:vert_names_end] %>%
#   as_tibble() %>% 
#   filter(nchar(value) > 0L) %>% 
#   separate(value, c(".vrt_id", "name", "coords"), sep = "\"") %>% 
#   select(-coords) %>% 
#   mutate_all(str_trim) #%>% 
#   # mutate(.vrt_id = as.integer(.vrt_id))
# 
# vert_wealth_start <- str_which(raw, '\\*Vertices\\s+16')[[2]] + 1
# vert_wealth_end <- str_which(raw, '\\*Vector #Priors') - 1
# 
# vert_wealth <- raw[vert_wealth_start:vert_wealth_end] %>%
#   as_tibble() %>% 
#   filter(nchar(value) > 0L) %>% 
#   mutate(value = value %>% 
#            str_trim() %>% 
#            as.integer()
#   ) %>% 
#   rename(wealth = value)
# 
# vert_prior_start <- str_which(raw, '\\*Vertices\\s+16')[[3]] + 1
# vert_prior_end <- str_which(raw, '\\*Vector #Ties') - 1
# 
# vert_priors <- raw[vert_prior_start:vert_prior_end] %>%
#   as_tibble() %>% 
#   filter(nchar(value) > 0L) %>% 
#   mutate(value = value %>% 
#            str_trim() %>% 
#            as.integer()
#   ) %>% 
#   rename(priorate_seats = value) %>% 
#   mutate(priorate_seats = if_else(priorate_seats < 0L, NA_integer_, priorate_seats))
# 
# verts <- bind_cols(vert_names, vert_wealth, vert_priors)
# 
# build_node_df <- function(.raw) {}
# 
# 
# # edges
# business_start <- str_which(raw, '\\*Arcs :1 "Business"') + 1
# business_end <- str_which(raw, '\\*Arcs :2 "Marital"') - 1
# 
# build_edge_df <- function(.raw, .relation, .relation_end) {
#   line_start <- str_which(.raw, sprintf('\\*Arcs :\\d+ "%s', .relation)) + 1L
#   line_end <- str_which(.raw, sprintf('\\*Arcs :\\d+ "%s"', .relation_end)) - 1L
#   .raw[line_start:line_end] %>% 
#     as_tibble() %>% 
#     filter(nchar(value) > 0L) %>% 
#     mutate(value = str_trim(value)) %>% 
#     separate(value, c("source", "target", "weight"), sep = "\\s+") %>% 
#     mutate_all(as.integer) %>% 
#     select(-weight) %>% 
#     mutate(relation = str_to_lower(.relation))
# }
# 
# build_edge_df(raw, .relation = "Business", .relation_end =  "Marital")
# build_edge_df(raw, "Marital", "")
# 
# 
# business <- raw[business_start:business_end] %>%
#   as_tibble() %>% 
#   filter(nchar(value) > 0L) %>% 
#   mutate(value = str_trim(value)) %>% 
#   separate(value, c(".ego", ".alter", "weight"), sep = "\\s+") %>% 
#   mutate_all(as.integer) %>% 
#   select(-weight) %>% 
#   mutate(relation = "business")
# 
# marriage_start <- str_which(raw, '\\*Arcs :2 "Marital"') + 1
# marriage_end <- str_which(raw, '\\*Vector Wealth') - 1
# 
# marriage <- raw[marriage_start:marriage_end] %>%
#   as_tibble() %>% 
#   filter(nchar(value) > 0L) %>% 
#   mutate(value = str_trim(value)) %>% 
#   separate(value, c("source", "target", "weight"), sep = "\\s+") %>% 
#   mutate_all(as.integer) %>% 
#   select(-weight) %>% 
#   mutate(relation = "marriage")
# 
# edges <- bind_rows(business, marriage) %>%
#   mutate(dyad = map2(.ego, .alter, ~ sort(c(.x, .y)))) %>% 
#   mutate(.ego = map_int(dyad, 1),
#          .alter = map_int(dyad, 2)) %>% 
#   select(-dyad) %>% 
#   distinct()
# 
# florence <- igraph::graph_from_data_frame(edges, directed = FALSE, vertices = verts) %>% 
#   snatools:::as_bridge_net() %>% 
#   snatools:::as_igraph.bridge_net()
# 
# usethis::use_data(florence)