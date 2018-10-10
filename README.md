
<!-- README.Rmd generates README.md. -->

# snatools <a href="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png"> <img src="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png" align="right" height="24%" width="24%" href="http://res.cloudinary.com/syknapptic/image/upload/v1537658876/logo_bnrvvg.png"/> </a>

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis-CI Build
Status](https://travis-ci.org/knapply/snatools.svg?branch=master)](https://travis-ci.org/knapply/snatools)
[![codecov](https://codecov.io/gh/knapply/snatools/branch/master/graph/badge.svg)](https://codecov.io/gh/knapply/snatools)

<!-- [![GitHub Downloads](https://img.shields.io/github/downloads/knapply/snatools/total.svg)](https://github.com/knapply/snatools) -->

## Overview

`snatools` is still *very* experimental; the current API and
dependencies are subject to change.

`snatools` has four immediate goals:

1.  Accurately bridge existing graph frameworks (i.e. `igraph` and
    `network`).
2.  Streamline network analytic workflows by making common tasks as
    intuitive (and stable) as possible.
      - Increase analyst efficiency.
      - Reduce the cognitive load of network newcomers and those
        instructing them.
3.  Implement metrics that are missing from R’s network ecosystem.
      - Reimplement those that *should* be made more accessible,
        efficient, or robust.
4.  Standardize fundamental tasks with methods that bypass the
    longstanding issues of `igraph` vs `network` namespace
conflicts.

<!-- Additionally, `snatools` seeks to prepare for the future by establishing the foundation (or a set of lessons-learned) for an enhanced network framework that: -->

<!-- 1. continues to be compatible with legacy frameworks. -->

<!-- 2. can be extended to take advantage of modern and future R's ecosystem . -->

<!--     + Optimization? `Rcpp`, C++'s Boost Graph Library, Python's graph-tool (heavily C++), CUDA -->

<!--     + Data Frame Efficiency? `data.table` -->

<!--     + Spatial Vector Data? `sf` -->

<!--     + Intuitive Code? `tidyverse`  -->

<!--         + `tidygraph` and `ggraph` may still be maturing, but they've rendered many legacy workflows for graph manipulation and plotting obsolete. -->

<!--     + Next Generation Tools? [Apache Arrow](https://arrow.apache.org/) is coming. -->

## Installation

``` r
# Install {devtools} if you haven't already.
# install.packages("devtools")
requireNamespace(devtools)

# Install {snatools} from GitHub.
devtools::install_github("knapply/snatools")

# Load {snatools}.
library(snatools)
```

<!-- ## Features -->

<!-- ```{r} -->

<!-- data("sampson_monastery") -->

<!-- ``` -->

<!-- ### `bridge_net` -->

<!-- The `bridge_net` object provides an intermediate graph structure that can effectively map data to both `igraph` and `network` objects. -->

<!-- ```{r} -->

<!-- sampson_monastery -->

<!-- ``` -->

<!-- ### Conversion -->

<!-- Through `bridge_net` objects, `igraph` and `network` objects can play together easier than ever. -->

<!-- ```{r} -->

<!-- ig <- as_igraph(sampson_monastery) -->

<!-- ``` -->

<!-- ```{r, echo=FALSE} -->

<!-- ig -->

<!-- ``` -->

<!-- ```{r} -->

<!-- nw <- as_network(sampson_monastery) -->

<!-- ``` -->

<!-- ```{r, echo=FALSE} -->

<!-- nw -->

<!-- ``` -->

<!-- ```{r} -->

<!-- as_network(ig) -->

<!-- ``` -->

<!-- ```{r} -->

<!-- as_igraph(nw) -->

<!-- ``` -->

<!-- #### `%==%` -->

<!-- Since `bridge_net`s are intermediate structures, comparing them is easy. `%==%` streamlines the process to make successful conversion confirmation as simple as possible. -->

<!-- ```{r} -->

<!-- all(ig %==% nw, -->

<!--     ig %==% sampson_monastery, -->

<!--     nw %==% sampson_monastery) -->

<!-- ``` -->

<!-- The only data that are not compared by `%==%` are non-structural attributes as `igraph` and `network` do not handle them in compatible ways. -->

<!-- ```{r} -->

<!-- ig %>% igraph::graph_attr_names() -->

<!-- nw %>% network::list.network.attributes() -->

<!-- ``` -->

<!-- ### Standardized Data Extraction -->

<!-- ```{r} -->

<!-- ig %>% vrt_to_df() -->

<!-- ``` -->

<!-- ```{r} -->

<!-- nw %>% edg_to_df() -->

<!-- ``` -->

<!-- ### Standardized Representations -->

<!-- ```{r} -->

<!-- ig %>% rep_as_edgelist() -->

<!-- ``` -->

<!-- ### Additional Tools -->

<!-- ```{r} -->

<!-- nw %>% rep_as_mixing_matrix(vrt_attr = "faction") -->

<!-- ``` -->

<!-- ### Easy Integeration with Modern Workflows -->

<!-- ```{r, message=FALSE} -->

<!-- library(tidygraph, warn.conflicts = FALSE) -->

<!-- library(ggraph, quietly = TRUE) -->

<!-- nw %>%  -->

<!--   as_igraph() %>%  -->

<!--   as_tbl_graph() %E>% -->

<!--   select(time, relation) %>%  -->

<!--   filter(time == 3) %>% -->

<!--   mutate(relation = factor(relation,  -->

<!--                            c("liking", "praise", "esteem", "positive influence", -->

<!--                              "disliking", "blame", "disesteem", "negative influence")) -->

<!--         ) %>%  -->

<!--   ggraph() + -->

<!--   geom_edge_fan(aes(colour = relation), width = 0.5, show.legend = FALSE) + -->

<!--   geom_node_point(aes(colour = faction)) + -->

<!--   guides(edge_colour = guide_legend(title = NULL), -->

<!--          colour = guide_legend(title = NULL)) + -->

<!--   facet_edges(~ relation, ncol = 4) + -->

<!--   theme_void() + -->

<!--   theme(strip.text = element_text(face = "bold", vjust = 1), -->

<!--         panel.border = element_rect(fill = NA, color = "lightgray"), -->

<!--         legend.position = "bottom") -->

<!-- ``` -->

<!-- # Development Tests -->

<!-- ```{r, message=FALSE} -->

<!-- # devtools::test() -->

<!-- ``` -->

<!-- ```{r} -->

<!-- covr::package_coverage() -->

<!-- ``` -->
