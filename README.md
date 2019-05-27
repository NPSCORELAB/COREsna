
<!-- README.Rmd generates README.md. -->

# COREsna

<!-- <a href=""> <img src="man/figures/corelogo.png" align="right"  height="27%" width="27%" href="https://github.com/NPSCORELAB/COREsna"/> </a> -->

<!-- badges: start -->

[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![GitHub last
commit](https://img.shields.io/github/last-commit/NPSCORELAB/COREsna.svg)](https://github.com/NPSCORELAB/COREsna/commits/master)
[![codecov](https://codecov.io/gh/NPSCORELAB/COREsna/branch/master/graph/badge.svg)](https://codecov.io/gh/NPSCORELAB/COREsna)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/NPSCORELAB/COREsna?branch=master&svg=true)](https://ci.appveyor.com/project/knapply/COREsna)
[![Travis-CI Build
Status](https://travis-ci.org/NPSCORELAB/COREsna.svg?branch=master)](https://travis-ci.org/NPSCORELAB/COREsna)

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Depends](https://img.shields.io/badge/Depends-GNU_R%3E=3.2-blue.svg)](https://www.r-project.org/)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/NPSCORELAB/COREsna.svg)](https://github.com/NPSCORELAB/COREsna)
[![HitCount](http://hits.dwyl.io/NPSCORELAB/COREsna.svg)](http://hits.dwyl.io/NPSCORELAB/COREsna)
<!-- badges: end -->

# Overview

# Installation

``` r
# install.packages("remotes")
remotes::install_github("NPSCORELAB/COREsna")
```

# Features

``` r
# TODO
```

# Syntax

<table class="table table-bordered" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

Action/Metric

</th>

<th style="text-align:left;">

Proposed Syntax

</th>

<th style="text-align:left;">

Related {statnet} Syntax

</th>

<th style="text-align:left;">

Related {igraph} Syntax

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

is network directed?

</td>

<td style="text-align:left;">

`net_is_directed()`

</td>

<td style="text-align:left;">

`network::is.directed()`

</td>

<td style="text-align:left;">

`igraph::is_directed()` <br> `igraph::is.directed()`

</td>

</tr>

<tr>

<td style="text-align:left;">

are a network’s edges weighted?

</td>

<td style="text-align:left;">

`net_is_weighted()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::is_weighted()` <br> `igraph::is.weighted()`

</td>

</tr>

<tr>

<td style="text-align:left;">

is a network multiplex?

</td>

<td style="text-align:left;">

`net_is_multiplex()`

</td>

<td style="text-align:left;">

`network::is.multiplex()`

</td>

<td style="text-align:left;">

`igraph::any_multiple()`

</td>

</tr>

<tr>

<td style="text-align:left;">

is a network bipartite?

</td>

<td style="text-align:left;">

`net_is_bipartite()`

</td>

<td style="text-align:left;">

`network::is.bipartite()`

</td>

<td style="text-align:left;">

`igraph::is_bipartite()` <br> `igraph::is.bipartite()`

</td>

</tr>

<tr>

<td style="text-align:left;">

does a network have isolates?

</td>

<td style="text-align:left;">

`net_has_isolates()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

does a network have loops?

</td>

<td style="text-align:left;">

`net_has_loops()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::which_loop()` <br> `igraph::is.loop()`

</td>

</tr>

<tr>

<td style="text-align:left;">

get node attribute

</td>

<td style="text-align:left;">

`node_get_attr()`

</td>

<td style="text-align:left;">

`network::get.vertex.attribute()` <br> `network::'%vattr%'()` <br>
`network::'%v%'()`

</td>

<td style="text-align:left;">

`igraph::V()` <br> `igraph::vertex_attr()` <br>
`igraph::get.vertex.attribute()` <br> `igraph::vertex.attributes()`

</td>

</tr>

<tr>

<td style="text-align:left;">

get node names

</td>

<td style="text-align:left;">

`node_get_names()`

</td>

<td style="text-align:left;">

`network::network.vertex.names()`

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

does a node attribute exist?

</td>

<td style="text-align:left;">

`node_attr_exists()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

get names of all node attributes

</td>

<td style="text-align:left;">

`node_get_attr_names()`

</td>

<td style="text-align:left;">

`network::list.vertex.attributes()`

</td>

<td style="text-align:left;">

`igraph::vertex_attr_names()` <br> `igraph::list.vertex.attributes()`

</td>

</tr>

<tr>

<td style="text-align:left;">

get edge attribute

</td>

<td style="text-align:left;">

`edge_get_attr()`

</td>

<td style="text-align:left;">

`network::get.edge.attribute()` <br> `network::'%eattr%'()` <br>
`network::'%e%'()`

</td>

<td style="text-align:left;">

`igraph::edge_attr()` <br> `igraph::get.edge.attribute()` <br>
`igraph::edge.attributes()`

</td>

</tr>

<tr>

<td style="text-align:left;">

does an edge attribute exist?

</td>

<td style="text-align:left;">

`edge_attr_exists()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

get names of all edge attributes

</td>

<td style="text-align:left;">

`edge_get_attr_names()`

</td>

<td style="text-align:left;">

`network::list.edge.attributes()`

</td>

<td style="text-align:left;">

`igraph::edge_attr_names()` <br> `igraph::list.edge.attributes()`

</td>

</tr>

<tr>

<td style="text-align:left;">

degree

</td>

<td style="text-align:left;">

`node_cent_degree()`

</td>

<td style="text-align:left;">

`sna::degree()`

</td>

<td style="text-align:left;">

`igraph::degree()`

</td>

</tr>

<tr>

<td style="text-align:left;">

closeness

</td>

<td style="text-align:left;">

`node_cent_closeness()`

</td>

<td style="text-align:left;">

`sna::closeness()`

</td>

<td style="text-align:left;">

`igraph::closeness()`

</td>

</tr>

<tr>

<td style="text-align:left;">

betweenness

</td>

<td style="text-align:left;">

`node_cent_betweenness()`

</td>

<td style="text-align:left;">

`sna::betweenness()`

</td>

<td style="text-align:left;">

`igraph::betweenness()`

</td>

</tr>

<tr>

<td style="text-align:left;">

eigenvector

</td>

<td style="text-align:left;">

`node_cent_eigenvector()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::eigen_centrality()$vector` <br> `igraph::evcent()$vector`

</td>

</tr>

<tr>

<td style="text-align:left;">

average reciprocal distance

</td>

<td style="text-align:left;">

`node_cent_avg_reciprocal_distance()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

order

</td>

<td style="text-align:left;">

`topo_order()`

</td>

<td style="text-align:left;">

`network::network.size()`

</td>

<td style="text-align:left;">

`igraph::vcount()` <br> `igraph::gorder()`

</td>

</tr>

<tr>

<td style="text-align:left;">

size

</td>

<td style="text-align:left;">

`topo_size()`

</td>

<td style="text-align:left;">

`network::network.edgecount()`

</td>

<td style="text-align:left;">

`igraph::ecount()`

</td>

</tr>

<tr>

<td style="text-align:left;">

EI index

</td>

<td style="text-align:left;">

`topo_ei_index()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

density

</td>

<td style="text-align:left;">

`topo_density()`

</td>

<td style="text-align:left;">

`sna::gden()`

</td>

<td style="text-align:left;">

`igraph::edge_density()` <br> `igraph::graph.density()`

</td>

</tr>

<tr>

<td style="text-align:left;">

average degree

</td>

<td style="text-align:left;">

`topo_average_degree()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

average distance

</td>

<td style="text-align:left;">

`topo_average_distance()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::mean_distance()` <br> `igraph::average.path.length()`

</td>

</tr>

<tr>

<td style="text-align:left;">

diameter

</td>

<td style="text-align:left;">

`topo_diameter()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::diameter()` <br> `igraph::get_diameter()` <br>
`igraph::get.diameter()`

</td>

</tr>

<tr>

<td style="text-align:left;">

centralization

</td>

<td style="text-align:left;">

`topo_centralization()`

</td>

<td style="text-align:left;">

`sna::centralization()`

</td>

<td style="text-align:left;">

`igraph::centr_*()` <br> `igraph::centr_*_tmax()` <br>
`igraph::centralization.*()` <br> `igraph::centralization.*.tmax()`

</td>

</tr>

<tr>

<td style="text-align:left;">

cohesion

</td>

<td style="text-align:left;">

`topo_cohesion()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

`igraph::cohesion()`

</td>

</tr>

<tr>

<td style="text-align:left;">

compactness

</td>

<td style="text-align:left;">

`topo_compactness()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

fragmentation

</td>

<td style="text-align:left;">

`topo_fragmentation()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

reciprocity

</td>

<td style="text-align:left;">

`topo_reciprocity()`

</td>

<td style="text-align:left;">

`sna::grecip()`

</td>

<td style="text-align:left;">

`igraph::reciprocity()`

</td>

</tr>

<tr>

<td style="text-align:left;">

transitivity

</td>

<td style="text-align:left;">

`topo_transitivity()`

</td>

<td style="text-align:left;">

`sna::gtrans()`

</td>

<td style="text-align:left;">

`igraph::transitivity()`

</td>

</tr>

<tr>

<td style="text-align:left;">

Moran’s I

</td>

<td style="text-align:left;">

`topo_morans_i()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Gerry’s C

</td>

<td style="text-align:left;">

`topo_gerrys_c()`

</td>

<td style="text-align:left;">

</td>

<td style="text-align:left;">

</td>

</tr>

</tbody>

</table>

<!-- # Developer Notes -->

<!-- * Building {`pkgdown`} Site -->

<!-- ```{r, eval=FALSE} -->

<!-- pkgdown::build_site() -->

<!-- ``` -->

<!-- ```{r, echo=FALSE, eval=FALSE} -->

<!-- pkgnet::CreatePackageVignette() -->

<!-- ``` -->
