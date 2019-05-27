
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

<table class="table" style="margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

category

</th>

<th style="text-align:left;">

prefix

</th>

<th style="text-align:left;">

metric

</th>

<th style="text-align:left;">

syntax

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

order

</td>

<td style="text-align:left;">

`topo_order()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

size

</td>

<td style="text-align:left;">

`topo_size()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

EI index

</td>

<td style="text-align:left;">

`topo_ei_index()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

density

</td>

<td style="text-align:left;">

`topo_density()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

average degree

</td>

<td style="text-align:left;">

`topo_average_degree()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

average distance

</td>

<td style="text-align:left;">

`topo_average_distance()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

diameter

</td>

<td style="text-align:left;">

`topo_diameter()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

centralization

</td>

<td style="text-align:left;">

`topo_centralization()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

cohesion

</td>

<td style="text-align:left;">

`topo_cohesion()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

compactness

</td>

<td style="text-align:left;">

`topo_compactness()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

fragmentation

</td>

<td style="text-align:left;">

`topo_fragmentation()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

reciprocity

</td>

<td style="text-align:left;">

`topo_reciprocity()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

transitivity

</td>

<td style="text-align:left;">

`topo_transitivity()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

Moran’s I

</td>

<td style="text-align:left;">

`topo_morans_i()`

</td>

</tr>

<tr>

<td style="text-align:left;">

topography

</td>

<td style="text-align:left;">

topo

</td>

<td style="text-align:left;">

Gerry’s C

</td>

<td style="text-align:left;">

`topo_gerrys_c()`

</td>

</tr>

</tbody>

</table>

# Developer Notes

  - Building {`pkgdown`} Site

<!-- end list -->

``` r
pkgdown::build_site()
```
