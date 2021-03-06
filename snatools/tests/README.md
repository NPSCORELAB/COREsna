Tests and Coverage
================
10 January, 2019 09:25:01

This output is created by
[covrpage](https://github.com/metrumresearchgroup/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                            | Coverage (%) |
| :------------------------------------------------ | :----------: |
| snatools                                          |    72.47     |
| [R/adj\_list.R](../R/adj_list.R)                  |     0.00     |
| [R/adj\_matrix.R](../R/adj_matrix.R)              |     0.00     |
| [R/ei\_index.R](../R/ei_index.R)                  |     0.00     |
| [R/ggplot.R](../R/ggplot.R)                       |     0.00     |
| [R/mix\_matrix.R](../R/mix_matrix.R)              |     0.00     |
| [R/metadata.R](../R/metadata.R)                   |    75.71     |
| [R/utils.R](../R/utils.R)                         |    79.22     |
| [R/subset.R](../R/subset.R)                       |    94.87     |
| [R/comparison.R](../R/comparison.R)               |    97.30     |
| [R/as\_bridge\_net.R](../R/as_bridge_net.R)       |    100.00    |
| [R/as\_igraph.R](../R/as_igraph.R)                |    100.00    |
| [R/as\_network.R](../R/as_network.R)              |    100.00    |
| [R/dataframe.R](../R/dataframe.R)                 |    100.00    |
| [R/edge-attributes.R](../R/edge-attributes.R)     |    100.00    |
| [R/edgelist.R](../R/edgelist.R)                   |    100.00    |
| [R/vertex-attributes.R](../R/vertex-attributes.R) |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat)
package.

| file                                                          |  n |  time | error | failed | skipped | warning |
| :------------------------------------------------------------ | -: | ----: | ----: | -----: | ------: | ------: |
| [test-conversion.R](testthat/test-conversion.R)               | 15 | 0.347 |     0 |      0 |       0 |       0 |
| [test-dataframe.R](testthat/test-dataframe.R)                 | 56 | 0.148 |     0 |      0 |       0 |       0 |
| [test-edge-attributes.R](testthat/test-edge-attributes.R)     | 12 | 0.013 |     0 |      0 |       0 |       0 |
| [test-edgelist.R](testthat/test-edgelist.R)                   | 27 | 0.032 |     0 |      0 |       0 |       0 |
| [test-metadata.R](testthat/test-metadata.R)                   | 78 | 0.111 |     0 |      0 |       0 |       0 |
| [test-subset.R](testthat/test-subset.R)                       |  6 | 0.043 |     0 |      0 |       0 |       0 |
| [test-utils.R](testthat/test-utils.R)                         |  9 | 0.013 |     0 |      0 |       0 |       0 |
| [test-vertex-attributes.R](testthat/test-vertex-attributes.R) | 12 | 0.017 |     0 |      0 |       0 |       0 |

<details closed>

<summary> Show Detailed Test Results
</summary>

| file                                                                  | context                                   | test                                                 | status |  n |  time |
| :-------------------------------------------------------------------- | :---------------------------------------- | :--------------------------------------------------- | :----- | -: | ----: |
| [test-conversion.R](testthat/test-conversion.R#L15_L17)               | graph conversion                          | network objects convert to igraph correctly          | PASS   | 10 | 0.183 |
| [test-conversion.R](testthat/test-conversion.R#L89_L91)               | graph conversion                          | igraph objects convert to network correctly          | PASS   |  4 | 0.123 |
| [test-conversion.R](testthat/test-conversion.R#L107_L109)             | graph conversion                          | nested attributes and networkDynamic objects work    | PASS   |  1 | 0.041 |
| [test-dataframe.R](testthat/test-dataframe.R#L67_L70)                 | vertex/edge data frame construction works | vertex data frame construction works                 | PASS   |  6 | 0.018 |
| [test-dataframe.R](testthat/test-dataframe.R#L104_L107)               | vertex/edge data frame construction works | edge data frame construction works                   | PASS   |  6 | 0.035 |
| [test-dataframe.R](testthat/test-dataframe.R#L223_L226)               | vertex/edge data frame construction works | bipartite vertex data frame construction works       | PASS   |  4 | 0.009 |
| [test-dataframe.R](testthat/test-dataframe.R#L248_L251)               | vertex/edge data frame construction works | bipartite edge data frame construction works         | PASS   |  3 | 0.007 |
| [test-dataframe.R](testthat/test-dataframe.R#L282_L285)               | vertex/edge data frame construction works | handling 0 optional vertex columns works as expected | PASS   |  4 | 0.007 |
| [test-dataframe.R](testthat/test-dataframe.R#L313_L316)               | vertex/edge data frame construction works | vrt\_dfs from igraphs missing names work as expected | PASS   |  2 | 0.005 |
| [test-dataframe.R](testthat/test-dataframe.R#L343_L346)               | vertex/edge data frame construction works | handling 0 optional edge columns works as expected   | PASS   |  4 | 0.007 |
| [test-dataframe.R](testthat/test-dataframe.R#L369)                    | vertex/edge data frame construction works | nested attributes and networkDynamic objects work    | PASS   | 11 | 0.036 |
| [test-dataframe.R](testthat/test-dataframe.R#L385)                    | vertex/edge data frame construction works | empty graphs works                                   | PASS   | 16 | 0.024 |
| [test-edge-attributes.R](testthat/test-edge-attributes.R#L29_L32)     | edge attributes                           | attribute name extraction works                      | PASS   |  3 | 0.002 |
| [test-edge-attributes.R](testthat/test-edge-attributes.R#L48_L51)     | edge attributes                           | specific attribute extraction works                  | PASS   |  6 | 0.009 |
| [test-edge-attributes.R](testthat/test-edge-attributes.R#L85_L87)     | edge attributes                           | empty attributes return NULL                         | PASS   |  3 | 0.002 |
| [test-edgelist.R](testthat/test-edgelist.R#L92_L95)                   | edgelist construction works               | raw edgelists build properly                         | PASS   | 12 | 0.016 |
| [test-edgelist.R](testthat/test-edgelist.R#L153_L156)                 | edgelist construction works               | edgelist objects: use\_names = TRUE                  | PASS   |  6 | 0.007 |
| [test-edgelist.R](testthat/test-edgelist.R#L190_L193)                 | edgelist construction works               | edgelist objects: vrt\_attr = ‘foo’                  | PASS   |  6 | 0.006 |
| [test-edgelist.R](testthat/test-edgelist.R#L285_L288)                 | edgelist construction works               | raw bipartite edgelists build properly               | PASS   |  3 | 0.003 |
| [test-metadata.R](testthat/test-metadata.R#L58_L60)                   | metadata queries                          | net\_is\_directed is correct                         | PASS   |  6 | 0.005 |
| [test-metadata.R](testthat/test-metadata.R#L89_L91)                   | metadata queries                          | net\_is\_multiplex is correct                        | PASS   | 12 | 0.015 |
| [test-metadata.R](testthat/test-metadata.R#L140_L142)                 | metadata queries                          | net\_has\_loops is correct                           | PASS   | 12 | 0.011 |
| [test-metadata.R](testthat/test-metadata.R#L191_L193)                 | metadata queries                          | net\_has\_isolates is correct                        | PASS   | 12 | 0.011 |
| [test-metadata.R](testthat/test-metadata.R#L250_L253)                 | metadata queries                          | net\_count\_vertices is correct                      | PASS   | 12 | 0.011 |
| [test-metadata.R](testthat/test-metadata.R#L315_L318)                 | metadata queries                          | net\_count\_edges is correct                         | PASS   | 12 | 0.011 |
| [test-metadata.R](testthat/test-metadata.R#L389_L391)                 | metadata queries                          | net\_is\_bipartite works                             | PASS   |  6 | 0.039 |
| [test-metadata.R](testthat/test-metadata.R#L415_L418)                 | metadata queries                          | net\_count\_actors works                             | PASS   |  6 | 0.008 |
| [test-subset.R](testthat/test-subset.R#L18_L20)                       | subsetting edges and vertices             | edg\_subset() works                                  | PASS   |  4 | 0.028 |
| [test-subset.R](testthat/test-subset.R#L60_L62)                       | subsetting edges and vertices             | vrt\_subset() works                                  | PASS   |  2 | 0.015 |
| [test-utils.R](testthat/test-utils.R#L4_L6)                           | internal utility functions                | utils work as expected                               | PASS   |  9 | 0.013 |
| [test-vertex-attributes.R](testthat/test-vertex-attributes.R#L27_L30) | vertex attributes                         | attribute name extraction works                      | PASS   |  2 | 0.002 |
| [test-vertex-attributes.R](testthat/test-vertex-attributes.R#L41_L44) | vertex attributes                         | specific attribute extraction works                  | PASS   |  4 | 0.007 |
| [test-vertex-attributes.R](testthat/test-vertex-attributes.R#L61_L64) | vertex attributes                         | vertex name extraction works                         | PASS   |  2 | 0.002 |
| [test-vertex-attributes.R](testthat/test-vertex-attributes.R#L83_L85) | vertex attributes                         | empty attributes return NULL                         | PASS   |  4 | 0.006 |

</details>

<details>

<summary> Session Info </summary>

| Field    | Value                         |
| :------- | :---------------------------- |
| Version  | R version 3.5.2 (2018-12-20)  |
| Platform | x86\_64-pc-linux-gnu (64-bit) |
| Running  | Ubuntu 18.04.1 LTS            |
| Language | en\_US                        |
| Timezone | America/Los\_Angeles          |

| Package  | Version |
| :------- | :------ |
| testthat | 2.0.1   |
| covr     | 3.2.1   |
| covrpage | 0.0.69  |

</details>

<!--- Final Status : pass --->
