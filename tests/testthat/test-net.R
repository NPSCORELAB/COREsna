test_that("net_is_directed() works", {
  expect_true(
    net_is_directed(build_test_ig(.directed = TRUE))
  )
  expect_true(
    net_is_directed(build_test_nw(.directed = TRUE))
  )

  expect_false(
    net_is_directed(build_test_ig(.directed = FALSE))
  )
  expect_false(
    net_is_directed(build_test_nw(.directed = FALSE))
  )

  # keep bipartite nets undirected
  expect_false(
    net_is_directed(build_test_ig(.directed = TRUE, .bipartite = TRUE))
  )
  expect_false(
    net_is_directed(build_test_nw(.directed = TRUE, .bipartite = TRUE))
  )
})

test_that("net_is_multiplex() works", {
  expect_true(
    net_is_multiplex(florence_combo_ig)
  )
  expect_true(
    net_is_multiplex(florence_combo_nw)
  )
  
  expect_false(
    net_is_multiplex(build_test_ig(.directed = FALSE))
  )
  expect_false(
    net_is_multiplex(build_test_nw(.directed = FALSE))
  )

  expect_false(
    net_is_multiplex(florence_business_ig)
  )
  expect_false(
    net_is_multiplex(florence_business_nw)
  )
})

test_that("net_has_loops() works", {
  expect_true(
    net_has_loops(build_test_ig(.diag = TRUE))
  )
  expect_true(
    net_has_loops(build_test_nw(.diag = TRUE))
  )
  
  expect_false(
    net_has_loops(build_test_ig(.diag = FALSE))
  )
  expect_false(
    net_has_loops(build_test_nw(.diag = FALSE))
  )
})

test_that("net_has_isolates() works", {
  expect_true(
    net_has_isolates(build_test_ig(.isolates = TRUE))
  )
  expect_true(
    net_has_isolates(build_test_nw(.isolates = TRUE))
  )
  
  expect_false(
    net_has_isolates(build_test_ig(.isolates = FALSE))
  )
  expect_false(
    net_has_isolates(build_test_nw(.isolates = FALSE))
  )
})

test_that("net_is_weighted() works", {
  expect_true(
    net_is_weighted(build_test_ig(.weighted = TRUE))
  )
  expect_true(
    net_is_weighted(build_test_nw(.weighted = TRUE))
  )
  
  expect_false(
    net_is_weighted(build_test_ig(.weighted = FALSE))
  )
  expect_false(
    net_is_weighted(build_test_nw(.weighted = FALSE))
  )
})

test_that("net_is_bipartite() works", {
  expect_true(
    net_is_bipartite(build_test_ig(.bipartite = TRUE))
  )
  expect_true(
    net_is_bipartite(build_test_nw(.bipartite = TRUE))
  )
  
  expect_false(
    net_is_bipartite(build_test_ig(.bipartite = FALSE))
  )
  expect_false(
    net_is_bipartite(build_test_nw(.bipartite = FALSE))
  )
})

