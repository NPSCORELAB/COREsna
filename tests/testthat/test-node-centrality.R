test_that("degree works", {
  expect_equal(
    cent_degree(build_test_ig(), direction = "in"),
    cent_degree(build_test_nw(), direction = "in")
  )
  expect_equal(
    cent_degree(build_test_ig(), direction = "out"),
    cent_degree(build_test_nw(), direction = "out")
  )
  expect_equal(
    cent_degree(build_test_ig(), direction = "all"),
    cent_degree(build_test_nw(), direction = "all")
  )
  
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE), direction = "in"),
    cent_degree(build_test_nw(.directed = FALSE), direction = "in")
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE), direction = "out"),
    cent_degree(build_test_nw(.directed = FALSE), direction = "out")
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE), direction = "all"),
    cent_degree(build_test_nw(.directed = FALSE), direction = "all")
  )
  
  expect_equal(
    cent_degree(build_test_ig(.diag = TRUE), direction = "in", loops = TRUE),
    cent_degree(build_test_nw(.diag = TRUE), direction = "in", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.diag = TRUE), direction = "out", loops = TRUE),
    cent_degree(build_test_nw(.diag = TRUE), direction = "out", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.diag = TRUE), direction = "all", loops = TRUE),
    cent_degree(build_test_nw(.diag = TRUE), direction = "all", loops = TRUE)
  )
  
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .diag = TRUE), 
                direction = "in", loops = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .diag = TRUE), 
                direction = "in", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .diag = TRUE), 
                direction = "out", loops = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .diag = TRUE), 
                direction = "out", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .diag = TRUE), 
                direction = "all", loops = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .diag = TRUE), 
                direction = "all", loops = TRUE)
  )
  
  expect_equal(
    cent_degree(build_test_ig(.bipartite = TRUE), direction = "in", loops = TRUE),
    cent_degree(build_test_nw(.bipartite = TRUE), direction = "in", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.bipartite = TRUE), direction = "out", loops = TRUE),
    cent_degree(build_test_nw(.bipartite = TRUE), direction = "out", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.bipartite = TRUE), direction = "all", loops = TRUE),
    cent_degree(build_test_nw(.bipartite = TRUE), direction = "all", loops = TRUE)
  )
  
  expect_equal(
    cent_degree(build_test_ig(.weighted = TRUE), 
                direction = "in", loops = TRUE, use_edge_weights = TRUE),
    cent_degree(build_test_nw(.weighted = TRUE), 
                direction = "in", loops = TRUE, use_edge_weights = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.weighted = TRUE), direction = "out", loops = TRUE),
    cent_degree(build_test_nw(.weighted = TRUE), direction = "out", loops = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.weighted = TRUE), direction = "all", use_edge_weights = TRUE),
    cent_degree(build_test_nw(.weighted = TRUE), direction = "all", use_edge_weights = TRUE)
  )
  
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .weighted = TRUE), 
                direction = "in", use_edge_weights = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .weighted = TRUE), 
                direction = "in", use_edge_weights = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .weighted = TRUE), 
                direction = "out", use_edge_weights = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .weighted = TRUE), 
                direction = "out", use_edge_weights = TRUE)
  )
  expect_equal(
    cent_degree(build_test_ig(.directed = FALSE, .weighted = TRUE), 
                direction = "all", use_edge_weights = TRUE),
    cent_degree(build_test_nw(.directed = FALSE, .weighted = TRUE), 
                direction = "all", use_edge_weights = TRUE)
  )
})
