test_that("node functions work", {
  expect_true(
    run_node_attr_tests()
  )
  
  expect_equal(
    node_get_names(build_test_ig()),
    node_get_names(build_test_nw())
  )
})
