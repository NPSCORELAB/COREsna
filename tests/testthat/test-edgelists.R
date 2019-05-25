context("test-edgelists")

# .fetch_edgelist() ====================================================================
test_that("`build_test_el()`` works", {
  expect_true(
    run_edgelist_tests()
  )
  
  
})
