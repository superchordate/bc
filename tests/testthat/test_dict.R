context( "dict" )

test_that("works as expected", {
  
  expect_equal( nrow(dict(cars)), ncol(cars) )
  
})