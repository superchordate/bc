context( "spl" )

test_that("works as expected", {
  
  expect_equal( nrow( spl( cars, n = 12 ) ), 12 )
  expect_equal( length( spl( cars, n = 12 ) ), 2 )
  expect_equal( length( spl( cars, n = 12, return.index = TRUE ) ), 12 )
  expect_equal( length( spl( cars$speed, n = 12, return.index = TRUE ) ), 12 )
  
})