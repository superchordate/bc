context( "search" )

test_that( "works as expected", {
  
  expect_equal( nrow( sch( iris, 'setos') ), 50 )
  expect_equal( nrow( sch( iris, 'Setos', ignore.case = FALSE ) ), 0 )
  expect_equal( nrow( sch( iris, '.', do.regex = TRUE ) ), nrow(iris) )
  
})