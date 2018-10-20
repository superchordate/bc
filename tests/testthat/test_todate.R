context( "todate" )

test_that( "works as expected", {
  
  expect_equal(
    todate( c( '12/3/14', 'Hello'), do.na = 'return-na', verbose = FALSE ),
    c( lubridate::mdy( '12/3/14'), NA )
  )
  
  
  expect_equal(
    todate( c( '12/3/14', 'Hello'), verbose = FALSE ),
    c( '12/3/14', 'Hello')
  )
  
})