context("tonum")

test_that("works as expected", {
  
  expect_equal(
    tonum( c( '1st', '2nd', '3rd' ) ),
    c( 1, 2, 3 )
  )
  
})