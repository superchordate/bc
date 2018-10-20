context( "hash.files" )

test_that("works as expected", {
  
  expect_equal( hash.files( list( cars, iris ) ), "65e0d6327fd4ed685b35e84fc74be9c8" )
  expect_equal( hash.files( cars ), "b0d04f96a36931d7b7a74441a9879c30" )
  
})