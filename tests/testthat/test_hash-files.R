context( "hash.files" )

test_that("works as expected", {
  
  expect_equal( hash.files( '../test-files/test-file.csv' ), "3795f4540a7b93f1c76daed5d3611b9d" )
  expect_equal( 
    hash.files( c( '../test-files/test-file.csv', '../test-files/test-file.xlsx' ) ),
    "9e4a179db88c986a55a59dd0156155bd" 
  )
  
})