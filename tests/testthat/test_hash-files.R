context( "hash.files" )

test_that("works as expected", {
  
  expect_equal( hash.files( '../test-files/test-file.csv' ), "ec831a2d15d78ca70fbfad8e807c6ab2" )

  expect_equal( 
    hash.files( c( '../test-files/test-file.csv', '../test-files/test-file.xlsx' ) ),
    "f5dbb9092bdc861f08220feea4b7c742" 
  )

  expect_equal( 
    hash.files( '../test-files' ),
    "702c58a76c671558adc409598fd8a98a" 
  )
  
})