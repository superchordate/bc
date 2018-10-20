context( "read.any" )

test_that( "works as expected", {
  
  test.folder = '../test-files'
  
    check.test.file = function(idt){
        
      expect_equal(
        idt$`Date Column`,
        lubridate::ymd( c( "2018-10-20", "2018-10-20", "2018-10-20", "2018-10-20" ) )
      )
      
      expect_equal(
        idt$`Numeric Column`,
        c( 1.05,   0.30,  -0.30, -45.00 )
      )
      
      expect_equal(
        idt$`Character Column`,
        c( "Row 1", "Row 2", "Row 3", "Row 4" )
      )
      
  }
  
  check.test.file( read.any( 'test-file.xlsx', folder = test.folder ) )
  check.test.file( read.any( 'test-file.csv', folder = test.folder ) )
    
  
})