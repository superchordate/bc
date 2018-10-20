context( "read.any" )

test_that( "works as expected", {
  
  test.folder = ifelse( 
    is_testing(),
    '../test-files',
    'tests/test-files'
  )
  
  check.test.file = function(idt){
      
    expect_equal(
      lubridate::as_datetime( idt$`Date Column` ),
      rep( lubridate::parse_date_time( c( "2018-10-20 0:00:00" ), 'ymd HMS' ), nrow(idt) )
    )
    
    expect_equal(
      idt$`Numeric Column`,
      c( 1.05,   0.30,  -0.30, -45.00 )
    )
    
    expect_equal(
      idt$`Character Column`,
      factor( c( "Row 1", "Row 2", "Row 3", "Row 4" ) )
    )
    
    expect_equal(
      idt$`Datetime Column`,
      rep( lubridate::parse_date_time( "10/20/2018 12:20:00", "mdy IMS" ), nrow(idt) )
    )
    
  }
  
  check.test.file( read.any( 'test-file.xlsx', folder = test.folder, do.time = TRUE ) )
  check.test.file( read.any( 'test-file.csv', folder = test.folder, do.time = TRUE ) )
  
  
})