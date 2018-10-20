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
  
  expect_equal(
    todate( c( "6/12/2015 20:45:00",  "4/4/2015 22:20:00",   "4/11/2016 23:24:00",  "9/29/2016 1:10:00",  "11/6/2016 2:46:00"), verbose = FALSE, do.time = TRUE ),
    lubridate::parse_date_time( c( "6/12/2015 20:45:00",  "4/4/2015 22:20:00",   "4/11/2016 23:24:00",  "9/29/2016 1:10:00",  "11/6/2016 2:46:00"), 'mdy HMS' )
  )
  
  expect_equal(
    todate( 43393.5138888889, do.time = TRUE ),
    lubridate::parse_date_time( "2018-10-20 12:20:00", 'ymd IMS', tz = "UTC" )
  )
  
})