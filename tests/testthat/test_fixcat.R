context( "fixcat" )

test_that("works as expected", {
  
  x = dplyr::tibble(
    cat = factor( c(
      'too.small2', 'too.small1', 'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough',
      'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough',
       'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough', 'large.enough'
    ))
  )
  
  expect_equal( sum( fixcat(x)$cat == "Other" ), 2 )
  expect_equal( length( unique( fixcat(x)$cat ) ), 2 )
  
})