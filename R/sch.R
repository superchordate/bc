#' Search
#'
#' Searches for a value and return rows/values that match.
#'
#' @param x Data frame, tibble, or vector to search.
#' @param search.for String or value to search for.
#' @param search.col Column to search. If NULL, search all columns.
#' @param do.regex Use regular expression to search.
#' @param ignore.case Ignore case when searching. 
#'
#' @return Search results.
#' @export
#'
#' @examples
#' sch( iris, 'seto' )
sch = function( x, search.for, search.col = NULL, do.regex = FALSE, ignore.case = TRUE ){
  
  if( ignore.case && is.character( search.for ) ) search.for = tolower( search.for )
  
  # Handle vector.
  if( is.null( ncol(x) ) ){
    
    # Manually handle ignore case since it won't work with fixed = TRUE>
    if( ignore.case && is.character(x) ) x = tolower(x)
    
    return( grepl( search.for, x, fixed = !do.regex ) )
    
  } 
  
  # Handle data frame.
  
    # Limit to search column if requested.
    if( !is.null( search.col ) ) x = x[[search.col]]
  
    # Handle ignore case by setting characters to lower case.
    if( ignore.case ){
      for( icol in 1:ncol(x) ) if( is.character( x[[icol]] ) ) x[[icol]] = tolower( x[[icol]] )
      rm(icol)
    }
    
    # Find matching rows.
    found.rows = c()
    for( icol in colnames(x) ){
      found.rows = c( found.rows, which( grepl( search.for, x[[icol]], fixed = !do.regex ) ) )
    }
  
    return( x[ unique( found.rows ), ] )
  
}