#' Convert Categorical Variables to Booleans.
#'
#' 
#'
#' @param x Dataframe or tibble.
#' @param match.to Data frame to match the values to. This will ensure consistent columns for modeling.
#' @param ignore.cols Columns to ignore.
#'
#' @return Modified data.
#' @export
#'
#' @examples
#' cat2bool( iris )
cat2bool = function( x, match.to = NULL, make.names = TRUE, ignore.cols = c() ){
  
  for( icol in setdiff( colnames(x), ignore.cols ) ) if( is.character( x[[icol]] ) || is.factor( x[[icol]] ) ){
    
    # Convert to character then factor to ensure no extra levels.
    x[[icol]] = as.character( x[[icol]] )
    x[[icol]] = as.factor( x[[icol]] )
    
    # Get unique values sorted by occurence. 
    # The least occured should be the 'other' that doesn't get its own columns.
    ivals = table( x[[icol]] ) %>% sort( decreasing = TRUE )
    
    for( ival in names( ivals )[ -length(ivals) ] ) x[[ cc( icol, '=', ival ) ]] = x[[icol]] == ival
    
    x = x[ , setdiff( colnames(x), icol ) ]
    
    if( make.names ){
      this.names = which( grepl( icol, colnames(x), fixed = TRUE ) )
      colnames(x)[ this.names ] = make.names( colnames(x)[ this.names ] )
    }
    
  }
  
  return(x)
    
}