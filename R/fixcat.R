#' Fix Categories
#' 
#' Groups small categories into a new bucket. Useful for preventing over-fitting on small categories.
#'
#' @param x Data frame or tibble.
#' @param minimum.cat.size Minimum size for a category to not get grouped with other small categories.
#' @param other.name Value name for the group of small categories.
#'
#' @return Data frame with modified columns.
#' @export
#'
#' @examples
#' fixcat(iris)
fixcat = function( x, minimum.cat.size = 15, other.name = 'Other' ){
  
  for( icol in 1:ncol(x) ) if( is.character( x[[icol]] ) || is.factor( x[[icol]] ) ){
    
    vals = table( x[[icol]] )
    
    too.small = names(vals)[ vals < minimum.cat.size ]
    
    if( is.factor( x[[icol]] ) ) levels( x[[icol]] ) = c( levels( x[[icol]] ), other.name )
    
    x[[icol]][ x[[icol]] %in% too.small ] <- 'Other'
    
    if( is.factor( x[[icol]] ) )  x[[icol]] = droplevels( x[[icol]], too.small )
    
  }
  
  return(x)
  
}