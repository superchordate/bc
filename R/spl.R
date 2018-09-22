#' Sample data frame or vector. 
#'
#' Uniform random sample from the input.
#'
#' @param x Vector to be covnerted to numeric.
#' @param n Strings to consider NA.
#' @param return.index Return just the sample index, not the actual data.
#'
#' @return Sampled data.
#' @export
#'
#' @examples
#' spl( cars )
#' spl( cars$speed )
spl = function( x, n = 10, return.index = FALSE ){
  
  nrows = nrow( x )
  len = length(x)
  
  this.n = if( is.null(nrows) ){ len } else { nrows }
  
  if( n >= this.n ) return(x)
  
  this.sample = base::sample( x = 1:this.n, size = n )
  
  if( return.index ) return( this.sample )
  
  return( 
    if( is.null(nrows) ){ x[ this.sample ] } else { x[ this.sample, ] }
  )
  
}