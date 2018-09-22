#' Easy Concatenate
#'
#' I got tired of typing paste and paste0 so created this function to replace them.
#'
#' @param ... Arguments to be pasted. Eventually passed to paste.
#' @param sep Separator passed to paste.
#'
#' @return Concatenated result.
#' @export
#'
#' @examples
#' cc( c( '1', '2', '3' ) ) 
cc = function( ..., sep = '' ){
  
  arglist = list(...)
  
  if( length( arglist[[1]] ) == 1 ){
    x = paste0( ..., sep = sep )
  } else{
    x = paste( ..., sep = sep )
  }
  
  return(x)
  
}