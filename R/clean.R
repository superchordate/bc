#' Clean Data
#'
#' My standard methods for cleaning a dataset.
#'
#' @param x Data frame to clean.
#' @param header.in.data
#' @param header.on.row
#'
#' @return Clean tibble.
#' @export
#'
#' @examples
#' #TODO
clean = function( x, header.in.data = TRUE, header.on.row = NULL ){

  x = as_tibble(x)
  
  if( header.in.data && is.null( header.on.row ) ) header.on.row = 1 
  
  iheaders = if( !header.in.data ){ colnames( x ) } else { x[ header.on.row, ] }
  iheaders = trimws( iheaders )
  
  if( header.in.data ) x = x[ (header.in.data+1):nrow(x), ]
  
  na.headers = which( is.na( iheaders ) | iheaders %in% bc::nastrings )

  iheaders[ na.headers ] = paste0( rep( 'NA', length(na.headers) ), 1:length(na.headers) )
  
  colnames(x) = iheaders
  
  return( x )
  
}
