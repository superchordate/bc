#' Convert to number.
#'
#' My flexible number conversion function.
#'
#' @param x Vector to be covnerted to numeric.
#' @param na.strings Strings to consider NA.
#' @param na.zero Set NA values to 0.
#' @param if.na What to do if new NA values are crated during the conversion.
#' @param verbose Print messages (via cat) about what the function is doing.
#'
#' @return Result vector.
#' @export
#'
#' @examples
#' tonum( c( '1st', '2nd', '3rd' ) )
tonum = function( x, na.strings = bc::na.strings, na.zero = FALSE, if.na = 'return-unchanged', verbose = TRUE ){
  
  # If x is alraedy numeric, return it.
  if( is.numeric( x ) ) return(x)
  
  if( if.na %ni% c(
    'return-unchanged', 'warning', 'error', 'verbose'
  ) ) stop( 'bc::tonum : Invalid argument for [ tonum(if.na) ].')
  
  x.in = x
  
  x = as.character(x)
  
  x[ x %in% na.strings ] <- NA
  
  x = gsub( '([0-9])(st|rd|nd|th)\\b', '\\1', x )
  x = gsub( '[(](.+)[)]', '-\\1', x )
  x = gsub( '--', '+', x )
  x = gsub( '+([0-9])', '\\1', x )
  
  attempt = suppressWarnings({ as.numeric( x ) })
  
  if( any( is.na(attempt) & !is.na(x) ) ) if( if.na == 'return-unchanged' ){
    
    cat( 'bc::tonum : NAs were created. Returning vector unchanged. \n' )
    return( x.in )
    
  } else if( if.na == 'warning' ){
    warning( 'bc::tonum : NAs were created. Returning vector with NAs.' )
    return( attempt )
    
  } else if( if.na == 'error' ){
    stop( 'bc::tonum : NAs were created.' )
    
  } else if( if.na == 'verbose' ){
    if( verbose ) cat( 'bc::tonum : NAs were created. Returning vector with NAs. \n' )
    return( attempt )
    
  } else {
    stop( 'bc::tonum : No appropriate if.na ')
  }
  
  return( attempt )
  
}