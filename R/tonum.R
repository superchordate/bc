#' Convert to number.
#'
#' My flexible number conversion function.
#'
#' @param x Vector to be covnerted to numeric.
#' @param already.clean.vector Sometimes in multi-step data operations clean.vector will have already been called.#' 
#' @param na.vals Values to consider NA.
#' @param na.zero Set NA values to 0.
#' @param do.remove.chars Remove characters that aren't numeric or numeric-related to aggressively extract numbers from strings.
#' @param do.na What to do if new NA values are crated during the conversion.
#' @param verbose Print messages (via cat) about what the function is doing.
#'
#' @return Result vector.
#' @export
#'
#' @examples
#' tonum( c( '1st', '2nd', '3rd' ) )
tonum = function( 
  x, 
  already.clean.vector = FALSE,
  na.vals = bc::na.strings, 
  na.zero = FALSE, 
  do.remove.chars = FALSE,
  do.na = c( 'return-unchanged', 'warning', 'stop', 'return-na' ),
  verbose = TRUE
){
  
  if( is.numeric( x ) ) return(x)
  
  if( !is.null( ncol(x) ) ) stop( 'tonum only accepts vector arguments. Error E900 tonum' )
  do.na  = match.arg(do.na)
  
  orig.x = x
  
  clean.x = as.character(x)
  rm(x)
  
  if( ! already.clean.vector ) clean.x = clean.vector( clean.x, verbose = verbose, na.vals = na.vals )
  
  # Remove common issues with numbers.
  
    clean.x = gsub( '([0-9])(st|rd|nd|th)\\b', '\\1', clean.x )
    clean.x = gsub( '[(](.+)[)]', '-\\1', clean.x )
    clean.x = gsub( '--', '+', clean.x )
    clean.x = gsub( '+([0-9])', '\\1', clean.x )
    
  if( do.remove.chars ) clean.x = gsub( '[^0-9+.-]', '', clean.x )
  
  # Attempt conversion.
    
    attempt = suppressWarnings({ as.numeric( clean.x ) })
    
  # If no new NAs, return immediately.
    
    new.nas = which( !is.na(orig.x) & is.na(attempt) )
    if( length( new.nas ) == 0 ) return(attempt)
  
  # If convert attempts are all NA, we assume the vector is not numeric. Return original x.
  
    if( all( is.na(attempt) ) ) return( orig.x )
    
  # Otherwise, some dates weren't converted. Choose what to do.
    
    num.not.converted = length(new.nas)
    not.converted = unique( orig.x[ new.nas ] )
    
    if( do.na == 'return-unchanged' ){
      
      if( verbose) cat( 
        'tonum:: Returned unchanged vector due to [', num.not.converted, '] invalid values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(orig.x)
      
    } else if( do.na == 'warning' ){
      
      warning( 
        'tonum:: Returned vector with [', num.not.converted, '] new NAs due to invalid values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(attempt)
      
    } else if( do.na == 'stop' ){
      
      stop( 
        'tonum:: [', num.not.converted, '] new NAs due to invalid values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      
    } else if( do.na == 'return-na' ){
      
      if( verbose) cat( 
        'tonum:: Returned vector with [', num.not.converted, '] new NAs due to invalid values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(attempt)
      
    } else { 
      
      stop( 'Unhandled new NAs. Error E909 tonum' )
      
    }
}