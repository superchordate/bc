#' Convert to Date.
#' 
#' Convert a vector to date. By default, returns vector unchanged if new NAs would be created. 
#' You can send non-dates in loops (or otherwise) and they won't be affected. Can be useful for checking if something is a date.
#'
#' @param x Vector to convert.
#' @param do.time NOT YET IMPLEMENTED Allow processing to time values.
#' @param do.excel NOT YET IMPLEMENTED Allow conversion of excel date integers.
#' @param already.clean.vector Sometimes in multi-step data operations clean.vector will have already been called.
#' @param verbose Print useful information via cat.
#' @param do.na What to do in case new NAs are created during conversion. If all NAs, the vector will be returned unchanged. Otherwise, this argument determines behavior: return-unchnaged returns the vector unchanged. warning returns new NAs with a warning. stop throws an error. return-na prints a message with cat which can be turned off with verbose = FALSE.
#'
#' @return Processed vector.
#' @export
#'
#' @examples
#' todate( c( '12/3/14', 'Hello'), do.na = 'return-na' )
#' todate( c( '12/3/14', 'Hello') )
todate = function( 
  x,
  do.time = FALSE,
  do.excel = FALSE,
  na.vals = bc::na.strings,
  already.clean.vector = FALSE,
  verbose = TRUE,
  do.na = c( 'return-unchanged', 'warning', 'stop', 'return-na' )
){
  
  if( lubridate::is.Date(x) || lubridate::is.POSIXt(x) ) return(x)
  
  if( !is.null( ncol(x) ) ) stop( 'todate only accepts vector arguments. Error E807 todate.' )
  do.na  = match.arg(do.na)
  
  orig.x = x
  
  if( ! already.clean.vector ) x = clean.vector( x, verbose = verbose, na.vals = na.vals )
  
  if( do.time ){
    stop( 'Time processing not set up yet.' )
  }
  
  # Attempt date conversion.
  
    try.formats = c( 'mdy', 'ymd', 'dmy' )
    convert.attempt = suppressWarnings( as.Date( lubridate::parse_date_time( x, try.formats ) ) )
    
    # If no new NAs were created, return the data.
    if( ! any( is.na(convert.attempt) & ! is.na(x) ) ) return(x)
    
  # TODO: attempt excel date conversion.
    
    convert.attempt.xl = as.Date( rep( NA, length(x) ) )
    
  # Combine convert attempts.
    
    x = dplyr::coalesce( convert.attempt, convert.attempt.xl )
    rm( convert.attempt, convert.attempt.xl )
    
  # If convert attempts are all NA, return original x.
  if( all( is.na( x ) ) ) return( orig.x )
    
  # Otherwise, some dates weren't converted. Choose what to do.
    
    not.converted = which( !is.na(orig.x) & is.na(x) )
    num.not.converted = length(not.converted)
    not.converted = unique( orig.x[ not.converted ] )
    
    if( do.na == 'return-unchanged' ){
      
      if( verbose) cat( 
        'todate:: Returned unchanged vector due to [', num.not.converted, '] invalid date values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(orig.x)
      
    } else if( do.na == 'warning' ){
      
      warning( 
        'todate:: Returned vector with [', num.not.converted, '] new NAs due to invalid date values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(x)
      
    } else if( do.na == 'stop' ){
      
      stop( 
        'todate:: [', num.not.converted, '] new NAs due to invalid date values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      
    } else if( do.na == 'return-na' ){
      
      if( verbose) cat( 
        'todate:: Returned vector with [', num.not.converted, '] new NAs due to invalid date values including [', 
        cc( head( not.converted, 5 ), sep = ', ' ), ']. \n'
      )
      return(x)
      
    } else { 
    
      stop( 'Unhandled new NAs. Error E845 todate' )
        
    }
  
}