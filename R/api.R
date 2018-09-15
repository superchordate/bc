#' Get data from API.
#'
#' @param url The URL of the API. You can set this up hou you like but it usually starts with htt and ends with .com or similar.
#' @param path The second part of the URL which varies by the service you are using.
#' @param params Named list/vector of key value pairs representing the arguments.
#' @param do.cache Save a cache of the output to be used instead of the API in case of an error, etc.
#' @param cache.path Path to save a cache to. Leave this blank and just use cache name if you wnat.
#' @param cache.name File name to use for the cache. Cache will always be a .RDS so you don't need to include the path.
#' @param use.cache.same.day Use the cache instead of calling the API again if the cache is the same day. Useful when you have limited API calls to an API.
#' @param verbose Print helpful messages about the API.
#'
#' @return List of data from the API.
#' @export
#'
#' @examples
#' #TODO
api = function(
  url, path = NULL, parms = NULL,
  do.cache = TRUE, cache.path = NULL, cache.name = 'api-cache', use.cache.same.day = TRUE,
  verbose = TRUE
){

  # Same-day cache.

    cache.name = gsub( '[.]RDS$', '', cache.name, ignore.case = TRUE )
    if( !is.null( cache.path ) ) cache.name = paste( cache.path, cache.name, sep = '/' )
    cache.name = paste0( cache.name, '.RDS' )

    if( use.cache.same.day ){

      icache = tryCatch({ readRDS( cache.name ) }, error = function(e){}, warning = function(w){} )

      if( !is.null( icache ) ) if( icache$date == Sys.Date() ){
        if( verbose ) cat( 'Same-day cache found. Returning it instead. \n' )
        return( icache )
      }

    }

  # Build our call URL.

    url = paste( url, path, sep = '/' )

    if( !is.null( parms ) ){

      pnames = names(parms)
      parms = as.character(parms)

      url = paste0( url, '?')

      for( i in 1:length(parms) ) url = paste0( url, pnames[i], '=', parms[i], '&' )

      url = gsub( '&$', '', url )

    }

  # Call the API. If there was an error try to use the cache.

    idt = tryCatch({ tryApi( url, verbose ) }, error = function(e){

      icache = tryCatch({ readRDS( cache.name ) }, function(e2){}, warning = function(w){} )

      if( !is.null( icache ) ){
        warning( 'Using cache due to error. Error was: \n', e )
        return( icache )
      }

      stop( e )

    } )

  # Prepare return list. Save cache.

    idt = list( date = Sys.Date(), data = idt )

    if( do.cache ) saveRDS( idt, file = cache.name )

  # Return data.

    return(idt)

}

tryApi = function( url, verbose ){

  url = URLencode( url )

  if( verbose ) cat( 'Final URL is: ', url, '\n' )

  raw.response = rawToChar( httr::GET( url )$content )

  tryCatch({
    idt = jsonlite::fromJSON( raw.response )
  }, error = function(e){})

  if( !exists('idt' ) ){
    stop( 'bc::api - Response could not be parsed. Response was: \n', dplyr::str_trunc( raw.response, width = 500 ) )
  }
  
  # Check common errors.
  if( !is.null( idt[['error']] ) ){
    if( !is.null( idt[['error']][['message']] ) ) stop( 'API Error message: ', idt[['error']][['message']]  )
    stop( 'API error: \n', idt[['error']] )
  }
  
  return( idt )

}
