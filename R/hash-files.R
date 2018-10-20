#' Hash Files and Objects
#' 
#' Digest and hash a list of files. Useful for determining if files have changed during cache management.
#'
#' @param files List of file locations, or a list of objects to hash.
#'
#' @return Hash string.
#' @export
#'
#' @examples
#' 
#' hash.files( list( cars, iris ) )
#' 
hash.files = function( files ){

    ihash = c()
    
    for( ifile in files ){
      
      ifile.digest = digest::digest( ifile )
      ihash = openssl::md5( cc( ihash, ifile.digest ) )
      
    }
    
    return( ihash[[1]] )

}