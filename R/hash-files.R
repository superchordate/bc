#' Hash Files
#' 
#' Create hash from a file or list of files. Useful for determining if files have changed during cache management.
#'
#' @param paths List of file or folder locations.
#' @param hash.info Hash the file info ( FALSE hashes the contents ). This is faster but slightly less reliable.
#'
#' @return Hash string.
#' @export
#'
#' @examples
#' #TODO
#' 
hash.files = function( paths, hash.info = TRUE ){

    ihash = NULL
    
    for( path in paths ){
      
      # folder check
      files = list.files( path, recursive = TRUE )
      if( length(files) == 0 ) files = c( path )

      for( file in files ){

        if( hash.info ){
          
          # mtime, ctime, atime
          idigest = digest::digest( file.info(file)[ c( 'size', 'isdir', 'mtime' ) ] )

        } else {
        
          idigest = digest::digest( file = file )

        }
        
        ihash = openssl::md5( c( ihash, idigest ) )

      }
      
    }
    
    return( ihash[[1]] )

}