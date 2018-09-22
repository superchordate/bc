#' Run the files in a folder.
#'
#' Helpful for looping through scripts.
#'
#' @param path Path to your files.
#' @param recursive Recursively runs child folders. See ?list.files.
#' 
#' @export
#'
#' @examples
#' # TODO
runfolder = function( path, recursive = TRUE ){
  
  for( i in list.files( path, full.names = TRUE, pattern = '[.][Rr]$', recursive = recursive ) ){
    
    cat( '\t running [', gsub( '^.*/', '', i ), ']. \n')
    
    tryCatch({
      eval.parent( source( i ) )
    }, error = function(e){
      file.edit(i)
      stop( 'Error at file [', i,' ]: \n \t ', e, '' )
    }
    )
    
    rm(i)
  }
  
}