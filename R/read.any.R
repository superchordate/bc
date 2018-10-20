#' Read Any
#' 
#' Flexible read function which also handles conversion to data types, and common data cleaning tasks.
#'
#' @param path Path to file within a folder, or full path.
#' @param folder Folder to look for path in, if applicable.
#' @param sheet If an excel file, the sheet to load.
#'
#' @return Tibble.
#' 
#' @export
#'
#' @examples
#' # TODO
read.any = function( 
  path,
  folder = NULL,
  sheet = 1
  #na.strings = bc::na.strings,
  #skip.rows = 0
){
  
  # Complete path.
  
    if( !is.null(folder) ) path = stringr::str_trim( paste0( c( folder, path ), collapse = '/' ) )
    rm(folder)
    
    if( !file.exists( path ) ) stop( 
      'File not found at [', path, ']. \n ',
      'Working directory is [', getwd(), ']. \n',
      'Error E958 read.any' 
    )
  
  # Handle file types.
    
    idt = NULL
  
    type = tolower( stringr::str_extract( path, '[^.]+$') )
  
    # Excel
    
      if( grepl( 'xls[xmb]?$', type ) ){
        
        err = NULL
        idt = tryCatch(
          { 
            openxlsx::read.xlsx( 
              path, sheet = sheet, detectDates = TRUE, check.names = FALSE, skipEmptyRows = TRUE
            ) 
          }, error = function(e){ } 
        )
        
        if( is.null(idt) ) stop( 'Error reading Excel file [', path, ']. Error E938 read.any' )
        
        colnames(idt) = gsub( '[.]', ' ', colnames(idt) )
        
      }
    
    # Delimited.
    
    if( type %in% c( 'tsv', 'csv' ) ) idt = data.table::fread( 
      path,
      na.strings = na.strings, stringsAsFactors = FALSE, verbose = FALSE,
      check.names = FALSE, blank.lines.skip = TRUE
    )
    
  # Apply tonum, todate to characters.
    
    for( i in 1:ncol(idt) ) if( is.character( idt[[i]] ) ){
      
      idt[[i]] = todate( idt[[i]], verbose = FALSE )
      idt[[i]] = tonum( idt[[i]], verbose = FALSE )
      
    }
    
  # Return the data as a tibble.
    
    return( dplyr::ungroup( dplyr::as_tibble( idt ) ) )
  
}
